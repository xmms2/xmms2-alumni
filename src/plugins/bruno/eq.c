/** @file eq.c
 *  Equalizer effect plugin
 *
 *  Copyright (C) 2006-2009 XMMS2 Team
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 */

#include "xmms/xmms_xformplugin.h"
#include "xmms/xmms_config.h"
#include "xmms/xmms_log.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


#define DELAY_TIME 2

typedef struct {

    short *buff;
    gint framerate;
    gint channels;
    gint read_pointer;
    gint write_pointer;
    gint time;
    gboolean eos;
    gfloat volume;
    gint testDataTime;

} xmms_fade_buffer_t;

typedef struct {

    gint8 read_buffer,write_buffer;
    xmms_fade_buffer_t *buffer[2];

    gboolean read_both_buffers;
    gboolean first;

} xmms_bruno_data_t;

static gboolean xmms_eq2_plugin_setup (xmms_xform_plugin_t *xform_plugin);
static gboolean xmms_eq2_init (xmms_xform_t *xform);
static void xmms_eq2_destroy (xmms_xform_t *xform);
static gint xmms_eq2_read (xmms_xform_t *xform, xmms_sample_t *buf, gint len,
                          xmms_error_t *error);
static gint64 xmms_eq2_seek (xmms_xform_t *xform, gint64 offset,
                            xmms_xform_seek_mode_t whence, xmms_error_t *err);
static void xmms_eq2_gain_changed (xmms_object_t *object, xmmsv_t *_data,
                                  gpointer userdata);
static void xmms_eq2_config_changed (xmms_object_t *object, xmmsv_t *data, gpointer userdata);
static gfloat xmms_eq2_gain_scale (gfloat gain, gboolean preamp);

XMMS_XFORM_PLUGIN ("bruno",
                   "Bruno effect",
                   XMMS_VERSION,
                   "Bruno effect",
        	xmms_eq2_plugin_setup);

static void
xmms_bruno_flush_buffer (xmms_fade_buffer_t *buffer)
{
   gint i,size;
   size = 44100 * 2 * buffer->time;
   for(i=0;i<size;++i)
   {
       buffer->buff[i] = 0;
   }
}

static gboolean
xmms_eq2_plugin_setup (xmms_xform_plugin_t *xform_plugin)
{
	xmms_xform_methods_t methods;

	XMMS_XFORM_METHODS_INIT (methods);

	methods.init = xmms_eq2_init;
	methods.destroy = xmms_eq2_destroy;
	methods.read = xmms_eq2_read;
	methods.seek = xmms_eq2_seek;

	xmms_xform_plugin_methods_set (xform_plugin, &methods);

//	xmms_xform_plugin_config_property_register (xform_plugin, "bands", "15",
//	                                            NULL, NULL);
	xmms_xform_plugin_indata_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              48000,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_plugin_indata_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              44100,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_plugin_indata_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              22050,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_plugin_indata_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              11025,
	                              XMMS_STREAM_TYPE_END);

	return TRUE;
}

static void 
xmms_eq2_reinit (xmms_fade_buffer_t *data, gint framerate, gint channels, gint time) {

    //data->read_pointer = 0;
    //data->write_pointer = 0;
    data->framerate = framerate;
    data->channels = channels;
    data->time = time*(data->framerate * data->channels)/2048;
    data->testDataTime = 0;

}


static gboolean
xmms_eq2_init (xmms_xform_t *xform)
{
    gint i,j;
	xmms_config_property_t *config;

	g_return_val_if_fail (xform, FALSE);

	xmms_bruno_data_t *priv;
	gint framerate, channels;
	priv = g_new0 ( xmms_bruno_data_t, 1);
	xmms_xform_private_data_set (xform, priv);
	framerate = xmms_xform_indata_get_int(xform,XMMS_STREAM_TYPE_FMT_SAMPLERATE);
	channels = xmms_xform_indata_get_int (xform, XMMS_STREAM_TYPE_FMT_CHANNELS);

	for(i=0; i<2;++i) {
	        priv->buffer[i] = g_malloc(sizeof(xmms_fade_buffer_t));
		xmms_eq2_reinit(priv->buffer[i],framerate,channels,DELAY_TIME);
		priv->buffer[i]->eos=FALSE;
		priv->buffer[i]->volume=1;
    		priv->buffer[i]->buff = g_malloc(44100 * 2 * sizeof (short) * priv->buffer[i]->time);
    		for(j=0; j < priv->buffer[i]->framerate * priv->buffer[i]->channels*2; j++){
	    		priv->buffer[i]->buff[j] = 0;
    		}		
		priv->buffer[i]->read_pointer=0;
		priv->buffer[i]->write_pointer=0;
	}

    priv->read_buffer = 0;
    priv->write_buffer = 0;
    priv->read_both_buffers = FALSE;
    priv->first = TRUE;

	xmms_xform_outdata_type_copy (xform);

	return TRUE;
}

static void
xmms_eq2_destroy (xmms_xform_t *xform)
{
    xmms_bruno_data_t *data;
    gint i;
    g_return_if_fail (xform);
    data = xmms_xform_private_data_get (xform);
    g_return_if_fail (data);
    for(i=0; i<2; ++i) {
	g_free (data->buffer[i]->buff);
	g_free (data->buffer[i]);
    }
    g_free (data);
}

static gint
xmms_eq2_read (xmms_xform_t *xform, xmms_sample_t *buf, gint len,
              xmms_error_t *error)
{
	gint read, chan,i,j, srate;
	xmms_bruno_data_t *data;
	xmms_fade_buffer_t *read_buffer, *write_buffer;
	gint read_pointer, write_pointer, read_pointer_2;
	g_return_val_if_fail (xform, -1);

	data = xmms_xform_private_data_get (xform);
	g_return_val_if_fail (data, -1);


	xmms_xform_t *chain;
	for(chain = xform; chain != NULL; chain = xmms_xform_prev_get(chain)) {
	    if(xmms_xform_eos_get(chain)) {
		data->buffer[data->write_buffer]->eos = TRUE;
		data->buffer[data->write_buffer]->read_pointer = 0;
		data->write_buffer = (data->write_buffer+1)%2;
		data->buffer[data->write_buffer]->write_pointer = data->buffer[data->write_buffer]->time/2 -1;
		xmms_log_info("END OF SONG!");
		break;
	    }
	}
	read_buffer = data->buffer[data->read_buffer];
	write_buffer = data->buffer[data->write_buffer];

	if(read_buffer->eos) {
	    read_buffer->testDataTime++;
	    if(!data->read_both_buffers) {
	    	data->read_both_buffers = TRUE;
		xmms_log_info("Read both buffers!");
	    }
//	    read_buffer->volume -= 1/(gfloat)read_buffer->time;
	    if(read_buffer->testDataTime >= read_buffer->time/2-1)
	    {
		read_buffer->volume = 1;
		xmms_bruno_flush_buffer(read_buffer);
//		read_buffer->write_pointer = 0;
		xmms_log_info("END OF PLAY : %i",read_buffer->testDataTime);
		read_buffer->eos = FALSE;
		read_buffer->testDataTime=0;
		data->read_buffer = (data->read_buffer+1)%2;
		data->read_both_buffers = FALSE;
		read_buffer = data->buffer[data->read_buffer];
		read_buffer->read_pointer = 0;
	    }
	}

    


	read = xmms_xform_read (xform, buf, len, error);
	chan = xmms_xform_indata_get_int (xform, XMMS_STREAM_TYPE_FMT_CHANNELS);
	srate = xmms_xform_indata_get_int(xform,XMMS_STREAM_TYPE_FMT_SAMPLERATE);
/*	if(srate != write_buffer->framerate || chan != write_buffer->channels) {
	    // the frame rate or the chan have changed, we need to reallocate the buffer
	    xmms_eq2_reinit(write_buffer,srate,chan,DELAY_TIME);
	}*/
	short *out = buf;

	if(data->first)
	{
	    write_buffer->write_pointer = write_buffer->time/2-1;
	    xmms_log_info("FIRST PTR : %i",write_buffer->write_pointer);
	    data->first = FALSE;
	}

	read_pointer =    read_buffer->read_pointer * read_buffer->framerate * read_buffer->channels;
	read_pointer_2 = write_buffer->read_pointer * write_buffer->framerate * write_buffer->channels;
	write_pointer =  write_buffer->write_pointer * write_buffer->framerate * write_buffer->channels;
	if(data->first)
	{
		if(read_pointer_2 == 0) {
		    write_pointer = (write_buffer->time-1) * write_buffer->framerate * write_buffer->channels;
		} else {
		    write_pointer = read_pointer_2 - write_buffer->framerate * write_buffer->channels;
		}
		data->first = FALSE;
	}



	for (i = 0; i < len/2; i+=chan)
	{
	    for(j=0;j<chan;++j)
	    {
		write_buffer->buff[i+j+write_pointer] = out[i+j];
		out[i+j] = read_buffer->buff[i+j+read_pointer]*read_buffer->volume;
		read_buffer->buff[i+j+read_pointer] = 0;

		if(data->read_both_buffers) {
		    out[i+j] += write_buffer->buff[i+j+read_pointer_2]*write_buffer->volume;
		    write_buffer->buff[i+j+read_pointer_2] = 0;
		}
		
	    }
	}


	read_buffer->read_pointer = (read_buffer->read_pointer + 1 ) % read_buffer->time;
	write_buffer->write_pointer = (write_buffer->write_pointer + 1 ) % write_buffer->time;
	if(data->read_both_buffers) {
	    write_buffer->read_pointer = (write_buffer->read_pointer + 1 ) % write_buffer->time;
	}
	return read;
}

static gint64
xmms_eq2_seek (xmms_xform_t *xform, gint64 offset, xmms_xform_seek_mode_t whence, xmms_error_t *err)
{
	return xmms_xform_seek (xform, offset, whence, err);
}
