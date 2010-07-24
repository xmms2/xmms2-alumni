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

#include "../includepriv/xmmspriv/xmms_ringbuf.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


#define DELAY_TIME 4
#define CHUNK_SIZE 4096


typedef struct {

    gint framerate;
    gint channels;
    gboolean eos;
    gfloat volume;
    gint testDataTime;

    xmms_ringbuf_t *buf;
    gint buffered;
    gint requested;


} xmms_fade_buffer_t;

typedef struct {

    gint8 read_buffer,write_buffer;
    xmms_fade_buffer_t *buffer[2];

    gboolean read_both_buffers;
    gboolean first;



} xmms_bruno_data_t;

static gboolean xmms_crossfade_plugin_setup (xmms_xform_plugin_t *xform_plugin);
static gboolean xmms_crossfade_init (xmms_xform_t *xform);
static void xmms_crossfade_destroy (xmms_xform_t *xform);
static gint xmms_crossfade_read (xmms_xform_t *xform, xmms_sample_t *buf, gint len,
	xmms_error_t *error);
static gint64 xmms_crossfade_seek (xmms_xform_t *xform, gint64 offset,
	xmms_xform_seek_mode_t whence, xmms_error_t *err);

XMMS_XFORM_PLUGIN ("bruno",
	"Bruno effect",
	XMMS_VERSION,
	"Bruno effect",
	xmms_crossfade_plugin_setup);


    static gboolean
xmms_crossfade_plugin_setup (xmms_xform_plugin_t *xform_plugin)
{
    xmms_xform_methods_t methods;

    XMMS_XFORM_METHODS_INIT (methods);

    methods.init = xmms_crossfade_init;
    methods.destroy = xmms_crossfade_destroy;
    methods.read = xmms_crossfade_read;
    methods.seek = xmms_crossfade_seek;

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



    static gboolean
xmms_crossfade_init (xmms_xform_t *xform)
{
    gint i;

    g_return_val_if_fail (xform, FALSE);

    xmms_bruno_data_t *priv;
    gint framerate, channels;
    priv = g_new0 ( xmms_bruno_data_t, 1);
    xmms_xform_private_data_set (xform, priv);
    framerate = xmms_xform_indata_get_int(xform,XMMS_STREAM_TYPE_FMT_SAMPLERATE);
    channels = xmms_xform_indata_get_int (xform, XMMS_STREAM_TYPE_FMT_CHANNELS);

    for(i=0; i<2;++i) {
	priv->buffer[i] = g_malloc(sizeof(xmms_fade_buffer_t));
	priv->buffer[i]->eos=FALSE;
	priv->buffer[i]->framerate = framerate;
	priv->buffer[i]->channels = channels;
	priv->buffer[i]->testDataTime = 0;
	priv->buffer[i]->volume=1;
	priv->buffer[i]->requested = DELAY_TIME*framerate*channels*2;
	priv->buffer[i]->buf = xmms_ringbuf_new(priv->buffer[i]->requested);
	priv->buffer[i]->buffered = 0;


    }


    priv->read_buffer = 0;
    priv->write_buffer = 0;
    priv->read_both_buffers = FALSE;
    priv->first = TRUE;

    xmms_xform_outdata_type_copy (xform);

    return TRUE;
}

    static void
xmms_crossfade_destroy (xmms_xform_t *xform)
{
    xmms_bruno_data_t *data;
    gint i;
    g_return_if_fail (xform);
    data = xmms_xform_private_data_get (xform);
    g_return_if_fail (data);
    for(i=0; i<2; ++i) {
	xmms_ringbuf_destroy(data->buffer[i]->buf);
	g_free (data->buffer[i]);
    }
    g_free (data);
}



    static gint
xmms_crossfade_read (xmms_xform_t *xform, xmms_sample_t *buf, gint len,
	xmms_error_t *error)
{

    gint read, chan,i,j, srate;
    xmms_bruno_data_t *data;
    xmms_fade_buffer_t *read_buffer, *write_buffer;
    g_return_val_if_fail (xform, -1);

    data = xmms_xform_private_data_get (xform);
    g_return_val_if_fail (data, -1);

    read_buffer = data->buffer[data->read_buffer];
    write_buffer = data->buffer[data->write_buffer];

    /* ensure buffer is filled */
    while (write_buffer->buffered < write_buffer->requested + CHUNK_SIZE) {
	char t[CHUNK_SIZE];
	int l = MIN( CHUNK_SIZE,  write_buffer->requested + CHUNK_SIZE - write_buffer->buffered);
	int r;
	r = xmms_xform_read (xform, t, l, error);
	if (r <= 0) {
	    write_buffer->eos = TRUE;
	    data->write_buffer = (data->write_buffer+1)%2;
	    //xmms_log_info("END OF SONG!");
	    return r;
	}
	xmms_ringbuf_write (write_buffer->buf, t, r);
	write_buffer->buffered += r;
    }


    if(read_buffer->eos) {
	read_buffer->testDataTime++;
	if(!data->read_both_buffers) {
	    data->read_both_buffers = TRUE;
	    //xmms_log_info("Read both buffers!");
	}
	read_buffer->volume -= 1/(gfloat)(read_buffer->requested/(gfloat)CHUNK_SIZE);
	if(read_buffer->testDataTime >= read_buffer->requested/CHUNK_SIZE)
	{
	    read_buffer->volume = 1;
	    //xmms_log_info("END OF PLAY : %i",read_buffer->testDataTime);
	    read_buffer->eos = FALSE;
	    read_buffer->testDataTime=0;
	    data->read_buffer = (data->read_buffer+1)%2;
	    data->read_both_buffers = FALSE;
	    read_buffer = data->buffer[data->read_buffer];
	}
    }

    read = xmms_ringbuf_read (read_buffer->buf, buf, len);
    read_buffer->buffered -= read;

    if(data->read_both_buffers)
    {

	gfloat volume = read_buffer->volume;
	read_buffer = data->buffer[ (data->read_buffer+1)%2 ];
	short *buf2 = g_malloc(read_buffer->requested);
	read = xmms_ringbuf_read (read_buffer->buf, buf2, len);
	read_buffer->buffered -= read;

	short *out = buf;
//	chan = xmms_xform_indata_get_int (xform, XMMS_STREAM_TYPE_FMT_CHANNELS);
	chan = read_buffer->channels;

	for (i = 0; i < len/2; i+=chan)
	{
	    for(j=0;j<chan;++j)
	    {
		out[i+j] = out[i+j]*volume + (1-volume)*buf2[i+j];
	    }

	}
	g_free(buf2);
    }

    return len;

}

    static gint64
xmms_crossfade_seek (xmms_xform_t *xform, gint64 offset, xmms_xform_seek_mode_t whence, xmms_error_t *err)
{
    return xmms_xform_seek (xform, offset, whence, err);
}
