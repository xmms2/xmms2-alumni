/** @file crossfade.c
 *  Cross Fade Plugin
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


/** TODO LIST *
 * replace DELAY_TIME by a proper config property
 * fix bugs #1 #2 #3
 * add more input formats (S24)
 * change line 265 to make something prettier
 * */

/** BUG LIST *
 *
 * #1 When the user changes the song (next/prev) it advances a little bit
 *
 * #2 When the number of channels varies between two songs, we hear "bad stuff"
 *
 * #3 The last song in the playlist does not fade out. We need to use the "play" command again to make it fade out while the first song is fading in
 *
 * */


// NOTE : won't work fine if delay_time is > song length / 2 - should check this
// we will also write it as a config property later
#define DELAY_TIME 3
#define CHUNK_SIZE 4096


/* this struct defines the buffer we will use in the plugin. 
 * We will need two buffers to fade between two songs
*/
 typedef struct {

     /* we need to store the format of the associated song */
    gint framerate;
    gint channels;

    /* if we have stopped filling the buffer (end of stream reached in the audio stream) */
    gboolean eos;

    /* we store the volume of this buffer (1 = std volume, 0 = muted)
     * so to fade in/out we will need to slowly inc/decrease this float number 
     * to know "where we are" in the fading, we also need a fading time 
     */
    gfloat volume;
    gint current_fading_time;

    /* this actually contains the audio buffer */
    xmms_ringbuf_t *buf;
    gint buffered;
    gint requested;


} xmms_fade_buffer_t;

/* this structure defines the private data we will use in the plugin 
 * it mainly contains two audio buffers
 */
typedef struct {

    /* we need two buffers */
    xmms_fade_buffer_t *buffer[2];
    /* and we need to know in which buffer we read/write */
    gint8 read_buffer,write_buffer;

    /* when we crossfade two songs, we need to read inside both of the buffers */
    gboolean read_both_buffers;

    /* used when the user manually changes the song */
    gboolean skipped;


} xmms_crossfade_data_t;

static gboolean xmms_crossfade_plugin_setup (xmms_xform_plugin_t *xform_plugin);
static gboolean xmms_crossfade_init (xmms_xform_t *xform);
static void xmms_crossfade_destroy (xmms_xform_t *xform);
static gint xmms_crossfade_read (xmms_xform_t *xform, xmms_sample_t *buf, gint len,
	xmms_error_t *error);
static gint64 xmms_crossfade_seek (xmms_xform_t *xform, gint64 offset,
	xmms_xform_seek_mode_t whence, xmms_error_t *err);

XMMS_XFORM_PLUGIN ("crossfade",
	"Crossfade Plugin",
	XMMS_VERSION,
	"Crossfade Plugin",
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

    //	xmms_xform_plugin_config_property_register (xform_plugin, "delay_time", "3", NULL, NULL);
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

    xmms_crossfade_data_t *priv;
    gint framerate, channels;
    priv = g_new0 ( xmms_crossfade_data_t, 1);
    xmms_xform_private_data_set (xform, priv);
    framerate = xmms_xform_indata_get_int(xform,XMMS_STREAM_TYPE_FMT_SAMPLERATE);
    channels = xmms_xform_indata_get_int (xform, XMMS_STREAM_TYPE_FMT_CHANNELS);

    for(i=0; i<2;++i) {
	priv->buffer[i] = g_malloc(sizeof(xmms_fade_buffer_t));
	priv->buffer[i]->eos=FALSE;
	priv->buffer[i]->framerate = framerate;
	priv->buffer[i]->channels = channels;
	priv->buffer[i]->current_fading_time = 0;
	priv->buffer[i]->volume=1;
	priv->buffer[i]->requested = DELAY_TIME*framerate*channels*2;
	priv->buffer[i]->buf = xmms_ringbuf_new(priv->buffer[i]->requested);
	priv->buffer[i]->buffered = 0;
    }


    priv->read_buffer = 0;
    priv->write_buffer = 0;
    priv->read_both_buffers = FALSE;
    priv->skipped = FALSE;

    xmms_xform_outdata_type_copy (xform);

    return TRUE;
}

    static void
xmms_crossfade_destroy (xmms_xform_t *xform)
{
    xmms_crossfade_data_t *data;
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
    xmms_crossfade_data_t *data;
    xmms_fade_buffer_t *read_buffer, *write_buffer;
    g_return_val_if_fail (xform, -1);

    data = xmms_xform_private_data_get (xform);
    g_return_val_if_fail (data, -1);

    read_buffer =  data->buffer[data->read_buffer];
    write_buffer = data->buffer[data->write_buffer];



    // Check if we need to adjust the buffer size due to format change
    if(write_buffer->eos || read_buffer->eos) {
	/* check for format change */
	chan = xmms_xform_indata_get_int (xform, XMMS_STREAM_TYPE_FMT_CHANNELS);
	srate = xmms_xform_indata_get_int(xform,XMMS_STREAM_TYPE_FMT_SAMPLERATE);

	if( srate != write_buffer->framerate || chan != write_buffer->channels) {
	    // the frame rate or the chan have changed, we need to reallocate the buffer
	    write_buffer->framerate = srate;
	    write_buffer->channels = chan;
	    write_buffer->requested = DELAY_TIME*srate*chan*2;
	    write_buffer->buffered = 0;
	    xmms_ringbuf_destroy(write_buffer->buf);
	    write_buffer->buf = xmms_ringbuf_new(write_buffer->requested);
	}
    }



    // Check if the song has been skipped by the user
     if(xmms_xform_song_has_been_skipped_get(xform) && !data->skipped) {
	data->skipped = TRUE;
	// TODO: You absolutely need to change this
	xmms_xform_song_has_been_skipped_set(xform,FALSE);
	read_buffer->eos = TRUE;
	data->write_buffer = (data->write_buffer+1)%2;
	write_buffer = data->buffer[data->write_buffer];
    }



    /* ensure the buffer is filled */
    while (write_buffer->buffered < write_buffer->requested + CHUNK_SIZE) {
	char t[CHUNK_SIZE];
	int l = MIN( CHUNK_SIZE,  write_buffer->requested + CHUNK_SIZE - write_buffer->buffered);
	int r;
	r = xmms_xform_read (xform, t, l, error);
	if (r <= 0) {
	    // The end of the song is reached. Note that we are still playing it.
	    write_buffer->eos = TRUE;
	    data->write_buffer = (data->write_buffer+1)%2;
	    XMMS_DBG("Crossfade Plugin : end of stream reached");
	    xmms_log_info("END OF STREAM");
	    data->skipped = FALSE;
	    return r;
	}
	xmms_ringbuf_write (write_buffer->buf, t, r);
	write_buffer->buffered += r;
    }


    // the buffer is fully filled - we need to read both buffers
    if(read_buffer->eos) {
	
	read_buffer->current_fading_time++;
	
	if(!data->read_both_buffers) {
	    data->read_both_buffers = TRUE;
	}

	gint volume = MIN(read_buffer->requested,write_buffer->requested);
	// we decrease the volume of the fading out song
	read_buffer->volume -= 1/(gfloat)(volume/(gfloat)CHUNK_SIZE);



	if(read_buffer->current_fading_time*CHUNK_SIZE >= read_buffer->requested)
	{
	    // The crossfade is over
	    read_buffer->volume = 1;
	    XMMS_DBG("Crossfade Plugin : crossfade over");
	    xmms_log_info("END OF CROSSFADE");
	    read_buffer->eos = FALSE;
	    read_buffer->buffered = 0;
	    read_buffer->current_fading_time=0;
	    data->read_buffer = (data->read_buffer+1)%2;
	    data->read_both_buffers = FALSE;
	    read_buffer = data->buffer[data->read_buffer];
	    data->skipped = FALSE;
	}

    }


    // Are we crossfading ?
    if(data->read_both_buffers)
    {

	read = xmms_ringbuf_read (write_buffer->buf, buf, len);
	write_buffer->buffered -= read;


	gfloat volume = read_buffer->volume;
	// K represent a factor between the two formats
	// For example, if song A is 44100Hz and song B is 22050Hz, then K = 2
	gint K;
	gint k = 0;
	// Indicates if we are upsampling or downsampling
	gboolean up_sample;


	// Note that :
	// read buffer is the fading out song
	// write buffer is the next song to be played

	short *fade_out;
	if(write_buffer->framerate >= read_buffer->framerate) {
	    // Here we are upsampling
	    // Note that both framerate are equal, we are neither upsampling nor downsampling
	    K=write_buffer->framerate/read_buffer->framerate;
	    fade_out = g_malloc(len*sizeof(short)/K);
	    read = xmms_ringbuf_read (read_buffer->buf, fade_out, len/K);
	    read_buffer->buffered -= read;
	    up_sample = true;
	}
	else {
	    // Here we are downsampling
	    up_sample = false;
	    K=read_buffer->framerate/write_buffer->framerate;
	    fade_out = g_malloc(len*sizeof(short)*K);
	    read = xmms_ringbuf_read (read_buffer->buf, fade_out, len*K);
	    read_buffer->buffered -= read;
	    // check that we haven't read too far
	    if(read <= 0) {
		read_buffer->volume = 1;
		read_buffer->eos = FALSE;
		read_buffer->buffered = 0;
		read_buffer->current_fading_time=0;
		data->read_buffer = (data->read_buffer+1)%2;
		data->read_both_buffers = FALSE;
    XMMS_DBG("Crossfade Plugin : previous song over");
	    xmms_log_info("END OF PLAY");


	    }
	}
	short *mem_to_free = fade_out;
	short *out = buf;
	chan = write_buffer->channels;

	gint fade_out_chan;

	// This is not optimized, but clear enough for tests
	for (i = 0; i < len/2; i+=chan ) {

	    fade_out_chan = 0;
	    for ( j = 0; j < chan; ++j ) {
		out[i+j] = fade_out[fade_out_chan]*volume + (1-volume)*out[i+j];
		if(fade_out_chan < read_buffer->channels) fade_out_chan++;
	    }

	    if(up_sample) { 
		//upsampling
		if(k%K==0) {
		    k = 0;
		    fade_out += read_buffer->channels;
		}
		k++;
	    }

	    else {
		// downsampling
		fade_out += read_buffer->channels*K;
		
	    }

	}

	g_free(mem_to_free);
    }
    else
    {
	read = xmms_ringbuf_read (read_buffer->buf, buf, len);
	read_buffer->buffered -= read;

    }




    return len;

}

    static gint64
xmms_crossfade_seek (xmms_xform_t *xform, gint64 offset, xmms_xform_seek_mode_t whence, xmms_error_t *err)
{
    return xmms_xform_seek (xform, offset, whence, err);
}

