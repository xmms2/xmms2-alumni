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

#include "xmms/xmms_ringbuf.h"
#include "xmms/xmms_xformplugin.h"
#include "xmms/xmms_config.h"
#include "xmms/xmms_log.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

static gboolean xmms_crossfade_plugin_setup (xmms_xform_plugin_t *xform_plugin);
static gboolean xmms_crossfade_init (xmms_xform_t *xform);
static void xmms_crossfade_destroy (xmms_xform_t *xform);
static gint xmms_crossfade_read (xmms_xform_t *xform, xmms_sample_t *buf, gint len,
                          xmms_error_t *error);
static gint64 xmms_crossfade_seek (xmms_xform_t *xform, gint64 offset,
                            xmms_xform_seek_mode_t whence, xmms_error_t *err);
static void xmms_crossfade_duration_changed (xmms_object_t *object, xmmsv_t *_data,
                                  gpointer userdata);

typedef struct xmms_crossfade_priv_St {
	xmms_ringbuf_t* buffer;
	GMutex *buffer_lock;
	gint max_buffer_size;
} xmms_crossfade_data_t;

XMMS_XFORM_PLUGIN ("crossfade",
                   "Crossfade effect",
                   XMMS_VERSION,
                   "Crossfade effect",
                   xmms_crossfade_plugin_setup);

static void xmms_crossfade_duration_changed (xmms_object_t *object, xmmsv_t *data, gpointer userdata)
{
}

static void xmms_crossfade_auxdata_notification (xmms_xform_t *xform, const gchar *key, void *val)
{
	if (!g_strcasecmp (key, "input format changed")) {
		xmms_xform_outdata_type_copy (xform);
	}
}

static gboolean
xmms_crossfade_plugin_setup (xmms_xform_plugin_t *xform_plugin)
{
	xmms_xform_methods_t methods;

	XMMS_XFORM_METHODS_INIT (methods);

	methods.init = xmms_crossfade_init;
	methods.destroy = xmms_crossfade_destroy;
	methods.read = xmms_crossfade_read;
	methods.seek = xmms_crossfade_seek;
	methods.auxdata_notification = xmms_crossfade_auxdata_notification;

	xmms_xform_plugin_methods_set (xform_plugin, &methods);

	xmms_xform_plugin_config_property_register (xform_plugin, "duration", "5",
	                                            xmms_crossfade_duration_changed, NULL);

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

	xmms_xform_outdata_type_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              48000,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_outdata_type_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              44100,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_outdata_type_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              22050,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_outdata_type_add (xform_plugin,
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
	xmms_crossfade_data_t *priv;
	xmms_config_property_t *config;
	gint srate, channels;
	gfloat gain;

	g_return_val_if_fail (xform, FALSE);

	priv = g_new0 (xmms_crossfade_data_t, 1);
	g_return_val_if_fail (priv, FALSE);

	xmms_xform_private_data_set (xform, priv);

	config = xmms_xform_config_lookup (xform, "duration");
	/*g_return_val_if_fail (config, FALSE);
	xmms_config_property_callback_set (config, xmms_eq_config_changed, priv);
	priv->enabled = !!xmms_config_property_get_int (config);*/

	srate = xmms_xform_indata_get_int (xform, XMMS_STREAM_TYPE_FMT_SAMPLERATE);
	xmms_xform_outdata_type_add (xform, XMMS_STREAM_TYPE_FMT_CHANNELS, 2);

	priv->max_buffer_size = 4096*8;
	priv->buffer = xmms_ringbuf_new (priv->max_buffer_size);
	priv->buffer_lock = g_mutex_new ();

	xmms_xform_outdata_type_add (xform,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              48000,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_outdata_type_add (xform,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              44100,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_outdata_type_add (xform,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              22050,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_outdata_type_add (xform,
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
xmms_crossfade_destroy (xmms_xform_t *xform)
{
	gpointer priv;

	g_return_if_fail (xform);

	priv = xmms_xform_private_data_get (xform);

	xmms_ringbuf_clear (((xmms_crossfade_data_t*)priv)->buffer);

	g_free (priv);
}

static gint64
xmms_crossfade_seek (xmms_xform_t *xform, gint64 offset, xmms_xform_seek_mode_t whence, xmms_error_t *err)
{
	/* TODO_xforms: Fix seek */
	return xmms_xform_seek (xform, offset, whence, err);
}

static void
fill (xmms_xform_t *xform, xmms_crossfade_data_t *priv)
{
	xmms_error_t err;
	char buf[4096];
	int res;

    while (!xmms_ringbuf_geteos (priv->buffer) && xmms_ringbuf_bytes_free (priv->buffer) > 0)
    {
        res = xmms_xform_read (xform, buf, sizeof (buf) < xmms_ringbuf_bytes_free (priv->buffer) ? sizeof (buf) : xmms_ringbuf_bytes_free (priv->buffer), &err);
        if (res > 0) {
            xmms_ringbuf_write_wait (priv->buffer, buf, res, priv->buffer_lock);
        } else if (res == -1) {
            /* XXX copy error */
        } else {
            xmms_ringbuf_set_eos (priv->buffer, TRUE);
        }
    }
}

static gint
xmms_crossfade_read (xmms_xform_t *xform, xmms_sample_t *buf, gint len,
              xmms_error_t *error)
{
	xmms_crossfade_data_t *priv;
	gint read, chan, i;
	xmms_sample_t *originalBuf;
	gint originalLen = len;
	//GMutex *wait_mutex = g_mutex_new();
	//g_mutex_unlock (wait_mutex);

	g_return_val_if_fail (xform, -1);

	priv = xmms_xform_private_data_get (xform);
	g_return_val_if_fail (priv, -1);

	chan = xmms_xform_indata_get_int (xform, XMMS_STREAM_TYPE_FMT_CHANNELS);

    read = 0;

    if (len > 4096)
    {
        len = 4096;
    }

    //if (!xmms_ringbuf_iseos (priv->buffer))
        fill (xform, priv);

    gint usedBytes = xmms_ringbuf_bytes_used (priv->buffer);
    //if (usedBytes < 32768)
    //    usedBytes = 4096;

    // I want at least 3 seconds in the buffer
    while ((int)(xmms_ringbuf_bytes_used (priv->buffer) - len) < 12288 && !xmms_ringbuf_geteos (priv->buffer))
    {
        if (usedBytes < 32768)
            fill (xform, priv);
        usedBytes = xmms_ringbuf_bytes_used (priv->buffer);
        read += xmms_ringbuf_read (priv->buffer, buf, len);
        buf += read;
        len -= read;
    }

    if (len != 0)
    {
        read += xmms_ringbuf_read (priv->buffer, buf, len);
    }

	//read = xmms_ringbuf_read (priv->buffer, buf, len);
	//while (!xmms_ringbuf_iseos (priv->buffer) && xmms_ringbuf_bytes_used (priv->buffer) < 4096*3)
	//{
	if (!xmms_ringbuf_geteos (priv->buffer))
        fill (xform, priv);
	//}

	if (xmms_ringbuf_bytes_used (priv->buffer) < 32768)
	{
        //TODO: This is just for testing
        //originalBuf = read + originalBuf;
        for (i = 0; i < originalLen; i++)
        {
            *(gint*)originalBuf = 0;
            originalBuf++;
        }
	}

	//TODO: Just wait for duration to be queued
	//xmms_ringbuf_wait_used (priv->buffer, 4096*3, wait_mutex);

	//g_mutex_free (wait_mutex);

	//if (xmms_ringbuf_iseos (priv->buffer)) {
		/* TODO_xforms: Check for the duration, if we hit duration left in buffer then start crossfade */
		/* For now, mute to cert things work so far */
		//for (i = 0; i < read; i++) {
		  //  gint8 * castedBuffer = (gint8*)buf;
			//castedBuffer = 0;
	//	}
	//}

	return read;
}
