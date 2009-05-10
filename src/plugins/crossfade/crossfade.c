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
} xmms_crossfade_data_t;

XMMS_XFORM_PLUGIN ("crossfade",
                   "Crossfade effect",
                   XMMS_VERSION,
                   "Crossfade effect",
                   xmms_crossfade_plugin_setup);

static void xmms_crossfade_duration_changed (xmms_object_t *object, xmmsv_t *data, gpointer userdata)
{
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

	return TRUE;
}

static gboolean
xmms_eq_init (xmms_xform_t *xform)
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
	channels = xmms_xform_indata_get_int (xform, XMMS_STREAM_TYPE_FMT_CHANNELS);

	priv->buffer = g_new0 (char, srate * xmms_config_property_get_int (config));

	xmms_xform_outdata_type_copy (xform);

	XMMS_DBG ("Equalizer initialized successfully!");

	return TRUE;
}

static void
xmms_crossfade_destroy (xmms_xform_t *xform)
{
	gpointer priv;

	g_return_if_fail (xform);

	priv = xmms_xform_private_data_get (xform);

	g_free (((xmms_crossfade_data_t*)priv)->buffer);
	g_free (priv);
}

static gint
xmms_crossfade_read (xmms_xform_t *xform, xmms_sample_t *buf, gint len,
              xmms_error_t *error)
{
	xmms_crossfade_data_t *priv;
	gint read, chan;

	g_return_val_if_fail (xform, -1);

	priv = xmms_xform_private_data_get (xform);
	g_return_val_if_fail (priv, -1);

	read = xmms_xform_read (xform, buf, len, error);
	chan = xmms_xform_indata_get_int (xform, XMMS_STREAM_TYPE_FMT_CHANNELS);
/*	if (read > 0 && priv->enabled) {
		iir (buf, read, chan, priv->extra_filtering);
	}*/

	return read;
}
