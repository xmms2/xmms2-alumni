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

static gboolean
xmms_eq2_plugin_setup (xmms_xform_plugin_t *xform_plugin)
{
	xmms_xform_methods_t methods;
	gchar buf[16];
	gint i;

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

static gboolean
xmms_eq2_init (xmms_xform_t *xform)
{
	xmms_config_property_t *config;

	g_return_val_if_fail (xform, FALSE);


/*	config = xmms_xform_config_lookup (xform, "enabled");
	g_return_val_if_fail (config, FALSE);
	xmms_config_property_callback_set (config, xmms_eq2_config_changed, priv);
	priv->enabled = !!xmms_config_property_get_int (config);*/

	xmms_xform_outdata_type_copy (xform);

	return TRUE;
}

static void
xmms_eq2_destroy (xmms_xform_t *xform)
{
}

static gint
xmms_eq2_read (xmms_xform_t *xform, xmms_sample_t *buf, gint len,
              xmms_error_t *error)
{
	gint read, chan,i,j;
	g_return_val_if_fail (xform, -1);

	read = xmms_xform_read (xform, buf, len, error);
	chan = xmms_xform_indata_get_int (xform, XMMS_STREAM_TYPE_FMT_CHANNELS);
	short *data = buf;
	for (i = 0; i < len/2; i+=chan)
	{
	    for(j=0;j<chan;++j)
	    {
		data[i+j] = data[i+j];
	    }
	}
	return read;
}

static gint64
xmms_eq2_seek (xmms_xform_t *xform, gint64 offset, xmms_xform_seek_mode_t whence, xmms_error_t *err)
{
	return xmms_xform_seek (xform, offset, whence, err);
}
