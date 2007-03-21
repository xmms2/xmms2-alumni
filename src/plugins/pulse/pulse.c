/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2007 XMMS2 Team
 *
 *  PLUGINS ARE NOT CONSIDERED TO BE DERIVED WORK !!!
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 */

#include "xmms/xmms_outputplugin.h"
#include "xmms/xmms_log.h"

#include <pulse/simple.h>

#include <glib.h>

/*
 * Type definitions
 */
typedef struct {
  pa_simple *server;
  pa_sample_spec format;
} xmms_pulse_data_t;

static struct {
	xmms_sample_format_t xmms_fmt;
	pa_sample_format_t pulse_fmt;
} formats[] = {
	{XMMS_SAMPLE_FORMAT_U8, PA_SAMPLE_U8},
	{XMMS_SAMPLE_FORMAT_S16, PA_SAMPLE_S16NE},
	{XMMS_SAMPLE_FORMAT_FLOAT, PA_SAMPLE_FLOAT32NE},
};

/*
 * Function prototypes
 */
static gboolean xmms_pulse_plugin_setup (xmms_output_plugin_t *plugin);
static void xmms_pulse_flush (xmms_output_t *output);
static void xmms_pulse_close (xmms_output_t *output);
static void xmms_pulse_write (xmms_output_t *output, gpointer buffer, gint len,
                             xmms_error_t *err);
static gboolean xmms_pulse_open (xmms_output_t *output);
static gboolean xmms_pulse_new (xmms_output_t *output);
static void xmms_pulse_destroy (xmms_output_t *output);
static gboolean xmms_pulse_format_set (xmms_output_t *output,
                                      const xmms_stream_type_t *format);

/*
 * Plugin header
 */
XMMS_OUTPUT_PLUGIN ("pulse", "PulseAudio Output", XMMS_VERSION,
                    "Output to a PulseAudio server",
                    xmms_pulse_plugin_setup);

static gboolean
xmms_pulse_plugin_setup (xmms_output_plugin_t *plugin)
{
	xmms_output_methods_t methods;

	XMMS_OUTPUT_METHODS_INIT (methods);

	methods.new = xmms_pulse_new;
	methods.destroy = xmms_pulse_destroy;
	methods.open = xmms_pulse_open;
	methods.close = xmms_pulse_close;
	methods.write = xmms_pulse_write;
	methods.flush = xmms_pulse_flush;
	methods.format_set = xmms_pulse_format_set;

	xmms_output_plugin_methods_set (plugin, &methods);

	xmms_output_plugin_config_property_register (plugin, "server", "",
	                                             NULL, NULL);
	xmms_output_plugin_config_property_register (plugin, "sink", "",
	                                             NULL, NULL);
	xmms_output_plugin_config_property_register (plugin, "name", "XMMS2",
	                                             NULL,NULL);

	return TRUE;
}

static gboolean
xmms_pulse_new (xmms_output_t *output)
{
	xmms_pulse_data_t *data;
	gint i;

	g_return_val_if_fail (output, FALSE);
	data = g_new0 (xmms_pulse_data_t, 1);
	g_return_val_if_fail (data, FALSE);

	xmms_output_private_data_set (output, data);
	for (i = 0; i < sizeof (formats); i++)
		/* TODO: make channels/samplerate flexible. */
		xmms_output_format_add (output, formats[i].xmms_fmt, 2, 44100);

	return TRUE;
}

static void
xmms_pulse_destroy (xmms_output_t *output)
{
	xmms_pulse_data_t *data;

	g_return_if_fail (output);
	data = xmms_output_private_data_get (output);
	g_return_if_fail (data);

	if (data->server)
		pa_simple_free(data->server);

	g_free(data);
}

static gboolean
xmms_pulse_open (xmms_output_t *output)
{
	return TRUE;
}

static void
xmms_pulse_close (xmms_output_t *output)
{
	xmms_pulse_data_t *data;

	g_return_if_fail (output);
	data = xmms_output_private_data_get (output);
	g_return_if_fail (data);

	if (data->server) {
		pa_simple_free(data->server);
		data->server = NULL;
	}
}

static gboolean
xmms_pulse_format_set (xmms_output_t *output, const xmms_stream_type_t *format)
{
	xmms_pulse_data_t *data;
	const xmms_config_property_t *val;
	const gchar *server, *sink, *name;
	xmms_sample_format_t xmms_format;
	pa_sample_format_t pa_format = PA_SAMPLE_INVALID;
	gint channels;
	gint samplerate;
	gint i;

	g_return_val_if_fail (output, FALSE);
	data = xmms_output_private_data_get (output);
	g_return_val_if_fail (data, FALSE);

	xmms_format = xmms_stream_type_get_int (format, XMMS_STREAM_TYPE_FMT_FORMAT);
	channels = xmms_stream_type_get_int (format, XMMS_STREAM_TYPE_FMT_CHANNELS);
	samplerate = xmms_stream_type_get_int (format, XMMS_STREAM_TYPE_FMT_SAMPLERATE);

	for (i = 0; i < sizeof(formats); i++) {
		if (formats[i].xmms_fmt == xmms_format) {
			pa_format = formats[i].pulse_fmt;
			break;
		}
	}
	if (pa_format == PA_SAMPLE_INVALID)
		return FALSE;

	/* If format hasn't changed, do nothing. */
	if (pa_format == data->format.format
		&& channels == data->format.channels
		&& samplerate == data->format.rate)
		return TRUE;

	if (data->server) {
		pa_simple_drain(data->server, NULL);
		pa_simple_free(data->server);
	}

	data->format.format = pa_format;
	data->format.channels = channels;
	data->format.rate = samplerate;

	val = xmms_output_config_lookup (output, "server");
	server = xmms_config_property_get_string (val);
	if (server && *server == '\0')
		server = NULL;

	val = xmms_output_config_lookup (output, "sink");
	sink = xmms_config_property_get_string (val);
	if (sink && *sink == '\0')
		sink = NULL;

	val = xmms_output_config_lookup (output, "name");
	name = xmms_config_property_get_string (val);

	data->server = pa_simple_new(server, name, PA_STREAM_PLAYBACK, sink, name,
								 &data->format, NULL, NULL, NULL);

	return data->server ? TRUE : FALSE;
}

static void
xmms_pulse_flush (xmms_output_t *output)
{
	xmms_pulse_data_t *data;

	g_return_if_fail (output);
	data = xmms_output_private_data_get (output);
	g_return_if_fail (data);

	if (data->server)
		pa_simple_flush(data->server, NULL);
}

static void
xmms_pulse_write (xmms_output_t *output, gpointer buffer, gint len,
                 xmms_error_t *err)
{
	xmms_pulse_data_t *data;

	g_return_if_fail (output);
	g_return_if_fail (buffer);
	data = xmms_output_private_data_get (output);
	g_return_if_fail (data);

	pa_simple_write (data->server, buffer, len, NULL);
}
