/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003	Peter Alm, Tobias Rundstr�m, Anders Gustafsson
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




/** @file
 * Decoder.
 *
 * This file contains functions that manipulate xmms_decoder_t objects.
 *
 */


#include "xmmspriv/xmms_decoder.h"
#include "xmmspriv/xmms_ringbuf.h"
#include "xmmspriv/xmms_visualisation.h"
#include "xmmspriv/xmms_effect.h"
#include "xmmspriv/xmms_sample.h"
#include "xmmspriv/xmms_plugin.h"
#include "xmms/xmms_object.h"
#include "xmmspriv/xmms_output.h"
#include "xmmspriv/xmms_transport.h"
#include "xmmspriv/xmms_medialib.h"
#include "xmms/xmms_log.h"

#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>

/*
 * Type definitions
 */


/** 
 * @defgroup Decoder Decoder
 * @ingroup XMMSServer
 * @brief Decoder module takes encoded data and decodes it.
 * @{
 */

/**
 * Structure describing decoder-objects.
 * Do not modify this structure directly outside this file, use the functions.
 */
struct xmms_decoder_St {
	xmms_object_t object;

	gboolean running;
	GThread *thread;
	GMutex *mutex;

	xmms_plugin_t *plugin;
	xmms_transport_t *transport; /**< transport associated with decoder.
	                               *   This is where the decoder gets it
	                               *   data from
	                               */

	gpointer plugin_data;

	xmms_medialib_entry_t entry;

	GList *effects; /* list of xmms_effect_t */


	xmms_output_t *output; /**< output associated with decoder.
	                         *   The decoded data will be written
	                         *   to this output.
	                         */
	xmms_visualisation_t *vis;

	guint decoded_frames; /**< Number of frames decoded so far */


	GList *format_list;
	GList *output_format_list;
	xmms_sample_converter_t *converter;


	xmms_ringbuf_t *buffer;
};

/*
 * Static function prototypes
 */

static xmms_plugin_t *xmms_decoder_find_plugin (const gchar *mimetype);
static gpointer xmms_decoder_thread (gpointer data);

/*
 * Public functions
 */

/** @} */


/**
 * @defgroup DecoderPlugin DecoderPlugin
 * @ingroup XMMSPlugin
 * @brief Decoder plugin documentation.
 *
 * A decoder plugin takes data from the transport and
 * unpacks/transforms it to stereo PCM 16bit data. A decoder
 * plugin needs to define the following methods to be valid:
 *
 * #XMMS_PLUGIN_METHOD_NEW allocates a new decoder. This will be 
 * called if can_handle returns TRUE. The prototype is:
 * @code
 * gboolean new (xmms_decoder_t *decoder, const gchar *mimetype);
 * @endcode
 * This method need to set the private data with xmms_decoder_data_set()
 *
 * #XMMS_PLUGIN_METHOD_INIT Initializes the decoders private data.
 * This is called when XMMS wants to play this file. Will only
 * be called once. Prototype is:
 * @code
 * gboolean init (xmms_decoder_t *decoder);
 * @endcode
 * Return TRUE if all works out. FALSE will abort the playback
 * of this source.
 *
 * #XMMS_PLUGIN_METHOD_DESTROY Destroys the decoder private data.
 * Prototype is:
 * @code
 * void destroy (xmms_decoder_t *decoder);
 * @endcode
 * Free all private data in the decoder struct here.
 *
 * #XMMS_PLUGIN_METHOD_CAN_HANDLE this method will be called
 * when XMMS has a new source of data to check if this plugin will
 * take care of this. The prototype is:
 * @code
 * gboolean can_handle (const gchar *mimetype);
 * @endcode
 * This method should return TRUE if we want to handle this mimetype.
 *
 * #XMMS_PLUGIN_METHOD_DECODE_BLOCK unpacks or transforms a 
 * block of data from the transporter. The method calls
 * xmms_transport_read() to read the data and then xmms_decoder_write()
 * after the data is transformed/unpacked. The prototype of this method
 * is:
 * @code
 * gboolen decode_block (xmms_decoder_t *decoder);
 * @endcode
 * This method should return TRUE if decode_block can be called
 * again. FALSE will discontinue decoding.
 *
 * #XMMS_PLUGIN_METHOD_GET_MEDIAINFO Extracts the mediainfo for this 
 * sourcetype. This will be called from the mediainfo reader when
 * something is added to the playlist. Prototype is:
 * @code
 * void mediainfo (xmms_decoder_t *decoder);
 * @endcode
 *
 * #XMMS_PLUGIN_METHOD_SEEK Will be called when XMMS wants to
 * seek in the current source. The prototype is:
 * @code
 * void seek (xmms_decoder_t *decoder, guint samples);
 * @endcode
 * The argument is how many relative samples we want to seek to.
 * This method should call xmms_transport_seek() to seek in transporter.
 *
 * @{
 */

/**
 * Extract plugin specific data from the decoder.
 * This should be set with #xmms_decoder_private_data_set.
 *
 * @sa xmms_decoder_private_data_set
 */

gpointer
xmms_decoder_private_data_get (xmms_decoder_t *decoder)
{
	gpointer ret;
	g_return_val_if_fail (decoder, NULL);

	ret = decoder->plugin_data;

	return ret;
}

/**
 * Set the private data for the plugin.
 * Data can be extracted by #xmms_decoder_private_data_get.
 *
 * @sa xmms_decoder_private_data_get
 */

void
xmms_decoder_private_data_set (xmms_decoder_t *decoder, gpointer data)
{
	g_return_if_fail (decoder);

	decoder->plugin_data = data;
}

/**
 * Get the transport associated with the decoder.
 *
 * @param decoder apanap
 * @return the associated transport
 */

xmms_transport_t *
xmms_decoder_transport_get (xmms_decoder_t *decoder)
{
	xmms_transport_t *ret;
	g_return_val_if_fail (decoder, NULL);

	g_mutex_lock (decoder->mutex);
	ret = decoder->transport;
	g_mutex_unlock (decoder->mutex);

	return ret;
}

void
xmms_decoder_format_add (xmms_decoder_t *decoder, xmms_sample_format_t fmt, guint channels, guint rate)
{
        xmms_audio_format_t *f;

        g_return_if_fail (decoder);
        g_return_if_fail (fmt);
        g_return_if_fail (channels);
        g_return_if_fail (rate);

        f = xmms_sample_audioformat_new (fmt, channels, rate);

        g_return_if_fail (f);

        decoder->format_list = g_list_append (decoder->format_list, f);

}

xmms_audio_format_t *
xmms_decoder_format_finish (xmms_decoder_t *decoder)
{
	xmms_sample_converter_t *converter;
	xmms_audio_format_t *fmt;
        GList *l;

        g_return_val_if_fail (decoder, NULL);

        converter = xmms_sample_audioformats_coerce (decoder->format_list,
		                                             decoder->output_format_list);
	if (!converter)
		return NULL;

	decoder->converter = converter;
	
	fmt = xmms_sample_converter_get_from (converter);

	xmms_visualisation_format_set (decoder->vis, fmt);

	for (l = decoder->effects; l; ) {
		if (!xmms_effect_format_set (l->data, fmt)) {
			GList *n;
			XMMS_DBG ("Rockstar ate my effect (didn't support format)");
			n = l->next;
			decoder->effects = g_list_delete_link (decoder->effects, l);
			l = n;
		} else {
			xmms_effect_entry_set (l->data, decoder->entry);
			l = g_list_next (l);
		}
	}

        return fmt;
}

xmms_audio_format_t *
xmms_decoder_audio_format_to_get (xmms_decoder_t *decoder)
{
	xmms_audio_format_t *ret = NULL;

	g_return_val_if_fail (decoder, NULL);

	g_mutex_lock (decoder->mutex);
	ret = xmms_sample_converter_get_to (decoder->converter);
	g_mutex_unlock (decoder->mutex);

	return ret;
}

/**
 * Read decoded data
 */

guint
xmms_decoder_read (xmms_decoder_t *decoder, gchar *buf, guint len)
{
	guint ret;
	
	g_return_val_if_fail (decoder, -1);
	g_return_val_if_fail (buf, -1);

	g_mutex_lock (decoder->mutex);
	ret = xmms_ringbuf_read_wait (decoder->buffer, buf, len, decoder->mutex);
	g_mutex_unlock (decoder->mutex);

	return ret;
}

/**
  * Get the EOS state on the decoder buffer.
  * @returns TRUE if decoder is EOS
  */

gboolean
xmms_decoder_iseos (xmms_decoder_t *decoder)
{
	gboolean ret;

	g_return_val_if_fail (decoder, FALSE);
	
	g_mutex_lock (decoder->mutex);
	ret = decoder->thread ? FALSE : TRUE;
	g_mutex_unlock (decoder->mutex);
	return ret;
}

/**
 * Write decoded data.
 * Should be used in #XMMS_PLUGIN_METHOD_DECODE_BLOCK to write the decoded
 * pcm-data to the output. The data should be 32bits per stereosample, ie
 * pairs of signed 16-bits samples. (in what byter order?)
 * xmms_decoder_samplerate_set must be called before any data is passed
 * to this function.
 *
 * @param decoder
 * @param buf buffer containing pcmdata in 2*S16
 * @param len size of buffen in bytes
 *
 */
void
xmms_decoder_write (xmms_decoder_t *decoder, gchar *buf, guint len)
{
	GList *l;
	xmms_sample_t *outbuf;
	guint outlen;

	g_return_if_fail (decoder);

	for (l = decoder->effects; l; l = g_list_next (l)) {
		xmms_effect_run (l->data, buf, len);
	}
	
	xmms_visualisation_calc (decoder->vis, buf, len, decoder->decoded_frames);

	xmms_sample_convert (decoder->converter, buf, len, &outbuf, &outlen);

	g_mutex_lock (decoder->mutex);
	xmms_ringbuf_write_wait (decoder->buffer, outbuf, outlen, decoder->mutex);
	g_mutex_unlock (decoder->mutex);
	
	
}

/** @} */


/** 
 * @defgroup Decoder Decoder
 * @ingroup XMMSServer
 *
 * @{
 */

/**
 * Seek to specified (in ms) position in song.
 * 
 * @param decoder decoder pointer
 * @param milliseconds The number of milliseconds to seek to.
 * @param err On failure this will be used.
 *
 * @returns the new position measured in _output_ samples, or -1 on error
 */
gint32
xmms_decoder_seek_ms (xmms_decoder_t *decoder, guint milliseconds, xmms_error_t *err)
{
	guint samples;
	g_return_val_if_fail (decoder, -1);

	samples = xmms_sample_ms_to_samples (xmms_sample_converter_get_from (decoder->converter), milliseconds);

	return xmms_decoder_seek_samples (decoder, samples, err);

}

/**
 * Seek to specified (in samples) position in song.
 * 
 * @param decoder decoder pointer
 * @param milliseconds The number of milliseconds to seek to.
 * @param err On failure this will be used.
 *
 * @returns the new position measured in _output_ samples, or -1 on error
 */
gint32
xmms_decoder_seek_samples (xmms_decoder_t *decoder, guint samples, xmms_error_t *err)
{
	xmms_decoder_seek_method_t meth;
	
	g_return_val_if_fail (decoder, FALSE);

	meth = xmms_plugin_method_get (decoder->plugin, XMMS_PLUGIN_METHOD_SEEK);

	g_return_val_if_fail (meth, FALSE);

	if (!meth (decoder, samples)) {
		xmms_error_set (err, XMMS_ERROR_GENERIC, "Could not seek there");
		return -1;
	}

	g_mutex_lock (decoder->mutex);
	xmms_ringbuf_clear (decoder->buffer);
	g_mutex_unlock (decoder->mutex);

	return xmms_sample_samples_to_converted_bytes (decoder->converter, samples);
}

/**
 * Get the output associated with the decoder.
 */
xmms_output_t *
xmms_decoder_output_get (xmms_decoder_t *decoder)
{
	xmms_output_t *ret;
	g_return_val_if_fail (decoder, NULL);

	g_mutex_lock (decoder->mutex);
	ret = decoder->output;
	g_mutex_unlock (decoder->mutex);

	return ret;
}

/**
 * Get the plugin used to instanciate this decoder.
 */
xmms_plugin_t *
xmms_decoder_plugin_get (xmms_decoder_t *decoder)
{
	xmms_plugin_t *ret;
	g_return_val_if_fail (decoder, NULL);

	g_mutex_lock (decoder->mutex);
	ret = decoder->plugin;
	g_mutex_unlock (decoder->mutex);

	return ret;
}

/*
 * Private functions
 */

static void
xmms_decoder_destroy (xmms_object_t *object) 
{
	xmms_decoder_t *decoder = (xmms_decoder_t *)object;
	xmms_decoder_destroy_method_t destroy_method = NULL;
	GList *n;

	xmms_ringbuf_set_eos (decoder->buffer, TRUE);

	if (decoder->plugin) {
		destroy_method = xmms_plugin_method_get (decoder->plugin, XMMS_PLUGIN_METHOD_DESTROY);
	}
	if (destroy_method)
		destroy_method (decoder);

	for (n = decoder->format_list; n; n = g_list_next (n)) {
		xmms_sample_audioformat_destroy (n->data);
	}
	g_list_free (decoder->format_list);

	g_list_free (decoder->effects);

	xmms_ringbuf_destroy (decoder->buffer);
	g_mutex_free (decoder->mutex);
	if (decoder->transport) {
		xmms_transport_stop (decoder->transport);
		xmms_object_unref (decoder->transport);
	}
	if (decoder->plugin) 
		xmms_object_unref (decoder->plugin);
	xmms_object_unref (decoder->converter);
	xmms_object_unref (decoder->vis);
}

/**
 * Allocate a new decoder.
 * Remember to unref it to free memory!
 */

xmms_decoder_t *
xmms_decoder_new ()
{
	xmms_decoder_t *decoder;
	xmms_config_value_t *val;

	decoder = xmms_object_new (xmms_decoder_t, xmms_decoder_destroy);
	decoder->mutex = g_mutex_new ();
	decoder->vis = xmms_visualisation_new ();

	val = xmms_config_lookup ("decoder.buffersize");
	decoder->buffer = xmms_ringbuf_new (xmms_config_value_int_get (val));

	return decoder;
}

/**
 * Open a decoder for this transport. This must be done before you call
 * #xmms_decoder_read.
 *
 * @param transport Initialized transport that points to encoded data.
 * @param decoder A allocated decoder from #xmms_decoder_new
 * @returns TRUE if a suitable decoder was found
 */

gboolean
xmms_decoder_open (xmms_decoder_t *decoder, xmms_transport_t *transport)
{
	xmms_plugin_t *plugin;
	xmms_decoder_new_method_t new_method;
	const gchar *mimetype;

	g_return_val_if_fail (decoder, FALSE);
	g_return_val_if_fail (transport, FALSE);

	mimetype = xmms_transport_mimetype_get (transport);
	decoder->entry = xmms_transport_medialib_entry_get (transport);

	XMMS_DBG ("Trying to create decoder for mime-type %s", mimetype);
	
	plugin = xmms_decoder_find_plugin (mimetype);
	if (!plugin) {
		XMMS_DBG ("Plugin for %s was *NOT* found!", mimetype);
		return FALSE;
	}
	
	xmms_object_ref (transport);

	XMMS_DBG ("Found plugin: %s", xmms_plugin_name_get (plugin));

	decoder->transport = transport;
	decoder->plugin = plugin;

	new_method = xmms_plugin_method_get (plugin, XMMS_PLUGIN_METHOD_NEW);

	if (!new_method || !new_method (decoder, mimetype)) {
		xmms_log_error ("open failed");
		xmms_object_unref (transport);
		return FALSE;
	}

	return TRUE;
}

/**
 * Initializes the decoder.
 * Only called by the output layer.
 *
 * @param decoder
 * @param output_format_list List with xmms_audio_format_t's that are
 *                           supported by the output plugin
 * @param effects A list of effect plugins to apply
 */
gboolean
xmms_decoder_init (xmms_decoder_t *decoder, GList *output_format_list,
                   GList *effects)
{
	gboolean ret;
	xmms_decoder_init_method_t init_meth;

	g_return_val_if_fail (decoder, FALSE);

	/* we need to store the format list here since the init method of
	 * the decoder plugin will most likely call
	 * xmms_decoder_format_finish() at some point, which relies on the
	 * output format list.
	 * ditto for the effect list.
	 * also, we copy the list here, because we'll remove entries if they
	 * don't accept our sample format.
	 */
	decoder->output_format_list = output_format_list;
	decoder->effects = g_list_copy (effects);

	init_meth = xmms_plugin_method_get (decoder->plugin,
	                                    XMMS_PLUGIN_METHOD_INIT);

	ret = init_meth && init_meth (decoder);
	if (!ret) {
		decoder->output_format_list = NULL;
	}

	return ret;
}

/**
 * Dispatch execution of the decoder.
 *
 * Associates the decoder with a transport and output.
 * Blesses it with a life of its own (a new thread is created)
 *
 * @param decoder
 * @param output
 *
 */
void
xmms_decoder_start (xmms_decoder_t *decoder, xmms_output_t *output)
{
	g_return_if_fail (decoder);
	g_return_if_fail (output);
	
	decoder->running = TRUE;
	decoder->output = output;
	decoder->thread = g_thread_create (xmms_decoder_thread, decoder, FALSE, NULL); 
}

/**
 * Quit all decoder operations.
 */

void
xmms_decoder_stop (xmms_decoder_t *decoder)
{
	g_return_if_fail (decoder);
	g_mutex_lock (decoder->mutex);
	decoder->running = FALSE;
	xmms_ringbuf_set_eos (decoder->buffer, TRUE);
	xmms_ringbuf_clear (decoder->buffer);
	g_mutex_unlock (decoder->mutex);
}


/**
 * Resolv metadata for the current entry
 */
void
xmms_decoder_mediainfo_get (xmms_decoder_t *decoder, 
			    xmms_transport_t *transport)
{
	xmms_decoder_get_mediainfo_method_t mediainfo;
	g_return_if_fail (decoder);
	g_return_if_fail (transport);

	decoder->transport = transport;

	mediainfo = xmms_plugin_method_get (decoder->plugin, XMMS_PLUGIN_METHOD_GET_MEDIAINFO);
	if (!mediainfo) {
		xmms_log_error ("get_mediainfo failed");
		return;
	}

	mediainfo (decoder);

	return;
}

xmms_medialib_entry_t
xmms_decoder_medialib_entry_get (xmms_decoder_t *decoder)
{
	g_return_val_if_fail (decoder, 0);
	return decoder->entry;
}

/*
 * Static functions
 */

static xmms_plugin_t *
xmms_decoder_find_plugin (const gchar *mimetype)
{
        GList *list, *node;
        xmms_plugin_t *plugin = NULL;
        xmms_decoder_can_handle_method_t can_handle;
	
        g_return_val_if_fail (mimetype, NULL);
	
        list = xmms_plugin_list_get (XMMS_PLUGIN_TYPE_DECODER);
	
        for (node = list; node; node = g_list_next (node)) {
                plugin = node->data;
                XMMS_DBG ("Trying plugin: %s", xmms_plugin_shortname_get (plugin));
                can_handle = xmms_plugin_method_get (plugin, XMMS_PLUGIN_METHOD_CAN_HANDLE);

                if (!can_handle)
                        continue;
		
                if (can_handle (mimetype)) {
                        xmms_object_ref (plugin);
                        break;
                }
        }
        if (!node)
                plugin = NULL;
	
        if (list)
                xmms_plugin_list_destroy (list);
	
        return plugin;
}

static gpointer
xmms_decoder_thread (gpointer data)
{
        xmms_transport_t *transport;

        xmms_decoder_t *decoder = data;
        xmms_decoder_decode_block_method_t decode_block;

        g_return_val_if_fail (decoder, NULL);

        decode_block = xmms_plugin_method_get (decoder->plugin, XMMS_PLUGIN_METHOD_DECODE_BLOCK);
        if (!decode_block)
                return NULL;

        xmms_object_ref (decoder);

        transport = xmms_decoder_transport_get (decoder);
        /*xmms_medialib_logging_start (entry);*/

        g_mutex_lock (decoder->mutex);

        while (42) {
                gboolean ret;

                g_mutex_unlock (decoder->mutex);
                ret = decode_block (decoder);
                g_mutex_lock (decoder->mutex);

                if (!ret || !decoder->running) {
                        break;
                }
        }

        decoder->thread = NULL;
        xmms_medialib_logging_stop (decoder->entry, xmms_output_playtime (decoder->output, NULL));

        if (decoder->running) {
                /* This means that we eofed... */
                XMMS_DBG ("EOF!");
                xmms_ringbuf_set_eos (decoder->buffer, TRUE);
                xmms_object_emit_f (XMMS_OBJECT (decoder),
                                XMMS_IPC_SIGNAL_DECODER_THREAD_EXIT,
                                XMMS_OBJECT_CMD_ARG_NONE,
                                NULL);
        } else {
                xmms_ringbuf_clear (decoder->buffer);
        }

        g_mutex_unlock (decoder->mutex);

        xmms_object_unref (decoder);

        return NULL;
}
