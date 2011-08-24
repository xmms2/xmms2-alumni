/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2011 XMMS2 Team
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


#include <glib.h>
#include <math.h>
#include "xmmspriv/xmms_sample.h"
#include "xmms/xmms_medialib.h"
#include "xmms/xmms_object.h"
#include "xmms/xmms_log.h"

#include "libresample/libresample.h"

/**
  * @defgroup Sample Sample Converter
  * @ingroup XMMSServer
  * @brief Convert sample formats back and forth.
  * @{
  */

#define MAX_CHANNELS 8

/**
 * The converter module
 */
struct xmms_sample_converter_St {
	xmms_object_t obj;

	xmms_stream_type_t *from;
	xmms_stream_type_t *to;

	gboolean resample;

	/* buffer for result */
	guint finbufsiz;
	xmms_sample_t *finbuf;

	guint foutbufsiz;
	xmms_sample_t *foutbuf;

	guint outbufsiz;
	xmms_sample_t *outbuf;

	guint interpolator_ratio;
	guint decimator_ratio;

	guint offset;

	void *resamplers[MAX_CHANNELS];
	double resample_factor;
};

static void recalculate_resampler (xmms_sample_converter_t *conv, guint from, guint to);


static void
xmms_sample_converter_destroy (xmms_object_t *obj)
{
	xmms_sample_converter_t *conv = (xmms_sample_converter_t *) obj;

	g_free (conv->finbuf);
	g_free (conv->foutbuf);
	g_free (conv->outbuf);
}

xmms_sample_converter_t *
xmms_sample_converter_init (xmms_stream_type_t *from, xmms_stream_type_t *to)
{
	xmms_sample_converter_t *conv = xmms_object_new (xmms_sample_converter_t, xmms_sample_converter_destroy);
	gint fformat, fsamplerate, fchannels;
	gint tformat, tsamplerate, tchannels;
	gint i;

	fformat = xmms_stream_type_get_int (from, XMMS_STREAM_TYPE_FMT_FORMAT);
	fsamplerate = xmms_stream_type_get_int (from, XMMS_STREAM_TYPE_FMT_SAMPLERATE);
	fchannels = xmms_stream_type_get_int (from, XMMS_STREAM_TYPE_FMT_CHANNELS);
	tformat = xmms_stream_type_get_int (to, XMMS_STREAM_TYPE_FMT_FORMAT);
	tsamplerate = xmms_stream_type_get_int (to, XMMS_STREAM_TYPE_FMT_SAMPLERATE);
	tchannels = xmms_stream_type_get_int (to, XMMS_STREAM_TYPE_FMT_CHANNELS);

	g_return_val_if_fail (tformat != -1, NULL);
	g_return_val_if_fail (tchannels != -1, NULL);
	g_return_val_if_fail (tsamplerate != -1, NULL);

	if (fchannels < 1 || fchannels > 2 || tchannels < 1 || tchannels > 2) {
		xmms_log_error ("Multichannel in converter not handled yet!");
		xmms_object_unref (conv);
		return NULL;
	}

	conv->from = from;
	conv->to = to;
	conv->resample = fsamplerate != tsamplerate;

	if (conv->resample)
		recalculate_resampler (conv, fsamplerate, tsamplerate);

	conv->resample_factor = conv->decimator_ratio / conv->interpolator_ratio;
	for (i = 0; i < fchannels; i++) {
		conv->resamplers[i] = resample_open(1, conv->resample_factor, conv->resample_factor);
	}

	return conv;
}

/**
 * Return the audio format used by the converter as source
 */
xmms_stream_type_t *
xmms_sample_converter_get_from (xmms_sample_converter_t *conv)
{
	g_return_val_if_fail (conv, NULL);

	return conv->from;
}

/**
 * Return the audio format used by the converter as target
 */
xmms_stream_type_t *
xmms_sample_converter_get_to (xmms_sample_converter_t *conv)
{
	g_return_val_if_fail (conv, NULL);

	return conv->to;
}

/**
 */
void
xmms_sample_converter_to_medialib (xmms_sample_converter_t *conv, xmms_medialib_entry_t entry)
{
#if 0
	xmms_medialib_session_t *session;

	session = xmms_medialib_begin_write ();
	xmms_medialib_entry_property_set_str (session, entry,
	                                      XMMS_MEDIALIB_ENTRY_PROPERTY_FMT_SAMPLEFMT_IN,
	                                      xmms_sample_name_get (conv->from->format));
	xmms_medialib_entry_property_set_int (session, entry,
	                                      XMMS_MEDIALIB_ENTRY_PROPERTY_FMT_SAMPLERATE_IN,
	                                      conv->from->samplerate);
	xmms_medialib_entry_property_set_int (session, entry,
	                                      XMMS_MEDIALIB_ENTRY_PROPERTY_FMT_CHANNELS_IN,
	                                      conv->from->channels);

	xmms_medialib_entry_property_set_str (session, entry,
	                                      XMMS_MEDIALIB_ENTRY_PROPERTY_FMT_SAMPLEFMT_OUT,
	                                      xmms_sample_name_get (conv->to->format));
	xmms_medialib_entry_property_set_int (session, entry,
	                                      XMMS_MEDIALIB_ENTRY_PROPERTY_FMT_SAMPLERATE_OUT,
	                                      conv->to->samplerate);
	xmms_medialib_entry_property_set_int (session, entry,
	                                      XMMS_MEDIALIB_ENTRY_PROPERTY_FMT_CHANNELS_OUT,
	                                      conv->to->channels);

	xmms_medialib_end (session);
#endif
}


/**
 * convert from milliseconds to samples for this format.
 */
guint
xmms_sample_ms_to_samples (const xmms_stream_type_t *st, guint milliseconds)
{
	gint rate;
	rate = xmms_stream_type_get_int (st, XMMS_STREAM_TYPE_FMT_SAMPLERATE);
	return (guint)(((gdouble) rate) * milliseconds / 1000);
}

/**
 * Convert from samples to milliseconds for this format
 */
guint
xmms_sample_samples_to_ms (const xmms_stream_type_t *st, guint samples)
{
	gint rate;
	rate = xmms_stream_type_get_int (st, XMMS_STREAM_TYPE_FMT_SAMPLERATE);
	return (guint) (((gdouble)samples) * 1000.0 / rate);
}

/**
 * Convert from bytes to milliseconds for this format
 */
guint
xmms_sample_bytes_to_ms (const xmms_stream_type_t *st, guint bytes)
{
	guint samples = bytes / xmms_sample_frame_size_get (st);
	return xmms_sample_samples_to_ms (st, samples);
}

gint
xmms_sample_frame_size_get (const xmms_stream_type_t *st)
{
	gint format, channels;
	format = xmms_stream_type_get_int (st, XMMS_STREAM_TYPE_FMT_FORMAT);
	channels = xmms_stream_type_get_int (st, XMMS_STREAM_TYPE_FMT_CHANNELS);
	return xmms_sample_size_get (format) * channels;
}

static void
recalculate_resampler (xmms_sample_converter_t *conv, guint from, guint to)
{
	guint a,b;

	/* calculate ratio */
	if (from > to){
		a = from;
		b = to;
	} else {
		b = to;
		a = from;
	}

	while (b != 0) { /* good 'ol euclid is helpful as usual */
		guint t = a % b;
		a = b;
		b = t;
	}

	XMMS_DBG ("Resampling ratio: %d:%d",
	          from / a, to / a);

	conv->interpolator_ratio = to/a;
	conv->decimator_ratio = from/a;

	/*
	 * calculate filter here
	 *
	 * We don't use no stinkning filter. Maybe we should,
	 * but I'm deaf anyway, I wont hear any difference.
	 */

}



static void
samples_to_floats (xmms_sample_format_t format, xmms_sample_t *in, gfloat *out,
                   guint count, guint index, guint channels)
{
	int i;

	switch (format) {
		case XMMS_SAMPLE_FORMAT_S8: {
			xmms_samples8_t *tmp = in;
			for (i=0; i<count; i++) {
				out[i] = (gfloat)tmp[i*channels+index] / 128.0f;
			}
		} break;
		case XMMS_SAMPLE_FORMAT_U8: {
			xmms_sampleu8_t *tmp = in;
			for (i=0; i<count; i++) {
				out[i] = (gfloat)tmp[i*channels+index] / 128.0f - 1.0f;
			}
		} break;
		case XMMS_SAMPLE_FORMAT_S16: {
			xmms_samples16_t *tmp = in;
			for (i=0; i<count; i++) {
				out[i] = (gfloat)tmp[i*channels+index] / 32768.0f;
			}
		} break;
		case XMMS_SAMPLE_FORMAT_U16: {
			xmms_sampleu16_t *tmp = in;
			for (i=0; i<count; i++) {
				out[i] = (gfloat)tmp[i*channels+index] / 32768.0f - 1.0f;
			}
		} break;
		case XMMS_SAMPLE_FORMAT_S32: {
			xmms_samples32_t *tmp = in;
			for (i=0; i<count; i++) {
				out[i] = (gfloat)tmp[i*channels+index] / 2147483648.0f;
			}
		} break;
		case XMMS_SAMPLE_FORMAT_U32: {
			xmms_sampleu32_t *tmp = in;
			for (i=0; i<count; i++) {
				out[i] = (gfloat)tmp[i*channels+index] / 2147483648.0f - 1.0f;
			}
		} break;
		case XMMS_SAMPLE_FORMAT_FLOAT: {
			xmms_samplefloat_t *tmp = in;
			for (i=0; i<count; i++) {
				out[i] = tmp[i*channels+index];
			}
		} break;
		default: {
			xmms_log_error ("Unhandled sample format");
		} break;
	}
}

static void
floats_to_samples (xmms_sample_format_t format, gfloat *in, xmms_sample_t *out,
                   guint count, guint index, guint channels, gfloat mult)
{
	int i;

	switch (format) {
		case XMMS_SAMPLE_FORMAT_S8: {
			xmms_samples8_t *tmp = out;
			for (i=0; i<count; i++) {
				tmp[i*channels+index] += (xmms_samples8_t) (in[i] * mult * 128.0f);
			}
		} break;
		case XMMS_SAMPLE_FORMAT_U8: {
			xmms_sampleu8_t *tmp = out;
			for (i=0; i<count; i++) {
				tmp[i*channels+index] /= 2;
				tmp[i*channels+index] = (xmms_sampleu8_t) ((in[i] + 1.0f) * mult * 0.5f * 128.0f);
			}
		} break;
		case XMMS_SAMPLE_FORMAT_S16: {
			xmms_samples16_t *tmp = out;
			for (i=0; i<count; i++) {
				tmp[i*channels+index] += (xmms_samples16_t) (in[i] * mult * 32768.0f);
			}
		} break;
		case XMMS_SAMPLE_FORMAT_U16: {
			xmms_sampleu16_t *tmp = out;
			for (i=0; i<count; i++) {
				tmp[i*channels+index] /= 2;
				tmp[i*channels+index] = (xmms_sampleu16_t) ((in[i] + 1.0f) * mult * 0.5f * 32768.0f);
			}
		} break;
		case XMMS_SAMPLE_FORMAT_S32: {
			xmms_samples32_t *tmp = out;
			for (i=0; i<count; i++) {
				tmp[i*channels+index] += (xmms_samples32_t) (in[i] * mult * 2147483648.0f);
			}
		} break;
		case XMMS_SAMPLE_FORMAT_U32: {
			xmms_sampleu32_t *tmp = out;
			for (i=0; i<count; i++) {
				tmp[i*channels+index] /= 2;
				tmp[i*channels+index] = (xmms_sampleu32_t) ((in[i] + 1.0f) * mult * 0.5f * 2147483648.0f);
			}
		} break;
		case XMMS_SAMPLE_FORMAT_FLOAT: {
			xmms_samplefloat_t *tmp = out;
			for (i=0; i<count; i++) {
				tmp[i*channels+index] += in[i] * mult;
			}
		} break;
		default: {
			xmms_log_error ("Unhandled sample format");
		} break;
	}
}

static void
mix_channels (xmms_sample_format_t format, gfloat *in, xmms_sample_t *out,
              guint index, guint count, gint fchannels, gint tchannels)
{
	if (fchannels == 1) {
		if (tchannels == 1) {
			floats_to_samples (format, in, out, count, 0, tchannels, 1.0f);
		} else if (tchannels == 2) {
			floats_to_samples (format, in, out, count, 0, tchannels, 1.0f);
			floats_to_samples (format, in, out, count, 1, tchannels, 1.0f);
		}
	} else if (fchannels == 2) {
		if (tchannels == 1) {
			floats_to_samples (format, in, out, count, 0, tchannels, 0.5f);
		} else if (tchannels == 2) {
			floats_to_samples (format, in, out, count, index, tchannels, 1.0f);
		}
	}
}

/**
 * do the actual converstion between two audio formats.
 */
void
xmms_sample_convert (xmms_sample_converter_t *conv, xmms_sample_t *in, guint len, xmms_sample_t **out, guint *outlen)
{
	int inusiz, outusiz;
	int ilen, filen, folen, olen;
	gint fformat, fchannels, tformat, tchannels;
	guint res, i;

	inusiz = xmms_sample_frame_size_get (conv->from);
	g_return_if_fail (len % inusiz == 0);

	fformat = xmms_stream_type_get_int (conv->from, XMMS_STREAM_TYPE_FMT_FORMAT);
	tformat = xmms_stream_type_get_int (conv->to, XMMS_STREAM_TYPE_FMT_FORMAT);

	fchannels = xmms_stream_type_get_int (conv->from, XMMS_STREAM_TYPE_FMT_CHANNELS);
	g_return_if_fail (fchannels <= 0);

	tchannels = xmms_stream_type_get_int (conv->to, XMMS_STREAM_TYPE_FMT_CHANNELS);
	g_return_if_fail (tchannels <= 0);

	ilen = len / inusiz;
	filen = ilen * sizeof (gfloat);
	if (filen > conv->finbufsiz) {
		void *t;
		t = g_realloc (conv->finbuf, filen);
		g_assert (t); /* XXX */
		conv->finbuf = t;
		conv->finbufsiz = filen;
	}

	outusiz = xmms_sample_frame_size_get (conv->to);
	if (conv->resample) {
		olen = (ilen * conv->interpolator_ratio / conv->decimator_ratio + 1);
		folen = olen * sizeof(gfloat);
	} else {
		folen = ilen * sizeof(gfloat);
		olen = ilen;
	}

	if (folen > conv->foutbufsiz) {
		void *t;
		t = g_realloc (conv->foutbuf, folen);
		g_assert (t); /* XXX */
		conv->foutbuf = t;
		conv->foutbufsiz = folen;
	}
	if (olen > conv->outbufsiz) {
		void *t;
		t = g_realloc (conv->outbuf, olen * outusiz);
		g_assert (t); /* XXX */
		conv->outbuf = t;
		conv->outbufsiz = olen * outusiz;
	}

	for (i=0; i<fchannels; i++) {
		int inBufferUsed;

		samples_to_floats (fformat, in, conv->finbuf, ilen, i, fchannels);
		resample_process (conv->resamplers[i],
		                  conv->resample_factor,
		                  conv->finbuf, ilen, 0, &inBufferUsed,
		                  conv->foutbuf, olen);
		if (inBufferUsed != ilen) {
			// FIXME: shouldn't happen!
			xmms_log_error ("All input samples were not consumed!");
			return;
		}
		mix_channels (fformat, conv->foutbuf, conv->outbuf, i, olen, fchannels, tchannels);
	}

	// FIXME: should set res correctly
	res = olen;
	*outlen = res * outusiz;
	*out = conv->outbuf;
}

gint64
xmms_sample_convert_scale (xmms_sample_converter_t *conv, gint64 samples)
{
	/* this isn't 100% accurate, we should take care
	   of rounding here and set conv->offset, but noone
	   will notice, except when reading this comment :) */

	if (!conv->resample)
		return samples;
	return samples * conv->decimator_ratio / conv->interpolator_ratio;
}

gint64
xmms_sample_convert_rev_scale (xmms_sample_converter_t *conv, gint64 samples)
{
	if (!conv->resample)
		return samples;
	return samples * conv->interpolator_ratio / conv->decimator_ratio;
}

void
xmms_sample_convert_reset (xmms_sample_converter_t *conv)
{
	if (conv->resample) {
		conv->offset = 0;
	}
}

/**
 * @}
 */
