/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2008 XMMS2 Team
 *  Copyright (C) 2008 ogg.k.ogg.k@googlemail.com
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




/**
  * @file kate decoder.
  * @url http://wiki.xiph.org/index.php/OggKate
  */


#include "xmms/xmms_xformplugin.h"
#include "xmms/xmms_log.h"
#include "xmms/xmms_ipc.h"

#include <ogg/ogg.h>
#include <kate/kate.h>

#include <glib.h>
#include <unistd.h>

typedef struct {
    char *text;
    double t;
} lyrics_event;

typedef struct xmms_kate_data_St {
    ogg_sync_state ksy;
    ogg_stream_state kst;
    kate_state k;
    enum { kis_none, kis_sync, kis_all } kate_init_state;
    size_t num_lyrics_events;
    lyrics_event *lyrics_events;
    GThread *thread;
    GMutex *mutex;
    gboolean running;
    gchar *current_lyrics;

    /* Playtime approximation since we can't know playtime, but we want to sync lyrics anyway.
       See comments in xmms_kate_get_now for details. */
    GTimer *read_timer; /* elapsed since last read */
    double time_between_reads;
    double last_read_start_time; /* roughly estimated timestamp of the start of the last read buffer */
} xmms_kate_data_t;

/*
 * Function prototypes
 */

static gboolean xmms_kate_plugin_setup (xmms_xform_plugin_t *xform_plugin);
static gint xmms_kate_read (xmms_xform_t *xform, xmms_sample_t *buf, gint len, xmms_error_t *err);
static gboolean xmms_kate_init (xmms_xform_t *decoder);
static void xmms_kate_destroy (xmms_xform_t *decoder);
static gint64 xmms_kate_seek (xmms_xform_t *xform, gint64 samples, xmms_xform_seek_mode_t whence, xmms_error_t *err);
static gboolean xmms_kate_get_lyrics (xmms_xform_t *xform, xmms_error_t *err);

static double process_kate(xmms_xform_t *xform, const char *ptr, size_t bytes);
static void add_lyrics_event(xmms_xform_t *xform, const char *text, double t);
static void clear_lyrics_events(xmms_xform_t *xform);
static char *get_next_lyrics_event(xmms_xform_t *xform, double t);
static void set_current_lyrics(xmms_xform_t *xform, const char *text);
static gpointer lyrics_thread (gpointer arg);

/*
 * Plugin header
 */

XMMS_XFORM_PLUGIN ("kate",
                   "Kate Decoder",
                   XMMS_VERSION,
                   "Ogg/Kate lyrics decoder",
                   xmms_kate_plugin_setup);

static gboolean
xmms_kate_plugin_setup (xmms_xform_plugin_t *xform_plugin)
{
    xmms_xform_methods_t methods;

    XMMS_XFORM_METHODS_INIT (methods);
    methods.init = &xmms_kate_init;
    methods.destroy = &xmms_kate_destroy;
    methods.read = &xmms_kate_read;
    methods.seek = &xmms_kate_seek;
    methods.lyrics = &xmms_kate_get_lyrics;

    xmms_xform_plugin_methods_set (xform_plugin, &methods);

    xmms_xform_plugin_config_property_register (xform_plugin, "all-kate-streams",
                                                "0", NULL, NULL);
    xmms_xform_plugin_indata_add (xform_plugin,
                                  XMMS_STREAM_TYPE_MIMETYPE,
                                  "application/ogg",
                                  XMMS_STREAM_TYPE_PRIORITY,
                                  XMMS_STREAM_TYPE_PRIORITY_DEFAULT+1,
                                  NULL);

    /* We can't add magic for the Kate stream itself, for three reasons:
       - a lyrics stream might be multiplexed with other streams in any number
         of ways: there could be extra streams between Vorbis and Kate, etc,
         so the number of places where a Kate stream could be found is large
       - we would not be able to parse lyrics for chained Ogg streams where
         the first chain does not contain lyrics
       - Lyrics might well be multiplexed with FLAC (or another audio codec)
         rather than Vorbis, so the offset would be different since the size
         of the FLAC BOS packet isn't the same as the Vorbis one (and old FLAC
         streams are different still).
      We can't add magic for just Ogg, since it'll conflict with Vorbis.
    */
#if 0
    /* if there are just Vorbis and Kate streams */
    xmms_magic_add ("ogg/kate header",
                    "application/x-ogg-kate",
                    "0 string OggS", ">4 byte 0",
                    ">>86 string \x80kate",
                    NULL);

    /* if there is a Skeleton stream */
    xmms_magic_add ("ogg/kate header",
                    "application/x-ogg-kate",
                    "0 string OggS", ">4 byte 0",
                    ">>178 string \x80kate",
                    NULL);

    /* others... */
#endif

    return TRUE;
}

static int
xmms_kate_close (xmms_xform_t *xform)
{
    xmms_kate_data_t *data;

    g_return_val_if_fail (xform, -1);
    data = xmms_xform_private_data_get (xform);
    g_return_val_if_fail (data, -1);

    if (data) {
        if (data->kate_init_state == kis_all) {
            ogg_stream_clear(&data->kst);
        }
        if (data->kate_init_state >= kis_sync) {
            ogg_sync_reset(&data->ksy);
        }
        if (data->kate_init_state == kis_all) {
            kate_high_decode_clear(&data->k);
        }
        data->kate_init_state = kis_sync;
    }

    clear_lyrics_events(xform);

    return 0;
}

static void
xmms_kate_destroy (xmms_xform_t *xform)
{
    xmms_kate_data_t *data;

    g_return_if_fail (xform);

    data = xmms_xform_private_data_get (xform);
    g_return_if_fail (data);

    data->running = FALSE;
    if (data->thread) {
        g_thread_join (data->thread);
    }
    g_mutex_free (data->mutex);
    g_timer_destroy (data->read_timer);

    xmms_ipc_signal_unregister (XMMS_IPC_SIGNAL_LYRICS);

    xmms_kate_close(xform);

    g_free (data);
}

static gboolean
xmms_kate_init (xmms_xform_t *xform)
{
    xmms_kate_data_t *data;
    GError *error = NULL;

    g_return_val_if_fail (xform, FALSE);

    data = g_new0 (xmms_kate_data_t, 1),

    ogg_sync_init(&data->ksy);
    data->kate_init_state = kis_sync;
    data->num_lyrics_events = 0;
    data->lyrics_events = NULL;
    data->mutex = g_mutex_new ();
    data->thread = NULL;
    data->running = TRUE;
    data->current_lyrics = NULL;
    data->read_timer = g_timer_new ();
    data->time_between_reads = 0.0;
    data->last_read_start_time = 0.0;

    xmms_ipc_signal_register (XMMS_OBJECT (xform),
                              XMMS_IPC_SIGNAL_LYRICS);

    xmms_xform_private_data_set (xform, data);

    data->thread = g_thread_create (&lyrics_thread, xform, TRUE, &error);
    if (data->thread) {
        g_thread_set_priority (data->thread, G_THREAD_PRIORITY_LOW);
    }

    xmms_xform_outdata_type_add (xform,
                                 XMMS_STREAM_TYPE_MIMETYPE,
                                 "application/ogg",
                                 XMMS_STREAM_TYPE_END);

    return TRUE;
}

static gint
xmms_kate_read (xmms_xform_t *xform, gpointer buf, gint len,
                xmms_error_t *err)
{
    gint ret;
    xmms_kate_data_t *data;
    const gint chunk_size = 1024; /* the smaller, the greater the approximation */
    gint read = 0;
    char *chunk = buf;
    double lyrics_time;
    gint remaining_len = len;
    gboolean lyrics_found = FALSE;

    g_return_val_if_fail (xform, -1);
    data = xmms_xform_private_data_get (xform);
    g_return_val_if_fail (data, -1);

    data->time_between_reads = g_timer_elapsed(data->read_timer, NULL);
    g_timer_reset (data->read_timer);

    /* read in smaller chunks - see xmms_kate_get_now for details */
    while (remaining_len > 0) {
        gint size = remaining_len > chunk_size ? chunk_size : remaining_len;
        ret = xmms_xform_read (xform, chunk, size, err);
        if (ret < 0)
            return ret;

        lyrics_time = process_kate(xform, chunk, ret);
        if (lyrics_time >= 0.0) {
            double ratio = read / (double)len;
            double approx_start_time = lyrics_time - ratio * data->time_between_reads;
            /* take the earliest */
            if (!lyrics_found || approx_start_time < data->last_read_start_time)
                data->last_read_start_time = approx_start_time;
            lyrics_found = TRUE;
        }

        remaining_len -= ret;
        chunk += ret;
        read += ret;
        if (ret < size) /* short read - should we break, or try to continue ? */
            break;
    }

    /* if we did not find lyrics, we just set the buffer start time to the previous one plus the
       time between reads - this should work OK if there are not too many time fluctuations, but
       will miss pauses, etc */
    if (!lyrics_found) {
        data->last_read_start_time += data->time_between_reads;
    }

    return read;
}

static void
reset_timing (xmms_kate_data_t *data)
{
    g_return_if_fail (data);

    g_timer_reset (data->read_timer);
    data->time_between_reads = 0.0;
    data->last_read_start_time = 0.0;
}

static gint64
xmms_kate_seek (xmms_xform_t *xform, gint64 offset,
                xmms_xform_seek_mode_t whence, xmms_error_t *err)
{
    xmms_kate_data_t *data;

    g_return_val_if_fail (xform, -1);

    data = xmms_xform_private_data_get (xform);
    g_return_val_if_fail (data, -1);

    if (data->kate_init_state >= kis_sync) {
        ogg_sync_reset(&data->ksy);
    }

    g_mutex_lock (data->mutex);
    clear_lyrics_events(xform);
    reset_timing(data);
    g_mutex_unlock (data->mutex);

    return xmms_xform_seek (xform, offset, whence, err);
}

static gboolean
is_kate_bos(ogg_page *og)
{
    ogg_stream_state os;
    ogg_packet op;
    gboolean kate_bos = FALSE;

    if (!ogg_page_bos(og)) return FALSE;
    ogg_stream_init(&os, ogg_page_serialno (og));
    ogg_stream_pagein(&os, og);
    if (ogg_stream_packetpeek(&os, &op) > 0) {
        if (op.bytes>=8 && !memcmp(op.packet,"\200kate\0\0\0",8))
            kate_bos = TRUE;
        ogg_stream_clear(&os);
    }
    return kate_bos;
}

static void
reset_kate(xmms_xform_t *xform)
{
    xmms_kate_data_t *data;

    data = xmms_xform_private_data_get (xform);
    g_return_if_fail (data);

    if (data->kate_init_state == kis_all) {
        ogg_stream_clear(&data->kst);
        kate_high_decode_clear(&data->k);
    }

    if (data->kate_init_state >= kis_sync) {
        data->kate_init_state = kis_sync;
    }

    g_mutex_lock (data->mutex);
    clear_lyrics_events(xform);
    reset_timing(data);
    g_mutex_unlock (data->mutex);
}

static double
process_kate(xmms_xform_t *xform, const char *ptr, size_t sz)
{
    int ret;
    ogg_page og;
    ogg_packet op;
    char *buffer;
    xmms_kate_data_t *data;
    double lyrics_time = -1.0;

    g_return_val_if_fail (xform, -1.0);
    data = xmms_xform_private_data_get (xform);
    g_return_val_if_fail (data, -1.0);

    if (data->kate_init_state < kis_sync)
        return -1.0;

    buffer = ogg_sync_buffer(&data->ksy, sz);
    if (buffer) {
        memcpy(buffer, ptr, sz);
        ogg_sync_wrote(&data->ksy, sz);
    }

    while (ogg_sync_pageout(&data->ksy, &og) > 0) {
        /* if we get a Kate BOS, reset */
        if (is_kate_bos(&og)) {
            XMMS_DBG("found Kate BOS - init for %08x", ogg_page_serialno (&og));
            reset_kate(xform);
            ogg_stream_init(&data->kst, ogg_page_serialno (&og));
            ogg_stream_pagein(&data->kst, &og);
            ret = kate_high_decode_init(&data->k);
            if (ret < 0) {
                xmms_log_error("kate_high_decode_init failed: %d", ret);
                ogg_stream_clear(&data->kst);
                data->kate_init_state = kis_sync;
            }
            else {
              if (ogg_stream_packetout(&data->kst, &op) > 0) {
                kate_packet kp;
                kate_packet_wrap(&kp, op.bytes, op.packet);
                ret = kate_high_decode_packetin(&data->k, &kp, NULL);
                if (ret < 0) {
                    xmms_log_error("kate_high_decode_packetin failed: %d", ret);
                }
                else {
                  const xmms_config_property_t *val = xmms_xform_config_lookup (xform, "all-kate-streams");
                  gint all = val && xmms_config_property_get_int (val);
                  if (!all && strcmp(data->k.ki->category, "lyrics") && strcmp(data->k.ki->category, "LRC")) {
                    XMMS_DBG("Kate stream has category '%s', ignored", data->k.ki->category);
                    ogg_stream_clear(&data->kst);
                    data->kate_init_state = kis_sync;
                  }
                  else {
                      data->kate_init_state = kis_all;
                  }
                }
              }
              else {
                xmms_log_error("Initial Kate page did not contain the BOS packet\n");
                ogg_stream_clear(&data->kst);
                data->kate_init_state = kis_sync;
              }
            }
        }
        else if (data->kate_init_state == kis_all) {
           ogg_stream_pagein(&data->kst, &og);

            while (ogg_stream_packetout(&data->kst, &op) > 0) {
                const kate_event *ev = NULL;
                kate_packet kp;
                kate_packet_wrap(&kp, op.bytes, op.packet);
                ret = kate_high_decode_packetin(&data->k, &kp, &ev);
                if (ret < 0) {
                    xmms_log_error("kate_high_decode_packetin failed: %d\n", ret);
                }
                else {
                    if (ev) {
                        g_mutex_lock (data->mutex);
                        add_lyrics_event (xform, ev->text, ev->start_time);
                        add_lyrics_event (xform, "", ev->end_time);
                        g_mutex_unlock (data->mutex);

                        /* remember the time of the first lyrics, if any, in (or around) this buffer */
                        if (lyrics_time < 0)
                            lyrics_time = ev->start_time;
                    }
                    if (ret > 0) {
                        /* eos */
                        reset_kate (xform);
                    }
                }
            }
        }
    }
    return lyrics_time;
}

static void
add_lyrics_event(xmms_xform_t *xform, const char *text, double t)
{
    xmms_kate_data_t *data;
    lyrics_event *new_lyrics_events;

    g_return_if_fail (xform);
    data = xmms_xform_private_data_get (xform);
    g_return_if_fail (data);

    g_return_if_fail (text);
    g_return_if_fail (t >= 0.0);

    /* lyrics are sent as signals, and it seems that sending two signals very
       close together will drop the second one on the floor. Since lyrics typically
       start just when the previous one ends, we'll check whether a lyrics line is
       added at the same time or slightly after the last one, and replace it if so */
    if (data->num_lyrics_events>0 && t-data->lyrics_events[data->num_lyrics_events-1].t<0.1) {
        g_free(data->lyrics_events[data->num_lyrics_events-1].text);
        --data->num_lyrics_events;
    }
    else {
        new_lyrics_events = (lyrics_event*)g_realloc(data->lyrics_events, (data->num_lyrics_events+1)*sizeof(lyrics_event));
        if (!new_lyrics_events) {
            /* failed to allocate */
            return;
        }
        data->lyrics_events = new_lyrics_events;
    }

    data->lyrics_events[data->num_lyrics_events].text = g_strdup(text);
    data->lyrics_events[data->num_lyrics_events].t = t;

    ++data->num_lyrics_events;
}

static char *
get_next_lyrics_event(xmms_xform_t *xform, double t)
{
    xmms_kate_data_t *data;
    char *text;

    g_return_val_if_fail (xform, NULL);
    data = xmms_xform_private_data_get (xform);
    g_return_val_if_fail (data, NULL);

    if (data->num_lyrics_events == 0) return NULL;
    if (data->lyrics_events[0].t > t) return NULL;

    text = data->lyrics_events[0].text;
    --data->num_lyrics_events;
    memmove(data->lyrics_events, data->lyrics_events+1, sizeof(lyrics_event)*data->num_lyrics_events);

    return text; /* caller owns and must free */
}

/* called with mutex held, or after the lyrics thread has died */
static void
clear_lyrics_events(xmms_xform_t *xform)
{
    xmms_kate_data_t *data;
    size_t n;

    g_return_if_fail (xform);
    data = xmms_xform_private_data_get (xform);
    g_return_if_fail (data);

    for (n=0; n<data->num_lyrics_events; ++n) {
        /* NULL is fine to free */
        g_free(data->lyrics_events[n].text);
    }
    g_free(data->lyrics_events);
    data->lyrics_events = NULL;
    data->num_lyrics_events = 0;

    set_current_lyrics(xform, "");
}

static void
set_current_lyrics(xmms_xform_t *xform, const char *text)
{
    xmms_kate_data_t *data;

    g_return_if_fail (xform);
    data = xmms_xform_private_data_get (xform);
    g_return_if_fail (data);

    if (data->current_lyrics)
        g_free(data->current_lyrics);
    data->current_lyrics = text ? g_strdup(text) : NULL;

    xmms_object_emit_f (XMMS_OBJECT (xform),
                        XMMS_IPC_SIGNAL_LYRICS,
                        XMMSV_TYPE_STRING,
                        text);
}

static gboolean
xmms_kate_get_lyrics (xmms_xform_t *xform, xmms_error_t *err)
{
    xmms_kate_data_t *data;

    g_return_val_if_fail (xform, FALSE);
    data = xmms_xform_private_data_get (xform);
    g_return_val_if_fail (xform, FALSE);

    g_mutex_lock (data->mutex);
    xmms_xform_set_lyrics (xform, data->current_lyrics);
    g_mutex_unlock (data->mutex);

    return TRUE;
}

static double
xmms_kate_get_now (xmms_kate_data_t *data)
{
    double latency = 0.0;

    g_return_val_if_fail (data, 0.0);

    /* And this is where it sucks: in order to signal lyrics at the correct time,
       we need to know where we are in playback, so we can compare this time to
       the start time for the next lyrics line. However, we don't know where we
       are in playback, as we can't get that information cleanly.
       A possible approach to mitigate this could be timing the delay between
       reads and interpolating blindly since Vorbis packets, which we are likely
       to be multiplexed with, are of relatively constant size, but this would
       only work once we get a Kate packet (as those are the ones we can know
       the timestamp of with certainty), and would cause trouble with pause as
       we get no notification of that either, so we can only deduce after the
       fact is the next read doesn't happen within a given time window. */

    /* Ideally, I'd call xmms_output_playtime, but it's not for plugins, and
       I don't have the output plugin handy anyway. */

    /* Alternatively, if the Vorbis plugin could tell me about its DSP time,
       I could use that and not be much in advance - this is what I do in the
       version that's within the Vorbis plugin. However, the lyrics might be
       multiplexed with another stream type than Vorbis. */

    /* So, for now, we return FLT_MAX so lyrics will get signalled as soon as
       they are read. If the read buffer is not too large, timing's not too
       far off, but larger buffers will cause lyrics to appear earlier */

    /* This also means that if there are several lyrics line within a single
       read's worth of Ogg data, they'll be signalled at the same time, which
       in practice means only the last one will be visible */

    /* No I don't do that anymore actually, "better" solution below
    return FLT_MAX;
    */

    /* Another solution, a bit hairy, but works:

       each time we get a read request: remember delay since last read, and start a timer
       on reset/seek: clear lyrics, clear timer
       split reads into smaller ones, and check each of them to see if we get a kate packet
       once we get a kate packet, we know its timestamp, and we know where (approx) in the
         large read it is, since we broke it up (so we know in which part it is)
            -> so we deduce (approx) the start time of the large read
            -> get_now: start time of the previous large read, plus elapsed time since
               last read. Ain't pretty, but it works OK in practice.
       the output will have some latency, but we don't know it, we just hope it's too too
         large or we'll be off (too early)

       BTW, this methods relies on the Kate packets being multiplexed optimally with
       the audio - this is usually the case, but Ogg allows non optimal multiplexing.
       The farther away from optimal muxing, the more the timing will be off.
    */

    /* Half a second's worth - just happens to look alright, though it might well
       depend on the output plugin, sound hardware, phase of the moon, etc.
       Are you feeling nauseous yet ? Gimme a xmms_get_output_playtime API :) */
    latency = 0.5;

    return data->last_read_start_time + g_timer_elapsed (data->read_timer, NULL) - latency;
}

static gpointer
lyrics_thread (gpointer arg)
{
    xmms_xform_t *xform;
    xmms_kate_data_t *data;
    double t;

    xform = (xmms_xform_t *)arg;
    g_return_val_if_fail (xform, NULL);
    data = xmms_xform_private_data_get (xform);
    g_return_val_if_fail (data, NULL);

    /* loop till told to end, popping next lyrics at the right time */
    while (data->running) {
        g_mutex_lock (data->mutex);
        t = xmms_kate_get_now (data);
        char *lyrics = get_next_lyrics_event (xform, t);
        if (lyrics) {
            set_current_lyrics (xform, lyrics);
            g_free (lyrics);
        }
        g_mutex_unlock (data->mutex);
		g_usleep (G_USEC_PER_SEC / 10); /* wait a tenth of a second, so we don't hog the CPU */
    }
    return NULL;
}

