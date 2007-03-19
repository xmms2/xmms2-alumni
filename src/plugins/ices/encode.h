/*
 * PCM data encoder for Icecast plugin.
 * Copyright (C) 2007 XMMS2 Team
 */

#ifndef __ENCODE_H
#define __ENCODE_H

#include <glib.h>
#include <ogg/ogg.h>
#include <vorbis/codec.h>

typedef struct encoder_state encoder_state;

encoder_state *xmms_ices_encoder_init(int min_br, int nom_br, int max_br);
void xmms_ices_encoder_fini(encoder_state *s);
gboolean xmms_ices_encoder_stream_change(encoder_state *s, int rate,
                                         int channels, vorbis_comment *vc);
void xmms_ices_encoder_input(encoder_state *s, float *buf, int n_samples);
void xmms_ices_encoder_finish(encoder_state *s);
gboolean xmms_ices_encoder_output(encoder_state *s, ogg_page *og);

#endif

