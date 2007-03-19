/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2007 XMMS2 Team
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

#ifndef __ENCODE_H
#define __ENCODE_H

#include <glib.h>
#include <ogg/ogg.h>
#include <vorbis/codec.h>

#include "xmms/xmms_sample.h"

typedef struct encoder_state encoder_state;

encoder_state *xmms_ices_encoder_init(int min_br, int nom_br, int max_br);
void xmms_ices_encoder_fini(encoder_state *s);
gboolean xmms_ices_encoder_stream_change(encoder_state *s, int rate,
                                         int channels, vorbis_comment *vc);
void xmms_ices_encoder_input(encoder_state *s, xmms_samplefloat_t *buf, int n_samples);
void xmms_ices_encoder_finish(encoder_state *s);
gboolean xmms_ices_encoder_output(encoder_state *s, ogg_page *og);

#endif

