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



#ifndef __XMMS_VISUALISATION_H__
#define __XMMS_VISUALISATION_H__

#include "xmms/xmms_object.h"
#include "xmms/xmms_error.h"
#include "xmms/xmms_sample.h"

/* this is in number of samples! */
//~ #define FFT_BITS 10
//~ #define FFT_LEN (1<<FFT_BITS)

struct xmms_visualisation_St;
typedef struct xmms_visualisation_St xmms_visualisation_t;

uint32_t xmms_visualisation_version (xmms_visualisation_t *vis, xmms_error_t *err);
int32_t xmms_visualisation_register_client (xmms_visualisation_t *vis, xmms_error_t *err);
void xmms_visualisation_init_shm (xmms_visualisation_t *vis, int32_t id, int32_t shmid, int32_t semid, xmms_error_t *err);
int32_t xmms_visualisation_property_set (xmms_visualisation_t *vis, int32_t id, gchar *key, gchar *value, xmms_error_t *err);

/* TODO: do this right */
void xmms_visualisation_send_data (xmms_visualisation_t *vis, short l, short r);

int32_t xmms_visualisation_properties_set (xmms_visualisation_t *vis, int32_t id, GList *prop, xmms_error_t *err);
void xmms_visualisation_shutdown (xmms_visualisation_t *vis, int32_t id, xmms_error_t *err);

void xmms_visualisation_init (void);
xmms_visualisation_t *xmms_visualisation_new ();
//~ void xmms_visualisation_calc (xmms_visualisation_t *vis, xmms_sample_t *buf, int len, guint32 pos);
//~ void xmms_visualisation_format_set (xmms_visualisation_t *vis, xmms_audio_format_t *fmt);



#endif
