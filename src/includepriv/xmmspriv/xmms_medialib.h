/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2009 XMMS2 Team
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




#ifndef __XMMS_PRIV_MEDIALIB_H__
#define __XMMS_PRIV_MEDIALIB_H__

#include "xmms/xmms_medialib.h"
#include "xmmspriv/xmms_playlist.h"
#include "xmmspriv/xmms_collection.h"

typedef struct xmms_medialib_St xmms_medialib_t;

xmms_medialib_t *xmms_medialib_init (xmms_playlist_t *playlist);

GList *xmms_medialib_info_list (xmms_medialib_t *medialib, guint32 id, xmms_error_t *err);

xmms_medialib_entry_t xmms_medialib_entry_not_resolved_get (void);
guint xmms_medialib_num_not_resolved (void);

void xmms_medialib_entry_remove (xmms_medialib_entry_t entry);

void xmms_medialib_entry_cleanup (xmms_medialib_entry_t entry);
xmms_medialib_entry_t xmms_medialib_entry_new_encoded (const char *url, xmms_error_t *error);
gboolean xmms_medialib_decode_url (char *url);
gboolean xmms_medialib_check_id (xmms_medialib_entry_t entry);

gboolean xmms_medialib_entry_property_set_str_source (xmms_medialib_entry_t entry, const gchar *property, const gchar *value, const gchar *source);
gboolean xmms_medialib_entry_property_set_int_source (xmms_medialib_entry_t entry, const gchar *property, gint value, const gchar *source);
void xmms_medialib_add_recursive (xmms_medialib_t *medialib, const gchar *playlist, const gchar *path, xmms_error_t *error);
void xmms_medialib_insert_recursive (xmms_medialib_t *medialib, const gchar *playlist, gint32 pos, const gchar *path, xmms_error_t *error);

gint xmms_medialib_query_random_id (xmms_coll_dag_t *dag, xmmsv_coll_t *coll);
GList *xmms_medialib_query_ids (xmms_coll_dag_t *dag, xmmsv_coll_t *coll);
GList *xmms_medialib_query_infos (xmms_coll_dag_t *dag, xmmsv_coll_t *coll, xmmsv_t *fetch);

#endif
