/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2008 XMMS2 Team
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

#ifndef __XMMS_COLLECTION_H__
#define __XMMS_COLLECTION_H__

#include <glib.h>


/*
 * Public definitions
 */

#define XMMS_COLLECTION_NUM_NAMESPACES  2

typedef enum {
	XMMS_COLLECTION_NSID_COLLECTIONS,
	XMMS_COLLECTION_NSID_PLAYLISTS,
	XMMS_COLLECTION_NSID_ALL,
	XMMS_COLLECTION_NSID_INVALID,
} xmms_collection_namespace_id_t;


/*
 * Private defintions
 */

struct xmms_coll_dag_St;
typedef struct xmms_coll_dag_St xmms_coll_dag_t;

#include "xmms/xmms_error.h"
#include "xmmsc/xmmsc_coll.h"
#include "xmmspriv/xmms_playlist.h"
#include "xmmspriv/xmms_medialib.h"

typedef void (*FuncApplyToColl)(xmms_coll_dag_t *dag, xmmsc_coll_t *coll, xmmsc_coll_t *parent, void *udata);


/*
 * Public functions
 */

xmms_coll_dag_t * xmms_collection_init (xmms_playlist_t *playlist);

xmmsc_coll_t * xmms_collection_get (xmms_coll_dag_t *dag, gchar *collname, gchar *namespace, xmms_error_t *error);
GList * xmms_collection_list (xmms_coll_dag_t *dag, gchar *namespace, xmms_error_t *error);
gboolean xmms_collection_save (xmms_coll_dag_t *dag, gchar *name, gchar *namespace, xmmsc_coll_t *coll, xmms_error_t *error);
gboolean xmms_collection_remove (xmms_coll_dag_t *dag, gchar *collname, gchar *namespace, xmms_error_t *error);
GList * xmms_collection_find (xmms_coll_dag_t *dag, guint mid, gchar *namespace, xmms_error_t *error);
gboolean xmms_collection_rename (xmms_coll_dag_t *dag, gchar *from_name, gchar *to_name, gchar *namespace, xmms_error_t *error);
void xmms_collection_sync (xmms_coll_dag_t *dag, xmms_error_t *error);

GList * xmms_collection_query_ids (xmms_coll_dag_t *dag, xmmsc_coll_t *coll, guint lim_start, guint lim_len, GList *order, xmms_error_t *err);
GList * xmms_collection_query_infos (xmms_coll_dag_t *dag, xmmsc_coll_t *coll, guint lim_start, guint lim_len, GList *order, GList *fetch, GList *group, xmms_error_t *err);

void xmms_collection_foreach_in_namespace (xmms_coll_dag_t *dag, guint nsid, GHFunc f, void *udata);
void xmms_collection_apply_to_all_collections (xmms_coll_dag_t *dag, FuncApplyToColl f, void *udata);
void xmms_collection_apply_to_collection (xmms_coll_dag_t *dag, xmmsc_coll_t *coll, FuncApplyToColl f, void *udata);

xmmsc_coll_t * xmms_collection_get_pointer (xmms_coll_dag_t *dag, const gchar *collname, guint namespace);
void xmms_collection_update_pointer (xmms_coll_dag_t *dag, const gchar *name, guint nsid, xmmsc_coll_t *newtarget);
const gchar * xmms_collection_find_alias (xmms_coll_dag_t *dag, guint nsid, xmmsc_coll_t *value, const gchar *key);
xmms_medialib_entry_t xmms_collection_get_random_media (xmms_coll_dag_t *dag, xmmsc_coll_t *source);
void xmms_collection_dag_replace (xmms_coll_dag_t *dag, xmms_collection_namespace_id_t nsid, gchar *key, xmmsc_coll_t *newcoll);

xmms_collection_namespace_id_t xmms_collection_get_namespace_id (gchar *namespace);
const gchar *xmms_collection_get_namespace_string (xmms_collection_namespace_id_t nsid);

gboolean xmms_collection_get_int_attr (xmmsc_coll_t *coll, const gchar *attrname, gint *val);
gboolean xmms_collection_set_int_attr (xmmsc_coll_t *coll, const gchar *attrname, gint newval);

GHashTable *xmms_collection_changed_msg_new (xmms_collection_changed_actions_t type, const gchar *plname, const gchar *namespace);
void xmms_collection_changed_msg_send (xmms_coll_dag_t *colldag, GHashTable *dict);

void bind_all_references (xmms_coll_dag_t *dag, xmmsc_coll_t *coll, xmmsc_coll_t *parent, void *udata);

#define XMMS_COLLECTION_PLAYLIST_CHANGED_MSG(dag, name) xmms_collection_changed_msg_send (dag, xmms_collection_changed_msg_new (XMMS_COLLECTION_CHANGED_UPDATE, name, XMMS_COLLECTION_NS_PLAYLISTS))


#endif
