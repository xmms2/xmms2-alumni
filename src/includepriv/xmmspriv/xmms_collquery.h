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

#ifndef __XMMS_COLLQUERY_H__
#define __XMMS_COLLQUERY_H__

#include <glib.h>

/*
 * Private definitions
 */

#include "xmmsc/xmmsv_coll.h"
#include "xmmspriv/xmms_collection.h"


/*
 * Public functions
 */

GList* xmms_coll_query_clustered (xmms_coll_dag_t *dag, xmmsv_coll_t *coll, const gchar *spec, xmms_error_t *err);
GList* xmms_coll_query_medialist (xmms_coll_dag_t *dag, xmmsv_coll_t *coll, const gchar *fetch, xmms_error_t *err);
GList* xmms_coll_query_legacy (xmms_coll_dag_t *dag, xmmsv_coll_t *coll, guint limit_start, guint limit_len, xmmsv_t *order, xmmsv_t *fetch, xmmsv_t *group, xmms_error_t *err);

uint32_t **xmms_collection_query_idlist_register_get ();

#endif
