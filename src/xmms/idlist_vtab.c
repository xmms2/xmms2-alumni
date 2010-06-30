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

/** @file
 *  An sqlite virtual table, giving access to in-memory idlists from sqlite.
 */

/** @defgroup IdlistVTab Idlist Virtual Table
  * @ingroup CollectionQuery
  * @brief An sqlite virtual table to access in-memory idlists.
  *
  * @{
  */

#include <stdlib.h>
#include <glib.h>
#include <sqlite3.h>
#include "xmmspriv/xmms_collquery.h"

#define SCHEMA \
"CREATE TABLE idlist_vtab (idlist_id integer, pos integer, id integer);"

typedef struct idlist_vtab_cursor {
	sqlite3_vtab_cursor base;
	guint idlist;
	guint pos;
} idlist_vtab_cursor;

static int
idlist_vtab_destroy (sqlite3_vtab *vtab)
{
	g_free (vtab);
	return 0;
}

static int
idlist_vtab_create (sqlite3 *db, void *aux, int argc, const char *const*argv,
                    sqlite3_vtab **vtab, char **err)
{
	*vtab = g_new0 (sqlite3_vtab, 1);
	return sqlite3_declare_vtab (db, SCHEMA);
}

static int
idlist_vtab_open (sqlite3_vtab *vtab, sqlite3_vtab_cursor **cursor)
{
	*cursor = (sqlite3_vtab_cursor *)g_new0(idlist_vtab_cursor, 1);

	return SQLITE_OK;
}

static int
idlist_vtab_close (sqlite3_vtab_cursor *cursor)
{
	g_free ((idlist_vtab_cursor *)cursor);

	return SQLITE_OK;
}

static int
idlist_vtab_column (sqlite3_vtab_cursor *cur_in, sqlite3_context *context,
                    int column_index)
{
	idlist_vtab_cursor* cursor = (idlist_vtab_cursor *)cur_in;

	switch (column_index) {
		case 0:
			sqlite3_result_int (context, cursor->idlist);
			break;
		case 1:
			sqlite3_result_int (context, cursor->pos);
			break;
		case 2:
			sqlite3_result_int (context,
			                    xmms_collection_query_idlist_register_get ()
			                    [cursor->idlist][cursor->pos]);
			break;
		default:
			g_assert_not_reached ();
	}

	return SQLITE_OK;
}

static int
idlist_vtab_rowid (sqlite3_vtab_cursor *cur_in, sqlite_int64 *rowid)
{
	idlist_vtab_cursor *cursor = (idlist_vtab_cursor *)cur_in;

	*rowid = ((guint64)(cursor->idlist) << 32) | cursor->pos;

	return SQLITE_OK;
}

static int
idlist_vtab_eof (sqlite3_vtab_cursor *cur_in)
{
	idlist_vtab_cursor *cursor = (idlist_vtab_cursor *)cur_in;

	return ! xmms_collection_query_idlist_register_get ()[cursor->idlist];
}

static int
idlist_vtab_next (sqlite3_vtab_cursor *cur_in)
{
	idlist_vtab_cursor *cursor = (idlist_vtab_cursor *)cur_in;

	cursor->pos++;
	while (!idlist_vtab_eof (cur_in) &&
	       !xmms_collection_query_idlist_register_get ()[cursor->idlist]
	                                                    [cursor->pos]) {
		cursor->idlist++;
		cursor->pos = 0;
	}

	return SQLITE_OK;
}

static int
idlist_vtab_filter (sqlite3_vtab_cursor *cur_in, int index_num,
                    const char *index_str, int argc, sqlite3_value **argv)
{
	idlist_vtab_cursor *cursor = (idlist_vtab_cursor *)cur_in;

	cursor->idlist = 0;
	cursor->pos = 0;

	return SQLITE_OK;
}

static int
idlist_vtab_best_index (sqlite3_vtab *vtab,
                        sqlite3_index_info *index_info)
{
	return SQLITE_OK;
}

static const sqlite3_module idlist_vtab_module = {
	0,
	idlist_vtab_create,
	idlist_vtab_create,
	idlist_vtab_best_index,
	idlist_vtab_destroy,
	idlist_vtab_destroy,
	idlist_vtab_open,
	idlist_vtab_close,
	idlist_vtab_filter,
	idlist_vtab_next,
	idlist_vtab_eof,
	idlist_vtab_column,
	idlist_vtab_rowid,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL
};

const sqlite3_module *
xmms_collection_idlist_vtab_module_get () {
	return &idlist_vtab_module;
}

/**
 * @}
 */
