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

#include <stdlib.h>
#include <glib.h>
#include <sqlite3.h>
#include "xmmspriv/xmms_collquery.h"
#include "xmmspriv/xmms_medialib.h"

static void
decode_url (sqlite3_context *c, int argc, sqlite3_value** argv)
{
	gchar *res;

	if (sqlite3_value_type (argv[0]) != SQLITE3_TEXT) {
		sqlite3_result_error (c,
				"First argument to xmms_implode should be a string", -1);
		return;
	}

	res = g_strdup (sqlite3_value_text (argv[0]));

	xmms_medialib_decode_url (res);

	sqlite3_result_text (c, res, -1, g_free);
}

static void
implode_step (sqlite3_context *c, int argc, sqlite3_value** argv)
{
	GString **sto;

	if (sqlite3_value_type (argv[1]) != SQLITE3_TEXT) {
		sqlite3_result_error (c,
				"Second argument to xmms_implode should be a string", -1);
		return;
	}

	sto = (GString **)sqlite3_aggregate_context (c, sizeof(GString *));

	if (!*sto) {
		*sto = g_string_new (sqlite3_value_text (argv[0]));
	} else {
		g_string_append (*sto, sqlite3_value_text (argv[1]));
		g_string_append (*sto, sqlite3_value_text (argv[0]));
	}
}

static void
implode_final (sqlite3_context *c)
{
	GString **sto;

	sto = (GString **)sqlite3_aggregate_context (c, sizeof(GString *));
	sqlite3_result_text (c, (*sto)->str, -1, g_free);
	g_string_free (*sto, FALSE);
}

void
xmms_sqlite_add_functions (sqlite3 *db) {
	sqlite3_create_function (db, "xmms_implode", 2, SQLITE_UTF8, NULL, NULL,
	                         implode_step, implode_final);
	sqlite3_create_function (db, "xmms_decode_url", 1, SQLITE_UTF8, NULL,
	                         decode_url, NULL, NULL);
}
