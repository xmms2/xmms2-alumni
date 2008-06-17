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
#include "cmd_other.h"
#include "common.h"

void
cmd_stats (xmmsc_connection_t *conn, gint argc, gchar **argv)
{
	xmmsc_result_t *res;
	xmms_value_t *val;

	res = xmmsc_main_stats (conn);
	xmmsc_result_wait (res);
	val = xmmsc_result_get_value (res);

	if (xmms_value_iserror (val)) {
		print_error ("%s", xmms_value_get_error (val));
	}

	xmms_value_dict_foreach (val, print_hash, NULL);
	xmmsc_result_unref (res);
}


void
cmd_plugin_list (xmmsc_connection_t *conn, gint argc, gchar **argv)
{
	xmmsc_result_t *res;
	xmms_value_t *val;
	xmms_plugin_type_t type = XMMS_PLUGIN_TYPE_ALL;

	if (argc > 2) {
		if (g_ascii_strcasecmp (argv[2], "output") == 0) {
			type = XMMS_PLUGIN_TYPE_OUTPUT;
		} else if (g_ascii_strcasecmp (argv[2], "xform") == 0) {
			type = XMMS_PLUGIN_TYPE_XFORM;
		} else {
			print_error ("no such plugin type!");
		}
	}

	res = xmmsc_plugin_list (conn, type);
	xmmsc_result_wait (res);
	val = xmmsc_result_get_value (res);

	if (xmms_value_iserror (val)) {
		print_error ("%s", xmms_value_get_error (val));
	}

	while (xmms_value_list_valid (val)) {
		const gchar *shortname, *desc;

		if (xmms_value_get_dict_entry_string (val, "shortname", &shortname) &&
		    xmms_value_get_dict_entry_string (val, "description", &desc)) {
			print_info ("%s - %s", shortname, desc);
		}

		xmms_value_list_next (val);
	}
	xmmsc_result_unref (res);
}


void
cmd_quit (xmmsc_connection_t *conn, gint argc, gchar **argv)
{
	xmmsc_result_t *res;

	res = xmmsc_quit (conn);
	xmmsc_result_wait (res);

	if (xmmsc_result_iserror (res)) {
		print_error ("%s", xmmsc_result_get_error (res));
	}
	xmmsc_result_unref (res);
}

void
cmd_browse (xmmsc_connection_t *conn, gint argc, gchar **argv)
{
	xmmsc_result_t *res;
	xmms_value_t *val;

	if (argc < 3) {
		print_error ("Need to specify a URL to browse");
	}

	res = xmmsc_xform_media_browse (conn, argv[2]);
	xmmsc_result_wait (res);
	val = xmmsc_result_get_value (res);

	if (xmms_value_iserror (val)) {
		print_error ("%s", xmms_value_get_error (val));
	}

	for (;xmms_value_list_valid (val); xmms_value_list_next (val)) {
		xmms_value_type_t type;
		const gchar *r;
		gint d;

		type = xmms_value_get_dict_entry_type (val, "realpath");
		if (type != XMMS_VALUE_TYPE_NONE) {
			xmms_value_get_dict_entry_string (val, "realpath", &r);
		} else {
			xmms_value_get_dict_entry_string (val, "path", &r);
		}

		xmms_value_get_dict_entry_int (val, "isdir", &d);
		print_info ("%s%c", r, d ? '/' : ' ');
	}

	xmmsc_result_unref (res);
}
