/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2006 XMMS2 Team
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

#include "common.h"

gint
res_has_key (xmmsc_result_t *res, const gchar *key)
{
	return xmmsc_result_get_dict_entry_type (res, key) != XMMSC_RESULT_VALUE_TYPE_NONE;
}


gchar *
format_url (gchar *item)
{
	gchar *url, rpath[PATH_MAX], *p;

	p = strchr (item, ':');
	if (!(p && p[1] == '/' && p[2] == '/')) {
		/* OK, so this is NOT an valid URL */

		if (!realpath (item, rpath)) {
			return NULL;
		}

		if (!g_file_test (rpath, G_FILE_TEST_IS_REGULAR)) {
			return NULL;
		}

		url = g_strdup_printf ("file://%s", rpath);
	} else {
		url = g_strdup_printf ("%s", item);
	}

	return url;
}


void
print_info (const gchar *fmt, ...)
{
	gchar buf[8096];
	va_list ap;
	
	va_start (ap, fmt);
	g_vsnprintf (buf, 8096, fmt, ap);
	va_end (ap);

	g_print ("%s\n", buf);
}


void
print_error (const gchar *fmt, ...)
{
	gchar buf[1024];
	va_list ap;
	
	va_start (ap, fmt);
	g_vsnprintf (buf, 1024, fmt, ap);
	va_end (ap);

	g_print ("ERROR: %s\n", buf);

	exit (-1);
}


void
print_hash (const void *key, xmmsc_result_value_type_t type, 
			const void *value, void *udata)
{
	if (type == XMMSC_RESULT_VALUE_TYPE_STRING) {
		print_info ("%s = %s", key, value);
	} else {
		print_info ("%s = %d", key, XPOINTER_TO_INT (value));
	}
}


void
print_entry (const void *key, xmmsc_result_value_type_t type, 
			 const void *value, const gchar *source, void *udata)
{
	if (type == XMMSC_RESULT_VALUE_TYPE_STRING) {
		print_info ("[%s] %s = %s", source, key, value);
	} else {
		print_info ("[%s] %s = %d", source, key, XPOINTER_TO_INT (value));
	}
}

gint
find_terminal_width() {
	gint columns = 0;
	struct winsize ws;
	char *colstr, *endptr;

	if (!ioctl(STDIN_FILENO, TIOCGWINSZ, &ws)) {
		columns = ws.ws_col;
	} 
	else {
		colstr = getenv("COLUMNS");
		if(colstr != NULL) {
			columns = strtol(colstr, &endptr, 10);
		}
	}

	/* Default to 80 columns */
	if(columns <= 0) {
		columns = 80;
	}

	return columns;
}

void
print_padded_string (gint columns, gchar padchar, gboolean padright, const gchar *fmt, ...)
{
	gchar buf[1024];
	gchar *padstring;

	va_list ap;
	
	va_start (ap, fmt);
	g_vsnprintf (buf, 1024, fmt, ap);
	va_end (ap);

	padstring = g_strnfill (columns - g_utf8_strlen(buf, -1), padchar);

	if (padright) {
		print_info ("%s%s", buf, padstring);
	}
	else {
		print_info ("%s%s", padstring, buf);
	}

	g_free(padstring);
}

gchar*
make_justified_columns_format (gint columns, const char type_first)
{
	int wd_id, wd_artist, wd_album, wd_title;
	gchar *buf = g_new (gchar, 128);

	/* count separators */
	columns -= 8;

	wd_id     = 5;
	wd_artist = (columns - wd_id) / 4;
	wd_album  = (columns - wd_id - wd_artist) / 2;
	wd_title  = (columns - wd_id - wd_artist - wd_album);

	g_snprintf (buf, 128, "%%-%d.%d%c| %%-%d.%ds | %%-%d.%ds | %%-%d.%ds",
	            wd_id, wd_id, type_first,
	            wd_artist, wd_artist, wd_album, wd_album, wd_title, wd_title);

	return buf;
}

void
format_pretty_list (xmmsc_connection_t *conn, GList *list)
{
	guint count = 0;
	GList *n;
	gint columns;
	gchar *format_header, *format_rows;

	columns = find_terminal_width ();
	format_header = make_justified_columns_format (columns, 's');
	format_rows   = make_justified_columns_format (columns, 'd');

	print_padded_string (columns, '-', TRUE, "-[Result]-");
	print_info (format_header, "Id", "Artist", "Album", "Title");

	for (n = list; n; n = g_list_next (n)) {
		gchar *title;
		xmmsc_result_t *res;
		gint mid = XPOINTER_TO_INT (n->data);

		if (mid) {
			res = xmmsc_medialib_get_info (conn, mid);
			xmmsc_result_wait (res);
		} else {
			print_error ("Empty result!");
		}

		if (xmmsc_result_get_dict_entry_str (res, "title", &title)) {
			gchar *artist, *album;
			if (!xmmsc_result_get_dict_entry_str (res, "artist", &artist)) {
				artist = "Unknown";
			}

			if (!xmmsc_result_get_dict_entry_str (res, "album", &album)) {
				album = "Unknown";
			}

			print_info (format_rows, mid, artist, album, title);
		} else {
			gchar *url, *filename;
			xmmsc_result_get_dict_entry_str (res, "url", &url);
			if (url) {
				filename = g_path_get_basename (url);
				if (filename) {
					print_info ("%-5.5d| %s", mid, filename);
					g_free (filename);
				}
			}
		}
		count++;
		xmmsc_result_unref (res);
	}

	print_padded_string (columns, '-', FALSE, "-[Count:%6.d]-----", count);

	g_free (format_header);
	g_free (format_rows);
}
