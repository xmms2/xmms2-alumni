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

#include "config.h"

/**
 * parse_config(), read_config() and free_config() are taken from XMMS2 cli with
 * some minor modifications.
 */

/**
 * Parse config file.
 */
static GHashTable *
parse_config (const gchar *buffer)
{
	gchar **split;
	gint i;
	GHashTable *config;

	config = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);

	split = g_strsplit (buffer, "\n", 0);
	for (i = 0; split && split[i]; i++) {
		gchar **s;

		s = g_strsplit (split[i], "=", 2);
		if (s && s[0] && s[1]) {
			if (g_strcasecmp (s[1], "NULL") == 0) {
				g_hash_table_insert (config, g_strdup (s[0]), NULL);
			} else {
				g_hash_table_insert (config, g_strdup (s[0]), g_strdup (s[1]));
			}
		}
		g_strfreev (s);
	}
	g_strfreev (split);

	return config;
}

/**
 * Read config file.
 */
GHashTable *
read_config (const gchar *name)
{
	GHashTable *config;
	gchar *buffer, *file;
	gint read_bytes = 0;
	struct stat st;
	FILE *fp;

	gchar userconf[PATH_MAX];
	xmmsc_userconfdir_get (userconf, PATH_MAX);
	file = g_build_path (G_DIR_SEPARATOR_S, userconf,
	                     "clients", name, NULL);

	if (!g_file_test (file, G_FILE_TEST_EXISTS)) {
		print_error ("Config file %s does not exist", file);
		return NULL;
	} else {
		fp = fopen (file, "r");
		if (!fp) {
			print_error ("Could not open configfile %s", file);
			g_free (file);
			return NULL;
		}
		g_free (file);

		if (fstat (fileno (fp), &st) == -1) {
			print_error ("fstat");
			return NULL;
		}

		buffer = g_malloc0 (st.st_size + 1);

		while (read_bytes < st.st_size) {
			guint ret = fread (buffer + read_bytes,
							   st.st_size - read_bytes, 1, fp);

			if (ret == 0) {
				break;
			}

			read_bytes += ret;
			g_assert (read_bytes >= 0);
		}
		fclose (fp);

		config = parse_config (buffer);

		g_free (buffer);
	}

	return config;
}

/**
 * Create the config file if it doesn't exist, and then write contents into the
 * file.
 */
gboolean
create_config (const gchar *file, const gchar *contents)
{
	FILE *fp;

	gchar *dir = g_path_get_dirname (file);
	g_mkdir_with_parents (dir, 0755);
	g_free (dir);

	fp = fopen (file, "w+");
	if (fp) {
		fwrite (contents, strlen (contents), 1, fp);
		fclose (fp);
	} else {
		print_info ("Could not create configfile: %s\nMake sure you have write permissions to that location.", file);
		return FALSE;
	}

	return TRUE;
}

/**
 * Free the config hash table created by read_config.
 */
void
free_config (GHashTable *config)
{
	if (config)
		g_hash_table_destroy (config);
}
