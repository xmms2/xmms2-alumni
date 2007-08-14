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

#include <glib/gstdio.h>
#include "config.h"

/**
 * parse_config(), read_config() and free_config() are taken from XMMS2 cli with
 * some minor modifications.
 */

/**
 * Parse service section.
 */
static void
parse_service (service_t **service, gchar **split, gint *i)
{
	gchar *stripped;
	gchar **s;
	method_t *method;

	*service = g_new0 (service_t, 1);
	(*service)->methods = g_hash_table_new_full (g_str_hash, g_str_equal,
	                                             g_free, free_method);

	for ((*i)++; split && split[*i]; (*i)++) {
		stripped = g_strstrip (split[*i]);

		if (g_str_has_prefix (stripped, "[") &&
		    g_str_has_suffix (stripped, "]"))
			break;

		s = g_strsplit (stripped, "=", 2);
		if (s && s[0] && s[1]) {
			if (g_strcasecmp (s[0], "description") == 0)
				(*service)->desc = g_strdup (s[1]);
			else if (g_strcasecmp (s[0], "major") == 0)
				(*service)->major = g_ascii_strtoull (s[1], NULL, 10);
			else if (g_strcasecmp (s[0], "minor") == 0)
				(*service)->minor = g_ascii_strtoull (s[1], NULL, 10);
			else {
				method = g_new0 (method_t, 1);
				method->desc = g_strdup (s[1]);
				g_hash_table_insert ((*service)->methods, g_strdup (s[0]),
				                     method);
			}
		}
		g_strfreev (s);
	}

	(*i)--;
}

/**
 * Parse config file.
 */
static config_t *
parse_config (const gchar *buffer)
{
	gchar **split, **s;
	gchar *stripped;
	gint i;
	config_t *config;
	service_t *service;

	config = g_new0 (config_t, 1);
	config->services = g_hash_table_new_full (g_str_hash, g_str_equal, g_free,
	                                          free_service);

	split = g_strsplit (buffer, "\n", 0);
	for (i = 0; split && split[i]; i++) {
		stripped = g_strstrip (split[i]);

		if (g_str_has_prefix (stripped, "[") &&
		    g_str_has_suffix (stripped, "]")) {
			parse_service (&service, split, &i);
			g_hash_table_insert (config->services,
			                     g_strndup (stripped + 1, strlen (stripped) - 2),
			                     service);

			continue;
		}

		s = g_strsplit (stripped, "=", 2);
		if (s && s[0] && s[1]) {
			if (g_strcasecmp (s[0], "path") == 0)
				config->path = g_strdup (s[1]);
			else if (g_strcasecmp (s[0], "argv") == 0)
				config->argv = g_strdup (s[1]);
			else if (g_strcasecmp (s[0], "auto") == 0) {
				if (g_strcasecmp (s[1], "yes") == 0)
					config->autostart = TRUE;
				else
					config->autostart = FALSE;
			}
		}
		g_strfreev (s);
	}
	g_strfreev (split);

	return config;
}

/**
 * Write one entry into the config file.
 */
static void
write_entry (gpointer key, gpointer value, gpointer udata)
{
	FILE *fp = udata;
	gchar *entry;

	entry = g_strconcat (key, "=", value, "\n", NULL);

	fwrite (entry, strlen (entry), 1, fp);

	g_free (entry);
}

/**
 * Get the config file dir.
 */
const gchar *
config_dir (void)
{
	static gchar userconf[PATH_MAX];
	static gchar *dir = NULL;

	if (!dir) {
		xmmsc_userconfdir_get (userconf, PATH_MAX);
		dir = g_build_path (G_DIR_SEPARATOR_S, userconf,
		                    "service_clients", NULL);
	}

	return dir;
}

/**
 * Read all config files.
 */
gboolean
read_all (void)
{
	GError *err = NULL;
	GDir *dir;
	const gchar *f;
	config_t *config;

	g_return_val_if_fail (!clients, FALSE);

	clients = g_hash_table_new_full (g_str_hash, g_str_equal,
	                                 g_free, free_config);

	if (!(dir = g_dir_open (config_dir (), 0, &err))) {
		print_error ("Unable to read config dir: %s", err->message);
		return FALSE;
	}

	while ((f = g_dir_read_name (dir))) {
		if ((config = read_config (f)))
			g_hash_table_insert (clients, g_strdup (f), config);
	}

	g_dir_close (dir);

	return TRUE;
}

/**
 * Read config file.
 */
config_t *
read_config (const gchar *name)
{
	config_t *config;
	gchar *buffer, *file;
	gint read_bytes = 0;
	struct stat st;
	FILE *fp;

	file = g_build_path (G_DIR_SEPARATOR_S, config_dir (), name, NULL);

	if (!g_file_test (file, G_FILE_TEST_EXISTS)) {
		print_info ("Config file %s does not exist", file);
		return NULL;
	} else {
		fp = fopen (file, "r");
		if (!fp) {
			print_info ("Could not open configfile %s", file);
			g_free (file);
			return NULL;
		}
		g_free (file);

		if (fstat (fileno (fp), &st) == -1) {
			print_info ("fstat");
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
 * Write contents to config file.
 */
gboolean
write_config (const gchar *name, GHashTable *contents)
{
	FILE *fp;
	gchar *file;
	gboolean ret = TRUE;

	file = g_build_path (G_DIR_SEPARATOR_S, config_dir (), name, NULL);

	if (!g_file_test (file, G_FILE_TEST_EXISTS)) {
		print_info ("Config file %s does not exist", file);
		ret = FALSE;
	} else {
		fp = fopen (file, "w");
		if (fp) {
			g_hash_table_foreach (contents, write_entry, fp);
			fclose (fp);
		} else {
			print_info ("Could not write to config file: %s\n"
			            "Make sure you have write permission to the file.",
			            file);
			ret = FALSE;
		}
	}

	g_free (file);

	return ret;
}

/**
 * Create the config file if it doesn't exist, and then write contents into the
 * file.
 */
gboolean
create_config (const gchar *name, const gchar *contents)
{
	const gchar *dir;
	gchar *file;
	FILE *fp;

	dir = config_dir ();
	file = g_build_path (G_DIR_SEPARATOR_S, dir, name, NULL);
	g_mkdir_with_parents (dir, 0755);

	fp = fopen (file, "w+");
	if (fp) {
		fwrite (contents, strlen (contents), 1, fp);
		fclose (fp);
	} else {
		print_info ("Could not create config file: %s\n"
		            "Make sure you have write permissions to that location.",
		            file);
		return FALSE;
	}

	return TRUE;
}

/**
 * Remove config file.
 */
gboolean
remove_config (const gchar *name)
{
	gchar *file;

	file = g_build_path (G_DIR_SEPARATOR_S, config_dir (), name, NULL);

	if (g_unlink (file))
		return FALSE;

	return TRUE;
}

/**
 * Free the config structure created by parse_config().
 */
void
free_config (gpointer v)
{
	config_t *config = v;

	if (config) {
		g_free (config->path);
		g_free (config->argv);
		if (config->pid)
			g_spawn_close_pid (config->pid);
		g_hash_table_destroy (config->services);
		g_free (config);
	}
}

/**
 * Free the service structure created by parse_service().
 */
void
free_service (gpointer v)
{
	service_t *service = v;

	if (service) {
		g_free (service->desc);
		g_hash_table_destroy (service->methods);
		g_free (service);
	}
}

/**
 * Free the method structure created by parse_service().
 */
void
free_method (gpointer v)
{
	method_t *method = v;

	if (method) {
		g_free (method->desc);
		g_free (method);
	}
}
