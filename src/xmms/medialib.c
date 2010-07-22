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

#include "xmms_configuration.h"
#include "xmmspriv/xmms_medialib.h"
#include "xmmspriv/xmms_xform.h"
#include "xmmspriv/xmms_utils.h"
#include "xmms/xmms_error.h"
#include "xmms/xmms_config.h"
#include "xmms/xmms_object.h"
#include "xmms/xmms_ipc.h"
#include "xmms/xmms_log.h"
#include "s4.h"

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include <glib.h>
#include <glib/gstdio.h>
#include <time.h>

/**
 * @file
 * Medialib is a metainfo cache that is searchable.
 */


static void xmms_medialib_client_remove_entry (xmms_medialib_t *medialib, gint32 entry, xmms_error_t *error);
gchar *xmms_medialib_url_encode (const gchar *path);

static void xmms_medialib_client_add_entry (xmms_medialib_t *, const gchar *, xmms_error_t *);
static void xmms_medialib_client_move_entry (xmms_medialib_t *, gint32 entry, const gchar *, xmms_error_t *);
static void xmms_medialib_client_import_path (xmms_medialib_t *medialib, const gchar *path, xmms_error_t *error);
static void xmms_medialib_client_rehash (xmms_medialib_t *medialib, gint32 id, xmms_error_t *error);
static void xmms_medialib_client_set_property_string (xmms_medialib_t *medialib, gint32 entry, const gchar *source, const gchar *key, const gchar *value, xmms_error_t *error);
static void xmms_medialib_client_set_property_int (xmms_medialib_t *medialib, gint32 entry, const gchar *source, const gchar *key, gint32 value, xmms_error_t *error);
static void xmms_medialib_client_remove_property (xmms_medialib_t *medialib, gint32 entry, const gchar *source, const gchar *key, xmms_error_t *error);
static GTree *xmms_medialib_client_get_info (xmms_medialib_t *medialib, gint32 id, xmms_error_t *err);
static gint32 xmms_medialib_client_get_id (xmms_medialib_t *medialib, const gchar *url, xmms_error_t *error);

#include "medialib_ipc.c"

/**
 *
 * @defgroup Medialib Medialib
 * @ingroup XMMSServer
 * @brief Medialib caches metadata
 *
 * Controls metadata storage.
 *
 * @{
 */

/**
 * Medialib structure
 */
struct xmms_medialib_St {
	xmms_object_t object;

	s4_t *s4;
	int32_t next_id;
	/** The current playlist */
	xmms_playlist_t *playlist;
};


/**
  * Ok, so the functions are written with reentrency in mind, but
  * we choose to have a global medialib object here. It will be
  * much easier, and I don't see the real use of multiple medialibs
  * right now. This could be changed by removing this global one
  * and altering the function callers...
  */
static xmms_medialib_t *medialib;


/**
 * If you add new sourcepreferences update this!
 */
#define SOURCE_PREF_COUNT 4

static s4_sourcepref_t *default_sp;
static const char *source_pref[] = {
	"server",
	"client/*",
	"plugin/id3v2",
	"plugin/*",
	"*",
	NULL
};

static void
xmms_medialib_destroy (xmms_object_t *object)
{
	xmms_medialib_t *mlib = (xmms_medialib_t *)object;

	s4_sourcepref_unref (default_sp);
	s4_close (mlib->s4);

	xmms_medialib_unregister_ipc_commands ();
}

#define XMMS_MEDIALIB_SOURCE_SERVER "server"
#define XMMS_MEDIALIB_SOURCE_SERVER_ID 1

static int32_t
find_highest_id (void)
{
	xmmsv_coll_t *coll = xmmsv_coll_universe ();
	xmmsv_t *fetch = xmmsv_new_dict ();
	xmmsv_t *id;
	int32_t ret;

	xmmsv_dict_set_string (fetch, "aggregate", "max");
	xmmsv_dict_set_string (fetch, "get", "id");

	id = xmms_medialib_query (NULL, coll, fetch);
	if (!xmmsv_get_int (id, &ret)) {
		ret = 0;
	}

	if (id != NULL) {
		xmmsv_unref (id);
	}

	xmmsv_unref (fetch);
	xmmsv_coll_unref (coll);

	return ret;
}

/**
 * Initialize the medialib and open the database file.
 *
 * @param playlist the current playlist pointer
 * @returns TRUE if successful and FALSE if there was a problem
 */

xmms_medialib_t *
xmms_medialib_init (xmms_playlist_t *playlist)
{
	gchar *path;
	const gchar *conf_path;
	const char *indices[] = {XMMS_MEDIALIB_ENTRY_PROPERTY_URL, XMMS_MEDIALIB_ENTRY_PROPERTY_STATUS, NULL};
	xmms_config_property_t *cp, *conv_conf, *coll_conf;

	medialib = xmms_object_new (xmms_medialib_t, xmms_medialib_destroy);
	medialib->playlist = playlist;

	xmms_medialib_register_ipc_commands (XMMS_OBJECT (medialib));

	path = XMMS_BUILD_PATH ("medialib.s4");

	cp = xmms_config_property_register ("medialib.path", path, NULL, NULL);
	conv_conf = xmms_config_property_register ("sqlite2s4.path", "sqlite2s4", NULL, NULL);
	coll_conf = xmms_config_property_register ("collection.directory",
			XMMS_BUILD_PATH ("collections"), NULL, NULL);

/*	xmms_config_property_register ("medialib.analyze_on_startup", "0", NULL, NULL);
	xmms_config_property_register ("medialib.allow_remote_fs",
	                               "0", NULL, NULL);*/

	conf_path = xmms_config_property_get_string (cp);
	medialib->s4 = s4_open (conf_path, indices, S4_VERIFY | S4_RECOVER);

	/* Could not open the S4 database */
	if (medialib->s4 == NULL) {
		int err = s4_errno ();
		/* Wrong magic number, maybe we stumbled upon an old sqlite database?
		 * We'll try to convert it
		 */
		if (err == S4E_MAGIC) {
			gchar *cmdline = g_strjoin (" ", xmms_config_property_get_string (conv_conf),
					conf_path, path, xmms_config_property_get_string (coll_conf), NULL);
			gchar *std_out, *std_err;
			gint exit_status;
			GError *error;

			XMMS_DBG ("Trying to run sqlite2s4, command line %s", cmdline);

			if (!g_spawn_command_line_sync (cmdline, &std_out, &std_err, &exit_status, &error) ||
					exit_status) {
				xmms_log_fatal ("Could not run \"%s\", try to run it manually", cmdline);
			}

			g_free (cmdline);

			medialib->s4 = s4_open (path, indices, S4_VERIFY | S4_RECOVER);

			/* Now we give up */
			if (medialib->s4 == NULL) {
				xmms_log_fatal ("Could not open the S4 database");
			} else {
				/* Move the sqlite database */
				gchar *new_path = g_strconcat (conf_path, ".obsolete", NULL);
				g_rename (conf_path, new_path);
				g_free (new_path);

				/* Update the config path */
				xmms_config_property_set_data (cp, path);
			}
		} else {
			/* TODO: Cleaner exit? */
			xmms_log_fatal ("Could not open the S4 database");
		}
	}

	g_free (path);

	default_sp = s4_sourcepref_create (source_pref);

	medialib->next_id = find_highest_id() + 1;

	return medialib;
}

static s4_val_t *
xmms_medialib_entry_property_get (xmms_medialib_entry_t id_num,
		const gchar *property)
{
	s4_val_t *ret = NULL;
	s4_val_t *ival = s4_val_new_int (id_num);

	g_return_val_if_fail (property, NULL);

	if (!strcmp (property, XMMS_MEDIALIB_ENTRY_PROPERTY_ID)) {
		ret = ival;
	} else {
		s4_fetchspec_t *fs = s4_fetchspec_create ();
		s4_fetchspec_add (fs, property, default_sp);
		s4_condition_t *cond = s4_cond_new_filter (S4_FILTER_EQUAL, "song_id", ival,
				default_sp, S4_CMP_CASELESS, S4_COND_PARENT);
		s4_resultset_t *set = s4_query (medialib->s4, fs, cond);
		s4_fetchspec_free (fs);

		if (set != NULL) {
			const s4_result_t *res = s4_resultset_get_result (set, 0, 0);
			if (res != NULL)
				ret = s4_val_copy (s4_result_get_val (res));
			s4_resultset_free (set);
		}

		s4_val_free (ival);
		s4_cond_free (cond);
	}

	return ret;
}


/**
 * Retrieve a property from an entry
 *
 * @see xmms_medialib_entry_property_get_str
 */

xmmsv_t *
xmms_medialib_entry_property_get_value (xmms_medialib_entry_t id_num,
                                        const gchar *property)
{
	xmmsv_t *ret = NULL;
	s4_val_t *prop;
	const char *s;
	int32_t i;

	prop = xmms_medialib_entry_property_get (id_num, property);
	if (prop == NULL)
		return NULL;

	if (s4_val_get_str (prop, &s))
		ret = xmmsv_new_string (s);
	else if (s4_val_get_int (prop, &i))
		ret = xmmsv_new_int (i);

	s4_val_free (prop);

	return ret;
}

/**
 * Retrieve a property from an entry.
 *
 * @param id_num Entry to query.
 * @param property The property to extract. Strings passed should
 * be defined in medialib.h
 *
 * @returns Newly allocated gchar that needs to be freed with g_free
 */

gchar *
xmms_medialib_entry_property_get_str (xmms_medialib_entry_t id_num,
                                      const gchar *property)
{
	gchar *ret = NULL;
	s4_val_t *prop;
	const char *s;
	int32_t i;

	prop = xmms_medialib_entry_property_get (id_num, property);
	if (prop == NULL)
		return NULL;

	if (s4_val_get_int (prop, &i))
		ret = g_strdup_printf ("%i", i);
	else if (s4_val_get_str (prop, &s))
		ret = g_strdup (s);

	s4_val_free (prop);

	return ret;
}

/**
 * Retrieve a property as a int from a entry.
 *
 * @param id_num Entry to query.
 * @param property The property to extract. Strings passed should
 * be defined in medialib.h
 *
 * @returns Property as integer, or -1 if it doesn't exist.
 */
gint
xmms_medialib_entry_property_get_int (xmms_medialib_entry_t id_num,
                                      const gchar *property)
{
	gint32 ret;
	s4_val_t *prop;

	prop = xmms_medialib_entry_property_get (id_num, property);
	if (prop == NULL || !s4_val_get_int (prop, &ret))
		ret = -1;

	s4_val_free (prop);

	return ret;
}

static gboolean
xmms_medialib_entry_property_set_source (xmms_medialib_entry_t id_num,
		const char *key, s4_val_t *new_prop,
		const gchar *source)
{
	s4_val_t *id_val = s4_val_new_int (id_num);
	const char *sources[2] = {source, NULL};
	s4_sourcepref_t *sp = s4_sourcepref_create (sources);
	s4_fetchspec_t *fs = s4_fetchspec_create ();
	s4_fetchspec_add (fs, key, sp);
	s4_condition_t *cond = s4_cond_new_filter (S4_FILTER_EQUAL, "song_id",
			id_val, sp, S4_CMP_CASELESS, S4_COND_PARENT);
	s4_resultset_t *set = s4_query (medialib->s4, fs, cond);

	if (set != NULL) {
		const s4_result_t *res = s4_resultset_get_result (set, 0, 0);
		if (res != NULL)
			s4_del (medialib->s4, "song_id", id_val, key, s4_result_get_val (res), source);

		s4_resultset_free (set);
	}

	s4_add (medialib->s4, "song_id", id_val, key, new_prop, source);

	s4_fetchspec_free (fs);
	s4_sourcepref_unref (sp);
	s4_cond_free (cond);
	s4_val_free (id_val);

	return TRUE;
}

/**
 * Set a entry property to a new value, overwriting the old value.
 *
 * @param entry Entry to alter.
 * @param property The property to extract. Strings passed should
 * be defined in medialib.h
 * @param value gint with the new value, will be copied in to the medialib
 *
 * @returns TRUE on success and FALSE on failure.
 */
gboolean
xmms_medialib_entry_property_set_int (xmms_medialib_entry_t entry,
                                      const gchar *property, gint value)
{
	return xmms_medialib_entry_property_set_int_source (entry,
	                                                    property, value,
	                                                    "server");
}


gboolean
xmms_medialib_entry_property_set_int_source (xmms_medialib_entry_t id_num,
                                             const gchar *property, gint value,
                                             const gchar *source)
{
	gboolean ret;
	s4_val_t *prop;

	g_return_val_if_fail (property, FALSE);

	prop = s4_val_new_int (value);
	ret = xmms_medialib_entry_property_set_source (id_num, property, prop, source);

	s4_val_free (prop);
	return ret;
}

/**
 * Set a entry property to a new value, overwriting the old value.
 *
 * @param entry Entry to alter.
 * @param property The property to extract. Strings passed should
 * be defined in medialib.h
 * @param value gchar with the new value, will be copied in to the medialib
 *
 * @returns TRUE on success and FALSE on failure.
 */
gboolean
xmms_medialib_entry_property_set_str (xmms_medialib_entry_t entry,
                                      const gchar *property, const gchar *value)
{
	return xmms_medialib_entry_property_set_str_source (entry,
	                                                    property, value,
	                                                    "server");
}


gboolean
xmms_medialib_entry_property_set_str_source (xmms_medialib_entry_t id_num,
                                             const gchar *property, const gchar *value,
                                             const gchar *source)
{
	gboolean ret;
	s4_val_t *prop;
	int ival;

	g_return_val_if_fail (property, FALSE);

	if (value && !g_utf8_validate (value, -1, NULL)) {
		XMMS_DBG ("OOOOOPS! Trying to set property %s to a NON UTF-8 string (%s) I will deny that!", property, value);
		return FALSE;
	}

	if (xmms_is_int (value, &ival)) {
		prop = s4_val_new_int (ival);
	} else {
		prop = s4_val_new_string (value);
	}
	ret = xmms_medialib_entry_property_set_source (id_num, property, prop, source);
	s4_val_free (prop);

	return ret;

}


/**
 * Return a fresh unused medialib id.
 */
static int32_t
xmms_medialib_get_new_id (void)
{
	return medialib->next_id++;
}


/**
 * Trigger a update signal to the client. This should be called
 * when important information in the entry has been changed and
 * should be visible to the user.
 *
 * @param entry Entry to signal a update for.
 */

void
xmms_medialib_entry_send_update (xmms_medialib_entry_t entry)
{
	xmms_object_emit_f (XMMS_OBJECT (medialib),
	                    XMMS_IPC_SIGNAL_MEDIALIB_ENTRY_UPDATE,
	                    XMMSV_TYPE_INT32, entry);
}

/**
 * Trigger an added siginal to the client. This should be
 * called when a new entry has been added to the medialib
 *
 * @param entry Entry to signal an add for.
 */
void
xmms_medialib_entry_send_added (xmms_medialib_entry_t entry)
{
	xmms_object_emit_f (XMMS_OBJECT (medialib),
	                    XMMS_IPC_SIGNAL_MEDIALIB_ENTRY_ADDED,
	                    XMMSV_TYPE_INT32, entry);
}

static void
xmms_medialib_client_remove_entry (xmms_medialib_t *medialib,
                                   gint32 entry, xmms_error_t *error)
{
	xmms_medialib_entry_remove (entry);
}

/**
 * Remove a medialib entry from the database
 *
 * @param id_num Entry to remove
 */

void
xmms_medialib_entry_remove (xmms_medialib_entry_t id_num)
{
	s4_fetchspec_t *fs;
	s4_condition_t *cond;
	s4_resultset_t *set;
	s4_val_t *id_val = s4_val_new_int (id_num);
	int i;

	fs = s4_fetchspec_create ();
	s4_fetchspec_add (fs, NULL, NULL);
	cond = s4_cond_new_filter (S4_FILTER_EQUAL, "song_id", id_val,
			default_sp, S4_CMP_CASELESS, S4_COND_PARENT);

	set = s4_query (medialib->s4, fs, cond);

	s4_cond_free (cond);
	s4_fetchspec_free (fs);

	if (set == NULL)
		return;

	for (i = 0; i < s4_resultset_get_rowcount (set); i++) {
		const s4_result_t *res = s4_resultset_get_result (set, i, 0);

		for (; res != NULL; res = s4_result_next (res)) {
			const char *src = s4_result_get_src (res);

			if (src != NULL)
				s4_del (medialib->s4, "song_id", id_val, s4_result_get_key (res),
						s4_result_get_val (res), src);
		}
	}

	s4_resultset_free (set);
	s4_val_free (id_val);

	/** @todo safe ? */
	xmms_playlist_remove_by_entry (medialib->playlist, id_num);
}

static xmms_medialib_entry_t xmms_medialib_entry_new_insert (guint32 id, const char *url, xmms_error_t *error);

static void
process_file (const gchar *playlist,
              gint32 pos,
              const gchar *path,
              xmms_error_t *error)
{
	xmms_medialib_entry_t entry;

	entry = xmms_medialib_entry_new_encoded (path, error);

	if (entry && playlist != NULL) {
		if (pos >= 0) {
			xmms_playlist_insert_entry (medialib->playlist,
			                            playlist, pos, entry, error);
		} else {
			xmms_playlist_add_entry (medialib->playlist,
			                         playlist, entry, error);
		}
	}
}

static gint
cmp_val (gconstpointer a, gconstpointer b)
{
	xmmsv_t *v1, *v2;
	const gchar *s1, *s2;
	v1 = (xmmsv_t *) a;
	v2 = (xmmsv_t *) b;
	if (xmmsv_get_type (v1) != XMMSV_TYPE_DICT)
		return 0;
	if (xmmsv_get_type (v2) != XMMSV_TYPE_DICT)
		return 0;

	xmmsv_dict_entry_get_string (v1, "path", &s1);
	xmmsv_dict_entry_get_string (v2, "path", &s2);

	return strcmp (s1, s2);
}

/* code ported over from CLI's "radd" command. */
/* note that the returned file list is reverse-sorted! */
static gboolean
process_dir (const gchar *directory,
             GList **ret,
             xmms_error_t *error)
{
	GList *list;

	list = xmms_xform_browse (directory, error);
	if (!list) {
		return FALSE;
	}

	list = g_list_sort (list, cmp_val);

	while (list) {
		xmmsv_t *val = list->data;
		const gchar *str;
		gint isdir;

		xmmsv_dict_entry_get_string (val, "path", &str);
		xmmsv_dict_entry_get_int (val, "isdir", &isdir);

		if (isdir == 1) {
			process_dir (str, ret, error);
		} else {
			*ret = g_list_prepend (*ret, g_strdup (str));
		}

		xmmsv_unref (val);
		list = g_list_delete_link (list, list);
	}

	return TRUE;
}

void
xmms_medialib_entry_cleanup (xmms_medialib_entry_t id_num)
{
	s4_fetchspec_t *fs;
	s4_condition_t *cond;
	s4_resultset_t *set;
	s4_val_t *id_val = s4_val_new_int (id_num);
	int i;

	fs = s4_fetchspec_create ();
	s4_fetchspec_add (fs, NULL, NULL);
	cond = s4_cond_new_filter (S4_FILTER_EQUAL, "song_id", id_val,
			default_sp, S4_CMP_CASELESS, S4_COND_PARENT);

	set = s4_query (medialib->s4, fs, cond);

	s4_cond_free (cond);
	s4_fetchspec_free (fs);

	if (set == NULL)
		return;

	for (i = 0; i < s4_resultset_get_rowcount (set); i++) {
		const s4_result_t *res = s4_resultset_get_result (set, i, 0);

		for (; res != NULL; res = s4_result_next (res)) {
			const char *src = s4_result_get_src (res);
			const char *key = s4_result_get_key (res);
			if (src != NULL && strcmp (XMMS_MEDIALIB_SOURCE_SERVER, src) == 0) {
				if (strcmp (key, XMMS_MEDIALIB_ENTRY_PROPERTY_URL) &&
						strcmp (key, XMMS_MEDIALIB_ENTRY_PROPERTY_ADDED) &&
						strcmp (key, XMMS_MEDIALIB_ENTRY_PROPERTY_STATUS) &&
						strcmp (key, XMMS_MEDIALIB_ENTRY_PROPERTY_LMOD) &&
						strcmp (key, XMMS_MEDIALIB_ENTRY_PROPERTY_LASTSTARTED) &&
						strcmp (key, XMMS_MEDIALIB_ENTRY_PROPERTY_TYPE))
					s4_del (medialib->s4, "song_id", id_val, key, s4_result_get_val (res), src);
			} else if (src != NULL
					&& strncmp (src, "plugin/", 7) == 0
					&& strcmp (src, "plugin/playlist") != 0) {
				s4_del (medialib->s4, "song_id", id_val, key, s4_result_get_val (res), src);
			}
		}
	}

	s4_resultset_free (set);
	s4_val_free (id_val);
}

static void
xmms_medialib_client_rehash (xmms_medialib_t *medialib, gint32 id, xmms_error_t *error)
{
	xmms_mediainfo_reader_t *mr;

	if (id) {
		xmms_medialib_entry_status_set (id, XMMS_MEDIALIB_ENTRY_STATUS_REHASH);
	} else {
		s4_fetchspec_t *fs;
		s4_condition_t *cond;
		s4_resultset_t *set;
		s4_val_t *ival = s4_val_new_int (XMMS_MEDIALIB_ENTRY_STATUS_OK);
		int i;

		fs = s4_fetchspec_create ();
		s4_fetchspec_add (fs, "song_id", default_sp);
		cond = s4_cond_new_filter (S4_FILTER_EQUAL, XMMS_MEDIALIB_ENTRY_PROPERTY_STATUS,
				ival, default_sp, S4_CMP_CASELESS, 0);

		set = s4_query (medialib->s4, fs, cond);

		s4_cond_free (cond);
		s4_fetchspec_free (fs);
		s4_val_free (ival);

		if (set == NULL)
			return;

		for (i = 0; i < s4_resultset_get_rowcount (set); i++) {
			const s4_result_t *res = s4_resultset_get_result (set, i, 0);

			for (; res != NULL; res = s4_result_next (res)) {
				s4_val_get_int (s4_result_get_val (res), &id);
				xmms_medialib_entry_status_set (id, XMMS_MEDIALIB_ENTRY_STATUS_REHASH);
			}
		}

		s4_resultset_free (set);
	}

	mr = xmms_playlist_mediainfo_reader_get (medialib->playlist);
	xmms_mediainfo_reader_wakeup (mr);

}

/* Recursively add entries under the given path to the medialib,
 * optionally adding them to a playlist if the playlist argument is
 * not NULL.
 */
void
xmms_medialib_add_recursive (xmms_medialib_t *medialib, const gchar *playlist,
                             const gchar *path, xmms_error_t *error)
{
	/* Just called insert with negative pos to append */
	xmms_medialib_insert_recursive (medialib, playlist, -1, path, error);
}

/* Recursively adding entries under the given path to the medialib,
 * optionally insert them into a playlist at a given position if the
 * playlist argument is not NULL. If the position is negative, entries
 * are appended to the playlist.
 */
void
xmms_medialib_insert_recursive (xmms_medialib_t *medialib, const gchar *playlist,
                                gint32 pos, const gchar *path,
                                xmms_error_t *error)
{
	GList *first, *list = NULL, *n;

	g_return_if_fail (medialib);
	g_return_if_fail (path);

	/* Allocate our first list node manually here. The following call
	 * to process_dir() will prepend all other nodes, so afterwards
	 * "first" will point to the last node of the list... see below.
	 */
	first = list = g_list_alloc ();

	process_dir (path, &list, error);

	XMMS_DBG ("taking the transaction!");

	/* We now want to iterate the list in the order in which the nodes
	 * were added, ie in reverse order. Thankfully we stored a pointer
	 * to the last node in the list before, which saves us an expensive
	 * g_list_last() call now. Increase pos each time to retain order.
	 */
	for (n = first->prev; n; n = g_list_previous (n)) {
		process_file (playlist, pos, n->data, error);
		if (pos >= 0)
			pos++;
		g_free (n->data);
	}

	g_list_free (list);

	XMMS_DBG ("and we are done!");
}

static void
xmms_medialib_client_import_path (xmms_medialib_t *medialib, const gchar *path,
                                  xmms_error_t *error)
{
	xmms_medialib_add_recursive (medialib, NULL, path, error);
}

static xmms_medialib_entry_t
xmms_medialib_entry_new_insert (guint32 id,
                                const char *url,
                                xmms_error_t *error)
{
	xmms_mediainfo_reader_t *mr;

	if (!xmms_medialib_entry_property_set_str (id,
			XMMS_MEDIALIB_ENTRY_PROPERTY_URL, url))
		return 0;

	xmms_medialib_entry_property_set_str (id,
			XMMS_MEDIALIB_ENTRY_PROPERTY_TYPE, "song");
	xmms_medialib_entry_status_set (id, XMMS_MEDIALIB_ENTRY_STATUS_NEW);
	mr = xmms_playlist_mediainfo_reader_get (medialib->playlist);
	xmms_mediainfo_reader_wakeup (mr);

	return 1;

}

/**
 * @internal
 */
xmms_medialib_entry_t
xmms_medialib_entry_new_encoded (const char *url, xmms_error_t *error)
{
	guint ret = 0;

	g_return_val_if_fail (url, 0);

	ret = xmms_medialib_client_get_id (medialib, url, error);

	if (ret == 0) {
		ret = xmms_medialib_get_new_id();

		if (!xmms_medialib_entry_new_insert (ret, url, error))
			return 0;
	}

	xmms_medialib_entry_send_added (ret);
	return ret;

}

/**
 * Welcome to a function that should be called something else.
 * Returns a entry for a URL, if the URL is already in the medialib
 * the current entry will be returned otherwise a new one will be
 * created and returned.
 *
 * @todo rename to something better?
 *
 * @param url URL to add/retrieve from the medialib
 * @param error If an error occurs, it will be stored in there.
 *
 * @returns Entry mapped to the URL
 */
xmms_medialib_entry_t
xmms_medialib_entry_new (const char *url, xmms_error_t *error)
{
	gchar *enc_url;
	xmms_medialib_entry_t res;

	enc_url = xmms_medialib_url_encode (url);
	if (!enc_url)
		return 0;

	res = xmms_medialib_entry_new_encoded (enc_url, error);

	g_free (enc_url);

	return res;
}

gint32
xmms_medialib_client_get_id (xmms_medialib_t *medialib, const gchar *url,
                             xmms_error_t *error)
{
	gint32 id = 0;
	s4_fetchspec_t *fs = s4_fetchspec_create ();
	s4_fetchspec_add (fs, "song_id", default_sp);
	s4_val_t *url_val = s4_val_new_string (url);
	s4_condition_t *cond = s4_cond_new_filter (S4_FILTER_EQUAL,
			XMMS_MEDIALIB_ENTRY_PROPERTY_URL, url_val, default_sp, S4_CMP_CASELESS, 0);
	s4_resultset_t *set = s4_query (medialib->s4, fs, cond);
	s4_fetchspec_free (fs);

	if (set != NULL) {
		const s4_result_t *res = s4_resultset_get_result (set, 0, 0);
		if (res != NULL)
			s4_val_get_int (s4_result_get_val (res), &id);

		s4_resultset_free (set);
	}

	s4_cond_free (cond);
	s4_val_free (url_val);

	return id;
}

static void
xmms_medialib_tree_add_tuple (GTree *tree, const char *key,
                              const char *source, xmmsv_t *value)
{
	xmmsv_t *keytreeval;

	if (key == NULL || source == NULL || value == NULL)
		return;

	/* Find (or insert) subtree matching the prop key */
	keytreeval = (xmmsv_t *) g_tree_lookup (tree, key);
	if (!keytreeval) {
		keytreeval = xmmsv_new_dict ();
		g_tree_insert (tree, g_strdup (key), keytreeval);
	}

	/* Replace (or insert) value matching the prop source */
	xmmsv_dict_set (keytreeval, source, value);
}

/**
 * Convert a entry and all properties to a key-source-value tree that
 * could be feed to the client or somewhere else in the daemon.
 *
 * @param session The medialib session to be used for the transaction.
 * @param entry Entry to convert.
 *
 * @returns Newly allocated tree with newly allocated strings
 * make sure to free them all.
 */

static GTree *
xmms_medialib_entry_to_tree (xmms_medialib_entry_t id_num)
{
	GTree *ret;
	xmmsv_t *v_entry;
	s4_fetchspec_t *fs;
	s4_condition_t *cond;
	s4_resultset_t *set;
	int i;

	g_return_val_if_fail (id_num, NULL);

	if (!xmms_medialib_check_id (id_num)) {
		return NULL;
	}

	fs = s4_fetchspec_create ();
	s4_fetchspec_add (fs, NULL, NULL);
	cond = s4_cond_new_filter (S4_FILTER_EQUAL, "song_id", s4_val_new_int (id_num),
			default_sp, S4_CMP_CASELESS, S4_COND_PARENT);

	set = s4_query (medialib->s4, fs, cond);

	s4_cond_free (cond);
	s4_fetchspec_free (fs);

	if (set == NULL)
		return NULL;

	ret = g_tree_new_full ((GCompareDataFunc) strcmp, NULL, g_free,
	                       (GDestroyNotify) xmmsv_unref);

	for (i = 0; i < s4_resultset_get_rowcount (set); i++) {
		const s4_result_t *res = s4_resultset_get_result (set, 0, 0);

		while (res != NULL) {
			const s4_val_t *val = s4_result_get_val (res);
			int32_t i;
			const char *s;

			if (s4_val_get_str (val, &s)) {
				v_entry = xmmsv_new_string (s);
			} else if (s4_val_get_int (val, &i)) {
				v_entry = xmmsv_new_int (i);
			}

			xmms_medialib_tree_add_tuple (ret, s4_result_get_key (res), s4_result_get_src (res), v_entry);
			xmmsv_unref (v_entry);

			res = s4_result_next (res);
		}

	}

	s4_resultset_free (set);
	v_entry = xmmsv_new_int (id_num);
	xmms_medialib_tree_add_tuple (ret, "id", "server", v_entry);
	xmmsv_unref (v_entry);

	return ret;
}

static GTree *
xmms_medialib_client_get_info (xmms_medialib_t *medialib, gint32 id,
                               xmms_error_t *err)
{
	GTree *ret = NULL;

	if (!id) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No such entry, 0");
	} else {
		ret = xmms_medialib_entry_to_tree (id);

		if (!ret) {
			xmms_error_set (err, XMMS_ERROR_NOENT,
			                "Could not retrieve info for that entry!");
		}
	}

	return ret;
}

/**
 * Add a entry to the medialib. Calls #xmms_medialib_entry_new and then
 * wakes up the mediainfo_reader in order to resolve the metadata.
 *
 * @param medialib Medialib pointer
 * @param url URL to add
 * @param error In case of error this will be filled.
 */

static void
xmms_medialib_client_add_entry (xmms_medialib_t *medialib, const gchar *url,
                                xmms_error_t *error)
{
	xmms_medialib_entry_t entry;

	g_return_if_fail (medialib);
	g_return_if_fail (url);

	entry = xmms_medialib_entry_new_encoded (url, error);

}

/**
 * Changes the URL of an entry in the medialib.
 *
 * @param medialib Medialib pointer
 * @param entry entry to modify
 * @param url URL to change to
 * @param error In case of error this will be filled.
 */
static void
xmms_medialib_client_move_entry (xmms_medialib_t *medialib, gint32 entry,
                                 const gchar *url, xmms_error_t *error)
{
	const gchar *key = XMMS_MEDIALIB_ENTRY_PROPERTY_URL;
	gchar *enc_url;

	enc_url = xmms_medialib_url_encode (url);

	xmms_medialib_entry_property_set_str_source (entry, key, enc_url,
	                                             "server");

	g_free (enc_url);

	xmms_medialib_entry_send_update (entry);
}

static void
xmms_medialib_client_set_property_string (xmms_medialib_t *medialib,
                                          gint32 entry, const gchar *source,
                                          const gchar *key, const gchar *value,
                                          xmms_error_t *error)
{
	if (g_ascii_strcasecmp (source, "server") == 0) {
		xmms_error_set (error, XMMS_ERROR_GENERIC,
		                "Can't write to source server!");
		return;
	}

	xmms_medialib_entry_property_set_str_source (entry, key, value,
	                                             source);
	xmms_medialib_entry_send_update (entry);
}

static void
xmms_medialib_client_set_property_int (xmms_medialib_t *medialib, gint32 entry,
                                       const gchar *source, const gchar *key,
                                       gint32 value, xmms_error_t *error)
{
	if (g_ascii_strcasecmp (source, "server") == 0) {
		xmms_error_set (error, XMMS_ERROR_GENERIC,
		                "Can't write to source server!");
		return;
	}

	xmms_medialib_entry_property_set_int_source (entry, key, value,
	                                             source);
	xmms_medialib_entry_send_update (entry);
}

void
xmms_medialib_property_remove (xmms_medialib_t *medialib, guint32 id_num,
                               const gchar *source, const gchar *key,
                               xmms_error_t *error)
{
	s4_val_t *id_val = s4_val_new_int (id_num);
	const char *sources[2] = {source, NULL};
	s4_sourcepref_t *sp = s4_sourcepref_create (sources);
	s4_fetchspec_t *fs = s4_fetchspec_create ();
	s4_fetchspec_add (fs, key, sp);
	s4_condition_t *cond = s4_cond_new_filter (S4_FILTER_EQUAL, "song_id",
			id_val, sp, S4_CMP_CASELESS, S4_COND_PARENT);
	s4_resultset_t *set = s4_query (medialib->s4, fs, cond);
	s4_fetchspec_free (fs);

	if (set != NULL) {
		const s4_result_t *res = s4_resultset_get_result (set, 0, 0);
		if (res != NULL)
			s4_del (medialib->s4, "song_id", id_val, key, s4_result_get_val (res), source);

		s4_resultset_free (set);
	}

	s4_val_free (id_val);
	s4_sourcepref_unref (sp);
	s4_cond_free (cond);

	xmms_medialib_entry_send_update (id_num);
}

static void
xmms_medialib_client_remove_property (xmms_medialib_t *medialib, gint32 entry,
                                      const gchar *source, const gchar *key,
                                      xmms_error_t *error)
{
	if (g_ascii_strcasecmp (source, "server") == 0) {
		xmms_error_set (error, XMMS_ERROR_GENERIC,
		                "Can't remove properties set by the server!");
		return;
	}

	return xmms_medialib_property_remove (medialib, entry, source, key, error);
}


/** @} */

/**
 * @internal
 */

gboolean
xmms_medialib_check_id (xmms_medialib_entry_t id)
{
	xmmsv_t *val = xmms_medialib_entry_property_get_value (id,
			XMMS_MEDIALIB_ENTRY_PROPERTY_URL);

	if (val == NULL)
		return FALSE;

	xmmsv_unref (val);

	return TRUE;
}


/**
 * @internal
 * Get the next unresolved entry. Used by the mediainfo reader..
 */

xmms_medialib_entry_t
xmms_medialib_entry_not_resolved_get (void)
{
	gint32 ret = 0;
	s4_fetchspec_t *fs = s4_fetchspec_create ();
	s4_fetchspec_add (fs, "song_id", default_sp);
	s4_val_t *val = s4_val_new_int (XMMS_MEDIALIB_ENTRY_STATUS_NEW);
	s4_condition_t *cond = s4_cond_new_filter (S4_FILTER_EQUAL,
			XMMS_MEDIALIB_ENTRY_PROPERTY_STATUS, val, default_sp, S4_CMP_CASELESS, 0);
	s4_resultset_t *set = s4_query (medialib->s4, fs, cond);
	s4_fetchspec_free (fs);

	if (set != NULL) {
		const s4_result_t *res = s4_resultset_get_result (set, 0, 0);

		if (res != NULL)
			s4_val_get_int (s4_result_get_val (res), &ret);

		s4_resultset_free (set);
	}

	s4_cond_free (cond);
	s4_val_free (val);

	return ret;
}

guint
xmms_medialib_num_not_resolved (void)
{
	gint ret = 0;
	s4_fetchspec_t *fs = s4_fetchspec_create ();
	s4_fetchspec_add (fs, "song_id", default_sp);
	s4_val_t *val = s4_val_new_int (XMMS_MEDIALIB_ENTRY_STATUS_NEW);
	s4_condition_t *cond = s4_cond_new_filter (S4_FILTER_EQUAL,
			XMMS_MEDIALIB_ENTRY_PROPERTY_STATUS, val, default_sp, S4_CMP_CASELESS, 0);
	s4_resultset_t *set = s4_query (medialib->s4, fs, cond);
	s4_fetchspec_free (fs);

	if (set != NULL) {
		ret = s4_resultset_get_rowcount (set);

		s4_resultset_free (set);
	}

	s4_cond_free (cond);
	s4_val_free (val);

	return ret;
}


gboolean
xmms_medialib_decode_url (char *url)
{
	int i = 0, j = 0;

	g_return_val_if_fail (url, TRUE);

	while (url[i]) {
		unsigned char chr = url[i++];

		if (chr == '+') {
			url[j++] = ' ';
		} else if (chr == '%') {
			char ts[3];
			char *t;

			ts[0] = url[i++];
			if (!ts[0])
				return FALSE;
			ts[1] = url[i++];
			if (!ts[1])
				return FALSE;
			ts[2] = '\0';

			url[j++] = strtoul (ts, &t, 16);
			if (t != &ts[2])
				return FALSE;
		} else {
			url[j++] = chr;
		}
	}

	url[j] = '\0';

	return TRUE;
}


#define GOODCHAR(a) ((((a) >= 'a') && ((a) <= 'z')) || \
                     (((a) >= 'A') && ((a) <= 'Z')) || \
                     (((a) >= '0') && ((a) <= '9')) || \
                     ((a) == ':') || \
                     ((a) == '/') || \
                     ((a) == '-') || \
                     ((a) == '.') || \
                     ((a) == '_'))

/* we don't share code here with medialib because we want to use g_malloc :( */
gchar *
xmms_medialib_url_encode (const gchar *path)
{
	static gchar hex[16] = "0123456789abcdef";
	gchar *res;
	int i = 0, j = 0;

	res = g_malloc (strlen (path) * 3 + 1);
	if (!res)
		return NULL;

	while (path[i]) {
		guchar chr = path[i++];
		if (GOODCHAR (chr)) {
			res[j++] = chr;
		} else if (chr == ' ') {
			res[j++] = '+';
		} else {
			res[j++] = '%';
			res[j++] = hex[((chr & 0xf0) >> 4)];
			res[j++] = hex[(chr & 0x0f)];
		}
	}

	res[j] = '\0';

	return res;
}

typedef struct {
	s4_fetchspec_t *fs;
	GHashTable *ft;
} fetch_info_t;

static int
fetchinfo_add_key (fetch_info_t *info, void *object, const char *key, s4_sourcepref_t *sp)
{
	int index;

	if (key != NULL && strcmp (key, "id") == 0)
		return 0;

	GHashTable *obj_table = g_hash_table_lookup (info->ft, object);

	if (key == NULL)
		key = "__NULL__";

	if (obj_table == NULL) {
		obj_table = g_hash_table_new (g_str_hash, g_str_equal);
		g_hash_table_insert (info->ft, object, obj_table);
	}

	if ((index = GPOINTER_TO_INT (g_hash_table_lookup (obj_table, key))) == 0) {
		index = s4_fetchspec_size (info->fs);
		g_hash_table_insert (obj_table, (void*)key, GINT_TO_POINTER (index));
		s4_fetchspec_add (info->fs, key, sp);
	}

	return index;
}

static int
fetchinfo_get_index (fetch_info_t *info, void *object, const char *key)
{
	GHashTable *obj_table = g_hash_table_lookup (info->ft, object);
	if (key == NULL)
		key = "__NULL__";
	return GPOINTER_TO_INT (g_hash_table_lookup (obj_table, key));
}

/* A filter for idlists. Checks if the value given (id number)
 * is in the hash table
 */
static int idlist_filter (s4_val_t *value, s4_condition_t *cond)
{
	int32_t ival;
	GHashTable *id_table = s4_cond_get_funcdata (cond);

	if (!s4_val_get_int (value, &ival))
		return 1;

	return g_hash_table_lookup (id_table, GINT_TO_POINTER (ival)) == NULL;
}

/* A token filter. Checks if the passed value contains a token */
static int token_filter (s4_val_t *value, s4_condition_t *cond)
{
	const char *token = s4_cond_get_funcdata (cond);
	const char *s;
	int32_t i;
	s4_cmp_mode_t mode = s4_cond_get_cmp_mode (cond);

	if ((mode == S4_CMP_CASELESS && s4_val_get_casefolded_str (value, &s)) ||
			s4_val_get_str (value, &s)) {
		while (*s) {
			/* Skip whitespaces */
			for (; isspace (*s); s++);

			/* Compare token */
			for (i = 0; *s && *s == token[i] && token[i] != '*'; i++, s++);

			/* Check if it matched */
			if (token[i] == '*' || (!token[i] && (isspace (*s) || !*s))) {
				return 0;
			}

			/* Eat the rest of the token */
			for (; *s && !isspace (*s); s++);
		}
	} else if (s4_val_get_int (value, &i)) {
		char *end;
		int32_t j = strtol (token, &end, 10);

		if (end != token) {
			/* If the token is just a number we have to have an exact match */
			if (*end == '\0' && j == i) {
				return 0;
			/* If the character after the last digit is a star we have to
			 * shift the number until it has the right number of digits
			 */
			} else if (*end == '*') {
				for (; i > j; i /= 10);
				if (i == j) {
					return 0;
				}
			}
		}
	}

	return 1;
}

/**
 * Creates a new resultset where the order is the same as in the idlist
 *
 * @param set The resultset to sort. It will be freed by this function
 * @param id_pos The position of the "id" column
 * @param idlist The idlist to order by
 * @return A new set with the same order as the idlist
 */
static s4_resultset_t *
xmms_medialib_result_sort_idlist (s4_resultset_t *set, xmmsv_t *idlist)
{
	int i;
	const s4_resultrow_t *row;
	const s4_result_t *result;
	int32_t ival;

	if (set == NULL)
		return NULL;

	GHashTable *row_table = g_hash_table_new (NULL, NULL);
	s4_resultset_t *ret = s4_resultset_create (s4_resultset_get_colcount (set));

	for (i = 0; s4_resultset_get_row (set, i, &row); i++) {
		if (s4_resultrow_get_col (row, 0, &result)
				&& s4_val_get_int (s4_result_get_val (result), &ival)) {
			g_hash_table_insert (row_table, GINT_TO_POINTER (ival), (void*)row);
		}
	}

	for (i = 0; xmmsv_list_get_int (idlist, i, &ival); i++) {
		row = g_hash_table_lookup (row_table, GINT_TO_POINTER (ival));
		if (row != NULL)
			s4_resultset_add_row (ret, row);
	}

	g_hash_table_destroy (row_table);
	s4_resultset_free (set);
	return ret;
}

/**
 * Sorts a resultset
 *
 * @param set The resultset to sort
 * @param fetch The fetch-list used when set was created
 * @param order A list with orderings. An ordering can be a string
 * telling which column to sort by (prefixed by '-' to sort ascending)
 * or a list of integers (an idlist).
 * @return The set (or a new set) with the correct ordering
 */
static s4_resultset_t *
xmms_medialib_result_sort (s4_resultset_t *set, fetch_info_t *fetch_info, xmmsv_t *order)
{
	int *s4_order = malloc (sizeof (int) * (xmmsv_list_get_size (order) + 1));
	int i, j, stop;
	xmmsv_t *val;
	const char *str;

	/* Find the first idlist-order operand */
	for (i = 0; xmmsv_list_get (order, i, &val); i++) {
		if (xmmsv_is_type (val, XMMSV_TYPE_LIST)) {
			set = xmms_medialib_result_sort_idlist (set, val);
			break;
		}
	}
	/* We will only order by the operands before the idlist */
	stop = i;

	for (i = 0, j = 0; i < stop; i++) {
		if (xmmsv_list_get_string (order, i, &str)) {
			int neg = (*str == '-')?1:0;
			str += neg;

			if (strcmp (str, "random") == 0) {
				if (i == 0) {
					s4_resultset_shuffle (set);
				}
				break;
			} else if (strcmp (str, "id") == 0) {
				s4_order[j] = 1;
			} else {
				s4_order[j] = fetchinfo_get_index (fetch_info, NULL, str) + 1;
			}

			if (neg) {
				s4_order[j] = -s4_order[j];
			}
			j++;
		}
	}
	s4_order[j] = 0;

	if (j > 0)
		s4_resultset_sort (set, s4_order);

	free (s4_order);
	return set;
}

/* Check if a collection is the universe
 * TODO: Move it to the xmmstypes lib?
 */
static int is_universe (xmmsv_coll_t *coll)
{
	char *target_name;
	int ret = 0;

	switch (xmmsv_coll_get_type (coll)) {
	case XMMS_COLLECTION_TYPE_UNIVERSE:
		ret = 1;
		break;
	case XMMS_COLLECTION_TYPE_REFERENCE:
		if (xmmsv_coll_attribute_get (coll, "reference", &target_name)
				&& strcmp (target_name, "All Media") == 0)
			ret = 1;
		break;
	default:
		break;
	}

	return ret;
}

/* Returns non-zero if the collection has an ordering, 0 otherwise */
static int has_order (xmms_coll_dag_t *dag, xmmsv_coll_t *coll)
{
	char *ref, *ns;
	xmmsv_t *operands = xmmsv_coll_operands_get (coll);
	xmmsv_coll_t *c;
	int i;

	switch (xmmsv_coll_get_type (coll)) {
		/* Intersection is orderded if the first operand is ordeed */
	case XMMS_COLLECTION_TYPE_INTERSECTION:
		xmmsv_list_get_coll (operands, 0, &c);
		return has_order (dag, c);

		/* Union is ordered if all operands are ordered (concat) */
	case XMMS_COLLECTION_TYPE_UNION:
		for (i = 0; xmmsv_list_get_coll (operands, i, &c); i++) {
			if (!has_order (dag, c))
				return 0;
		}

		/* These are always ordered */
	case XMMS_COLLECTION_TYPE_IDLIST:
	case XMMS_COLLECTION_TYPE_ORDER:
	case XMMS_COLLECTION_TYPE_LIMIT:
		return 1;

	case XMMS_COLLECTION_TYPE_REFERENCE:
		if (!is_universe (coll)) {
			xmmsv_coll_attribute_get (coll, "reference", &ref);
			xmmsv_coll_attribute_get (coll, "namespace", &ns);
			c = xmms_collection_get_pointer (dag, ref, xmms_collection_get_namespace_id (ns));
			return has_order (dag, c);
		}
	case XMMS_COLLECTION_TYPE_COMPLEMENT:
	case XMMS_COLLECTION_TYPE_UNIVERSE:
	case XMMS_COLLECTION_TYPE_MEDIASET:
	case XMMS_COLLECTION_TYPE_FILTER:
		break;
	}

	return 0;
}

/**
 * Queries the medialib. Big and ugly function
 * TODO: Make it smaller and prettier
 *
 * @param dag The collection DAG
 * @param coll The collection to query for
 * @param fetch A list of keys to fetch
 * @param order A list of orderings. If an operand adds an ordering
 * on the result it appends it to this list
 * @param cond A pointer to where the condition to use when querying will be stored
 * @param return_result Specifies if we should return a result (non-zero)
 * or just the condition (0).
 * @return An S4 resultset, or NULL if return_result equals 0
 */
static s4_resultset_t*
xmms_medialib_query_recurs (xmms_coll_dag_t *dag, xmmsv_coll_t *coll, fetch_info_t *fetch,
		xmmsv_t *order, s4_condition_t **cond, int return_result)
{
	s4_resultset_t *res = NULL;
	s4_val_t *sval = NULL;
	char *key, *val;
	xmmsv_coll_t *c;
	xmmsv_t *operands = xmmsv_coll_operands_get (coll);
	GList *list = NULL;
	int i;

	switch (xmmsv_coll_get_type (coll)) {
		case XMMS_COLLECTION_TYPE_REFERENCE:
			if (!is_universe (coll)) {
				xmmsv_coll_attribute_get (coll, "reference", &key);
				xmmsv_coll_attribute_get (coll, "namespace", &val);
				c = xmms_collection_get_pointer (dag, key,
						xmms_collection_get_namespace_id (val));
				res = xmms_medialib_query_recurs (dag, c, fetch, order,
						cond, return_result);
				break;
			}
			/* Fall through */
		case XMMS_COLLECTION_TYPE_UNIVERSE:
			sval = s4_val_new_string ("song");
			*cond = s4_cond_new_filter (S4_FILTER_EQUAL, "type",
					sval, default_sp, S4_CMP_CASELESS, 0);
			s4_val_free (sval);
			break;

		case XMMS_COLLECTION_TYPE_UNION:
		{
			int concat = has_order (dag, coll) && order != NULL;
			xmmsv_t *id_list = concat?xmmsv_new_list ():NULL;
			GHashTable *id_table;
			s4_resultset_t *set;

			if (concat) {
				id_table = g_hash_table_new (NULL, NULL);
				if (return_result) {
					res = s4_resultset_create (s4_fetchspec_size (fetch->fs));
				}
			}

			for (i = 0; xmmsv_list_get_coll (operands, i, &c); i++) {
				s4_condition_t *op_cond;

				/* If this is a concatenation we have to do a query for every operand */
				if (concat) {
					xmmsv_t *child_order = xmmsv_new_list ();
					const s4_resultrow_t *row;
					const s4_result_t *result;
					int j;

					/* Query the operand and sort it */
					set = xmms_medialib_query_recurs (dag, c, fetch, child_order, &op_cond, 1);
					set = xmms_medialib_result_sort (set, fetch, child_order);

					s4_cond_free (op_cond);

					/* Append the IDs to the id_list */
					for (j = 0; s4_resultset_get_row (set, j, &row); j++) {
						int32_t ival;

						if (s4_resultrow_get_col (row, 0, &result)
								&& s4_val_get_int (s4_result_get_val (result), &ival)) {
							xmmsv_list_append_int (id_list, ival);
							g_hash_table_insert (id_table, GINT_TO_POINTER (ival), GINT_TO_POINTER (1));
						}

						/* If we're returning a result we have to add the row to the result */
						if (return_result) {
							s4_resultset_add_row (res, row);
						}
					}

					s4_resultset_free (set);

					xmmsv_unref (child_order);
				} else { /* If this is not a concat, just a simple union,
							we make a list of the conditions for the operands */
					xmms_medialib_query_recurs (dag, c, fetch, NULL, &op_cond, 0);
					list = g_list_prepend (list, op_cond);
				}
			}

			if (concat) {
				xmmsv_list_append (order, id_list);
				xmmsv_unref (id_list);
				*cond = s4_cond_new_custom_filter (idlist_filter, id_table,
						(free_func_t)g_hash_table_destroy, "song_id", default_sp, 0, S4_COND_PARENT);
			} else {
				*cond = s4_cond_new_combiner (S4_COMBINE_OR, list);
			}
			break;
		}

		case XMMS_COLLECTION_TYPE_INTERSECTION:
			for (i = 0; xmmsv_list_get_coll (operands, i, &c); i++) {
				s4_condition_t *op_cond;
				if (i == 0) {
					/* We keep the ordering of the first operand */
					xmms_medialib_query_recurs (dag, c, fetch, order, &op_cond, 0);
				} else {
					xmms_medialib_query_recurs (dag, c, fetch, NULL, &op_cond, 0);
				}
				list = g_list_prepend (list, op_cond);
			}
			*cond = s4_cond_new_combiner (S4_COMBINE_AND, list);
			break;

		case XMMS_COLLECTION_TYPE_COMPLEMENT:
			if (xmmsv_list_get_coll (operands, 0, &c)) {
				s4_condition_t *op;
				xmms_medialib_query_recurs (dag, c, fetch, NULL, &op, 0);
				list = g_list_prepend (list, op);
			}
			*cond = s4_cond_new_combiner (S4_COMBINE_NOT, list);
			break;

		case XMMS_COLLECTION_TYPE_FILTER:
		{
			s4_filter_type_t type;
			s4_sourcepref_t *sp = default_sp;
			s4_cmp_mode_t cmp_mode;
			int32_t ival;
			int flags = 0;
			int not = 0;
			int token = 0;
			const char *token_value;

			xmmsv_coll_attribute_get (coll, "field", &key);
			if (strcmp (key, "id") == 0) {
				key = (char*)"song_id";
				flags = S4_COND_PARENT;
			}

			if (!xmmsv_coll_attribute_get (coll, "operation", &val) || strcmp (val, "=") == 0) {
				type = S4_FILTER_EQUAL;
			} else if (strcmp (val, "<") == 0) {
				type = S4_FILTER_SMALLER;
			} else if (strcmp (val, ">") == 0) {
				type = S4_FILTER_GREATER;
			} else if (strcmp (val, "match") == 0) {
				type = S4_FILTER_MATCH;
			} else if (strcmp (val, "has") == 0) {
				type = S4_FILTER_EXISTS;
			} else if (strcmp (val, "<=") == 0) {
				type = S4_FILTER_GREATER;
				not = 1;
			} else if (strcmp (val, ">=") == 0) {
				type = S4_FILTER_SMALLER;
				not = 1;
			} else if (strcmp (val, "!=") == 0) {
				type = S4_FILTER_EQUAL;
				not = 1;
			} else if (strcmp (val, "token") == 0) {
				token = 1;
			} else { /* Unknown operation, default to equal */
				type = S4_FILTER_EQUAL;
			}

			if (xmmsv_coll_attribute_get (coll, "value", &val)) {
				if (token) {
					token_value = val;
				} else {
					if (xmms_is_int (val, &ival)) {
						sval = s4_val_new_int (ival);
					} else {
						sval = s4_val_new_string (val);
					}
				}
			}

			if (!xmmsv_coll_attribute_get (coll, "collation", &val)) {
				/* For < and > we default to natcoll,
				 * so that strings will order correctly
				 * */
				if (type == S4_FILTER_SMALLER || type == S4_FILTER_GREATER) {
					cmp_mode = S4_CMP_COLLATE;
				} else {
					cmp_mode = S4_CMP_CASELESS;
				}
			} else if (strcmp (val, "NOCASE") == 0) {
				cmp_mode = S4_CMP_CASELESS;
			} else if (strcmp (val, "BINARY") == 0) {
				cmp_mode = S4_CMP_BINARY;
			} else if (strcmp (val, "NATCOLL") == 0) {
				cmp_mode = S4_CMP_COLLATE;
			} else { /* Unknown collation, fall back to caseless matching */
				cmp_mode = S4_CMP_CASELESS;
			}

			if (xmmsv_coll_attribute_get (coll, "source-preference", &val)) {
				char **prefs = g_strsplit (val, ":", -1);
				sp = s4_sourcepref_create ((const char**)prefs);
				g_strfreev (prefs);
			}

			if (token) {
				if (cmp_mode == S4_CMP_CASELESS) {
					token_value = s4_string_casefold (token_value);
				} else {
					token_value = g_strdup (token_value);
				}
				*cond = s4_cond_new_custom_filter (token_filter, (void*)token_value,
						g_free, key, sp, cmp_mode, flags);
			} else {
				*cond = s4_cond_new_filter (type, key, sval, sp, cmp_mode, flags);
			}

			if (sval != NULL)
				s4_val_free (sval);

			if (sp != default_sp) {
				s4_sourcepref_unref (sp);
			}

			xmmsv_list_get_coll (operands, 0, &c);
			if (!is_universe (c)) {
				s4_condition_t *op_cond;
				GList *list = g_list_prepend (NULL, *cond);
				xmms_medialib_query_recurs (dag, c, fetch, order, &op_cond, 0);
				list = g_list_prepend (list, op_cond);
				*cond = s4_cond_new_combiner (S4_COMBINE_AND, list);
			}

			if (not) {
				GList *list = g_list_prepend (NULL, *cond);
				*cond = s4_cond_new_combiner (S4_COMBINE_NOT, list);
			}
			break;
		}
		case XMMS_COLLECTION_TYPE_ORDER:
			if (order != NULL) {
				if (!xmmsv_coll_attribute_get (coll, "type", &key) || strcmp (key, "value") == 0) {
					xmmsv_coll_attribute_get (coll, "field", &val);

					fetchinfo_add_key (fetch, NULL, val, default_sp);

					if (!xmmsv_coll_attribute_get (coll, "order", &key) || strcmp (key, "ASC") == 0) {
						xmmsv_list_append_string (order, val);
					} else if (strcmp (key, "DESC") == 0) {
						val = g_strconcat ("-", val, NULL);
						xmmsv_list_append_string (order, val);
						g_free (val);
					}
				} else if (strcmp (key, "random") == 0) {
					/* FIXME: Do this in a way that doesn't make it
					 * impossible to sort by an actual field named "random"
					 */
					xmmsv_list_append_string (order, "random");
				}
			}
			xmmsv_list_get_coll (operands, 0, &c);
			xmms_medialib_query_recurs (dag, c, fetch, order, cond, 0);
			break;
		case XMMS_COLLECTION_TYPE_LIMIT:
		{
			unsigned int start = 0, stop = UINT_MAX;
			xmmsv_t *child_order = xmmsv_new_list ();
			const s4_resultrow_t *row;
			const s4_result_t *result;
			const s4_resultset_t *set;
			int32_t ival;
			GHashTable *id_table = g_hash_table_new (NULL, NULL);

			if (xmmsv_coll_attribute_get (coll, "start", &key)) {
				start = atoi (key);
			}
			if (xmmsv_coll_attribute_get (coll, "length", &key)) {
				stop = atoi (key) + start;
			}

			xmmsv_list_get_coll (operands, 0, &c);
			set = xmms_medialib_query_recurs (dag, c, fetch, child_order, cond, 1);
			xmmsv_unref (child_order);

			child_order = xmmsv_new_list ();

			for (; start < stop && s4_resultset_get_row (set, start, &row); start++) {
				if (return_result) {
					if (res == NULL) {
						res = s4_resultset_create (s4_resultset_get_colcount (set));
					}
					s4_resultset_add_row (res, row);
				}

				s4_resultrow_get_col (row, 0, &result);
				if (result != NULL && s4_val_get_int (s4_result_get_val (result), &ival)) {
					g_hash_table_insert (id_table, GINT_TO_POINTER (ival), GINT_TO_POINTER (1));
					xmmsv_list_append_int (child_order, ival);
				}
			}

			if (order != NULL) {
				xmmsv_list_append (order, child_order);
			}

			*cond = s4_cond_new_custom_filter (idlist_filter, id_table,
					(free_func_t)g_hash_table_destroy, "song_id", default_sp, 0, S4_COND_PARENT);
			xmmsv_unref (child_order);
			break;
		}
		case XMMS_COLLECTION_TYPE_MEDIASET:
			xmmsv_list_get_coll (operands, 0, &c);
			res = xmms_medialib_query_recurs (dag, c, fetch, NULL, cond, return_result);
			break;
		case XMMS_COLLECTION_TYPE_IDLIST:
		{
			GHashTable *id_table = g_hash_table_new (NULL, NULL);
			int i;
			int32_t ival;

			if (order != NULL) {
				xmmsv_list_append (order, xmmsv_coll_idlist_get (coll));
			}

			for (i = 0; xmmsv_coll_idlist_get_index (coll, i, &ival); i++) {
				g_hash_table_insert (id_table, GINT_TO_POINTER (ival), GINT_TO_POINTER (1));
			}

			*cond = s4_cond_new_custom_filter (idlist_filter, id_table,
					(free_func_t)g_hash_table_destroy, "song_id", default_sp, 0, S4_COND_PARENT);
			break;
		}
	}

	if (res == NULL && return_result) {
		res = s4_query (medialib->s4, fetch->fs, *cond);
		res = xmms_medialib_result_sort (res, fetch, order);
	}

	return res;
}

/**
 * Returns a random entry from a collection
 *
 * @param dag The collection DAG
 * @param coll The collection to find a random entry in
 * @return A random entry from the collection, 0 if the collection is empty
 */
xmms_medialib_entry_t
xmms_medialib_query_random_id (xmms_coll_dag_t *dag, xmmsv_coll_t *coll)
{
	xmmsv_t *fetch_spec = xmmsv_new_dict ();
	xmmsv_t *get_list = xmmsv_new_list ();
	xmmsv_t *res;
	xmms_medialib_entry_t ret;

	xmmsv_list_append_string (get_list, "id");

	xmmsv_dict_set_string (fetch_spec, "_type", "metadata");
	xmmsv_dict_set_string (fetch_spec, "aggregate", "random");
	xmmsv_dict_set (fetch_spec, "get", get_list);

	res = xmms_medialib_query (dag, coll, fetch_spec);
	xmmsv_get_int (res, &ret);

	xmmsv_unref (get_list);
	xmmsv_unref (fetch_spec);
	xmmsv_unref (res);

	return ret;
}

typedef enum {
	AGGREGATE_FIRST,
	AGGREGATE_SUM,
	AGGREGATE_MAX,
	AGGREGATE_MIN,
	AGGREGATE_LIST,
	AGGREGATE_RANDOM,
	AGGREGATE_AVG
} aggregate_function_t;

typedef struct fetch_spec_St {
	enum {
		FETCH_CLUSTER_LIST,
		FETCH_CLUSTER_DICT,
		FETCH_ORGANIZE,
		FETCH_METADATA
	} type;
	union {
		struct {
			const char **cluster_by;
			int cluster_count;
			int *cols;
			struct fetch_spec_St *data;
		} cluster;
		struct {
			enum {
				METADATA_ID,
				METADATA_KEY,
				METADATA_VALUE,
				METADATA_SOURCE,
				METADATA_NONE
			} get[5];
			int col_count;
			int *cols;
			aggregate_function_t aggr_func;
		} metadata;
		struct {
			int count;
			const char **keys;
			struct fetch_spec_St **data;
		} organize;
	} data;
} fetch_spec_t;

/* Converts an S4 result (a column) into a list of xmmsv values */
static xmmsv_t *
result_to_xmmsv (xmmsv_t *ret, int32_t id, const s4_result_t *res,
		fetch_spec_t *spec)
{
	int32_t i;
	xmmsv_t *dict = NULL; /* The dict used to find cur */
	xmmsv_t *cur = ret;
	const char *strval, *key = NULL;
	char buf[12]; /* Big enough to hold 2^32 with minus sign */
	int32_t ival;
	const s4_val_t *val;

	/* Loop through all the values the column has */
	while (res != NULL) {
		/* Loop through the list of what to get ("key", "source", ..) */
		for (i = 0; spec->data.metadata.get[i] != METADATA_NONE; i++) {
			strval = NULL;
			/* Fill strval with the correct value if it is a string
			 * or ival if it is an integer
			 */
			switch (spec->data.metadata.get[i]) {
			case METADATA_KEY:
				strval = s4_result_get_key (res);
				break;
			case METADATA_SOURCE:
				strval = s4_result_get_src (res);
				break;
			case METADATA_ID:
				ival = id;
				break;
			case METADATA_VALUE:
				val = s4_result_get_val (res);

				if (!s4_val_get_int (val, &ival)) {
					s4_val_get_str (val, &strval);
				}
				break;
			default: break; /* Silence compiler warning */
			}

			/* If this is not the last property to get we use this property
			 * as a key in a dict
			 */
			if (spec->data.metadata.get[i + 1] != METADATA_NONE) {
				/* Convert integers to strings */
				if (strval == NULL) {
					sprintf (buf, "%i", ival);
					key = buf;
				} else {
					key = strval;
				}

				/* Create a new dict if none exists */
				if (cur == NULL) {
					cur = xmmsv_new_dict ();

					if (dict == NULL) {
						ret = cur;
					} else {
						xmmsv_dict_set (dict, key, cur);
					}
				}
				dict = cur;
				if (!xmmsv_dict_get (dict, key, &cur)) {
					cur = NULL;
				}
			}
		}

		/* We do not build a list for first, but simply return the first value */
		if (spec->data.metadata.aggr_func == AGGREGATE_FIRST) {
			if (cur != NULL)
				return ret;
			if (strval != NULL) {
				cur = xmmsv_new_string (strval);
			} else {
				cur = xmmsv_new_int (ival);
			}
		} else { /* Append the new value to the end of the list */
			if (cur == NULL) {
				cur = xmmsv_new_list ();
			}
			if (strval != NULL) {
				xmmsv_list_append_string (cur, strval);
			} else {
				xmmsv_list_append_int (cur, ival);
			}
		}

		/* Update the previous dict (if there is one) */
		if (dict != NULL) {
			xmmsv_dict_set (dict, key, cur);
		} else {
			ret = cur;
		}

		res = s4_result_next (res);
	}

	return ret;
}

/* Takes a list and returns the result after the aggregate function has been
 * applied to it. The returned value has a refcount of 1.
 */
static xmmsv_t *
aggregate_list (xmmsv_t *list, aggregate_function_t aggr_func)
{
	int i, list_size = xmmsv_list_get_size (list);
	int32_t ival, min = INT32_MAX, max = INT32_MIN, sum = 0, n = 0;
	const char *str;
	xmmsv_t *val, *ret = NULL;
	GHashTable *str_table, *int_table;
	GHashTableIter iter;

	switch (aggr_func) {
	case AGGREGATE_FIRST:
		xmmsv_list_get (list, 0, &ret);
		xmmsv_ref (ret);
		break;
	case AGGREGATE_LIST:
		/* To remove duplicates from the list we create two hash tables
		 * one for integers and one for strings and insert all strings
		 * and integers into them. The hash table have no duplicates
		 * and we simply iterate over it afterwards to get back the values
		 */
		str_table = g_hash_table_new (g_str_hash, g_str_equal);
		int_table = g_hash_table_new (NULL, NULL);
		ret = xmmsv_new_list ();

		for (i = 0; xmmsv_list_get (list, i, &val); i++) {
			if (xmmsv_get_int (val, &ival)) {
				g_hash_table_insert (int_table, GINT_TO_POINTER (ival), (void*)1);
			} else if (xmmsv_get_string (val, &str)) {
				g_hash_table_insert (str_table, (void*)str, (void*)1);
			}
		}

		g_hash_table_iter_init (&iter, str_table);
		while (g_hash_table_iter_next (&iter, (void**)&str, NULL)) {
			xmmsv_list_append_string (ret, str);
		}
		g_hash_table_iter_init (&iter, int_table);
		while (g_hash_table_iter_next (&iter, (void**)&ival, NULL)) {
			xmmsv_list_append_int (ret, ival);
		}

		g_hash_table_destroy (str_table);
		g_hash_table_destroy (int_table);
		break;
	case AGGREGATE_SUM:
	case AGGREGATE_MAX:
	case AGGREGATE_MIN:
	case AGGREGATE_AVG:
		for (i = 0; i < list_size; i++) {
			int32_t ival;
			if (xmmsv_list_get_int (list, i, &ival)) {
				sum += ival;
				if (ival < min)
					min = ival;
				if (ival > max)
					max = ival;
				n++;
			}
		}
		if (n > 0) {
			switch (aggr_func) {
			case AGGREGATE_SUM: ret = xmmsv_new_int (sum); break;
			case AGGREGATE_MIN: ret = xmmsv_new_int (min); break;
			case AGGREGATE_MAX: ret = xmmsv_new_int (max); break;
			case AGGREGATE_AVG: ret = xmmsv_new_int (sum/n); break;
			default: break; /* To prevent compiler warnings */
			}
		}
		break;
	case AGGREGATE_RANDOM:
		xmmsv_list_get (list, g_random_int_range (0, n), &ret);
		xmmsv_ref (ret);
		break;
	}

	return ret;
}

/* Applies an aggregation function to an xmmsv */
static xmmsv_t *
aggregate_result (xmmsv_t *val, aggregate_function_t aggr_func)
{
	if (val == NULL) {
		return NULL;
	} else if (xmmsv_is_type (val, XMMSV_TYPE_DICT)) {
		/* If it's a dict we call this function recursively on all its values */
		xmmsv_dict_iter_t *it;
		xmmsv_get_dict_iter (val, &it);

		for (; xmmsv_dict_iter_valid (it); xmmsv_dict_iter_next (it)) {
			xmmsv_t *val;
			xmmsv_dict_iter_pair (it, NULL, &val);
			val = aggregate_result (val, aggr_func);
			xmmsv_dict_iter_set (it, val);
			xmmsv_unref (val);
		}
	} else if (xmmsv_is_type (val, XMMSV_TYPE_LIST)) {
		val = aggregate_list (val, aggr_func);
	}

	return val;
}

/* Converts an S4 resultset to an xmmsv using the fetch specification */
static xmmsv_t *
metadata_to_xmmsv (s4_resultset_t *set, fetch_spec_t *spec)
{
	xmmsv_t *ret = NULL, *tmp;
	const s4_resultrow_t *row;
	const s4_result_t *res;
	int i, j;

	/* Loop over the rows in the resultset */
	for (i = 0; s4_resultset_get_row (set, i, &row); i++) {
		int32_t id;
		s4_val_get_int (s4_result_get_val (s4_resultset_get_result (set, i, 0)), &id);
		for (j = 0; j < spec->data.metadata.col_count; j++) {
			if (s4_resultrow_get_col (row, spec->data.metadata.cols[j], &res)) {
				ret = result_to_xmmsv (ret, id, res, spec);
			}
		}
	}

	/* Optimize for first */
	if (spec->data.metadata.aggr_func == AGGREGATE_FIRST) {
		tmp = ret;
	} else {
		tmp = aggregate_result (ret, spec->data.metadata.aggr_func);
		if (tmp != ret) {
			xmmsv_unref (ret);
		}
	}

	return tmp;
}

/* Divides an S4 set into a list of smaller sets with
 * the same values for the cluster attributes
 */
static GList *
cluster_set (s4_resultset_t *set, fetch_spec_t *spec)
{
	int i, j, levels = spec->data.cluster.cluster_count;
	s4_resultset_t *cluster;
	const s4_resultrow_t *row;
	const s4_result_t *res;
	GHashTable *cur_table, *root_table;
	GList *ret = NULL;

	/* Create a hash table for hash tables if we group on more than one attribute */
	if (levels > 1) {
		root_table = g_hash_table_new_full (NULL, NULL,
				NULL, (GDestroyNotify)g_hash_table_destroy);
	} else {
		root_table = g_hash_table_new (NULL, NULL);
	}

	/* Run through all the rows in the result set.
	 * Uses a hash table to find the correct cluster to put the row in
	 */
	for (i = 0; s4_resultset_get_row (set, i, &row); i++) {
		cur_table = root_table;
		for (j = 0; j < levels; j++) {
			int col = spec->data.cluster.cols[j];
			void *value = NULL;

			/* If the value to cluster by is a string we save the pointer
			 * of it into value (this works because there are no duplicate
			 * strings in S4), otherwise we write the integer to value
			 */
			if (s4_resultrow_get_col (row, col, &res)) {
				const s4_val_t *val = s4_result_get_val (res);
				if (!s4_val_get_str (val, (const char**)&value))
					s4_val_get_int (val, (int32_t*)&value);
			}

			/* If we have more keys to cluster by after this
			 * we lookup the hash table for that key
			 */
			if (j < (levels - 1)) {
				GHashTable *next_table = g_hash_table_lookup (cur_table, value);
				if (next_table == NULL) {
					if (j < (levels - 2)) {
						next_table = g_hash_table_new_full (NULL, NULL,
								NULL, (GDestroyNotify)g_hash_table_destroy);
					} else {
						next_table = g_hash_table_new (NULL, NULL);
					}
					g_hash_table_insert (cur_table, value, next_table);
				}
				cur_table = next_table;
			} else {
				/* This is the last key to cluster by, we find the cluster
				 * and insert the row
				 */
				cluster = g_hash_table_lookup (cur_table, value);
				if (cluster == NULL) {
					cluster = s4_resultset_create (s4_resultset_get_colcount (set));
					g_hash_table_insert (cur_table, value, cluster);
					ret = g_list_prepend (ret, cluster);
				}
				s4_resultset_add_row (cluster, row);
			}
		}
	}

	g_hash_table_destroy (root_table);

	return g_list_reverse (ret);
}

/* Converts an S4 resultset into an xmmsv_t, based on the fetch specification */
static xmmsv_t *
resultset_to_xmmsv (s4_resultset_t *set, fetch_spec_t *spec)
{
	xmmsv_t *val, *tmp, *dict, *ret = NULL;
	GList *sets;
	int i;

	switch (spec->type) {
	case FETCH_METADATA:
		ret = metadata_to_xmmsv (set, spec);
		break;

	case FETCH_ORGANIZE:
		ret = xmmsv_new_dict ();

		for (i = 0; i < spec->data.organize.count; i++) {
			val = resultset_to_xmmsv (set, spec->data.organize.data[i]);
			if (val != NULL) {
				xmmsv_dict_set (ret, spec->data.organize.keys[i], val);
				xmmsv_unref (val);
			}
		}
		break;

	case FETCH_CLUSTER_LIST:
	case FETCH_CLUSTER_DICT:
		sets = cluster_set (set, spec);

		if (spec->type == FETCH_CLUSTER_LIST) {
			ret = xmmsv_new_list ();
			for (; sets != NULL; sets = g_list_delete_link (sets, sets)) {
				set = sets->data;

				val = resultset_to_xmmsv (set, spec->data.cluster.data);
				if (val != NULL) {
					xmmsv_list_append (ret, val);
					xmmsv_unref (val);
				}
				s4_resultset_free (set);
			}
		} else {
			ret = xmmsv_new_dict ();
			for (; sets != NULL; sets = g_list_delete_link (sets, sets)) {
				set = sets->data;

				val = resultset_to_xmmsv (set, spec->data.cluster.data);

				/* Insert the value in the dict(s) */
				if (val != NULL) {
					dict = ret;

					for (i = 0; i < spec->data.cluster.cluster_count; i++) {
						int col = spec->data.cluster.cols[i];
						int32_t ival;
						char buf[12];
						const char *str;
						const s4_result_t *res;

						/* Get the attributes from the first row. Since they are in
						 * the same cluster this should be the same for all the
						 * entries in the set
						 */
						if ((res = s4_resultset_get_result (set, 0, col)) != NULL) {
							const s4_val_t *s4_val = s4_result_get_val (res);
							if (!s4_val_get_str (s4_val, &str)) {
								s4_val_get_int (s4_val, &ival);
								sprintf (buf, "%i", ival);
								str = buf;
							}

							if (i < (spec->data.cluster.cluster_count - 1)) {
								if (!xmmsv_dict_get (dict, str, &tmp)) {
									tmp = xmmsv_new_dict ();
									xmmsv_dict_set (dict, str, tmp);
									xmmsv_unref (tmp);
								}
								dict = tmp;
							} else {
								xmmsv_dict_set (dict, str, val);
							}
						} else {
							/* A cluster where one of the cluster-properties
							 * is not set, we do not store it in the dict
							 */
							break;
						}
					}

					xmmsv_unref (val);
				}
				s4_resultset_free (set);
			}
		}
		break;
	}

	return ret;
}

/* Converts a fetch specification in xmmsv_t form into a fetch_spec_t structure */
static fetch_spec_t *
fetch_to_spec (xmmsv_t *fetch, fetch_info_t *info)
{
	xmmsv_t *val;
	const char *str;
	int i, id_only = 0;
	fetch_spec_t *ret = malloc (sizeof (fetch_spec_t));
	s4_sourcepref_t *sp;

	switch (xmmsv_get_type (fetch)) {
	case XMMSV_TYPE_DICT:
	{
		const char *type;
		if (!xmmsv_dict_entry_get_string (fetch, "_type", &type)) {
			type = "metadata";
		}

		if (strcmp (type, "metadata") == 0) {
			ret->type = FETCH_METADATA;

			if (xmmsv_dict_get (fetch, "get", &val)) {
				for (i = 0; i < 4 && xmmsv_list_get_string (val, i, &str); i++) {
					if (strcmp (str, "id") == 0) {
						ret->data.metadata.get[i] = METADATA_ID;
					} else if (strcmp (str, "key") == 0) {
						ret->data.metadata.get[i] = METADATA_KEY;
					} else if (strcmp (str, "value") == 0) {
						ret->data.metadata.get[i] = METADATA_VALUE;
					} else if (strcmp (str, "source") == 0) {
						ret->data.metadata.get[i] = METADATA_SOURCE;
					}
				}
				if (xmmsv_get_string (val, &str)) {
					if (strcmp (str, "id") == 0) {
						ret->data.metadata.get[i] = METADATA_ID;
					} else if (strcmp (str, "key") == 0) {
						ret->data.metadata.get[i] = METADATA_KEY;
					} else if (strcmp (str, "value") == 0) {
						ret->data.metadata.get[i] = METADATA_VALUE;
					} else if (strcmp (str, "source") == 0) {
						ret->data.metadata.get[i] = METADATA_SOURCE;
					}
					i++;
				}

				ret->data.metadata.get[i] = METADATA_NONE;
				if (i == 1 && ret->data.metadata.get[0] == METADATA_ID)
					id_only = 1;
			} else {
				ret->data.metadata.get[0] = METADATA_VALUE;
				ret->data.metadata.get[1] = METADATA_NONE;
			}

			if (xmmsv_dict_get (fetch, "source-preference", &val)) {
				const char **strs = malloc (
						sizeof (const char*) * (xmmsv_list_get_size (val) + 1));

				for (i = 0; xmmsv_list_get_string (val, i, &str); i++) {
					strs[i] = str;
				}
				strs[i] = NULL;
				sp = s4_sourcepref_create (strs);
				free (strs);
			} else {
				sp = s4_sourcepref_ref (default_sp);
			}
			if (id_only) {
				ret->data.metadata.col_count = 1;
				ret->data.metadata.cols = malloc (sizeof (int) * ret->data.metadata.col_count);
				ret->data.metadata.cols[0] = 0;
			} else if (xmmsv_dict_get (fetch, "keys", &val)) {
				ret->data.metadata.col_count = xmmsv_list_get_size (val);
				if (ret->data.metadata.col_count == -1) {
					ret->data.metadata.col_count = 1;
				}
				ret->data.metadata.cols = malloc (sizeof (int) * ret->data.metadata.col_count);
				for (i = 0; xmmsv_list_get_string (val, i, &str); i++) {
					ret->data.metadata.cols[i] =
						fetchinfo_add_key (info, fetch, str, sp);
				}
				if (xmmsv_get_string (val, &str)) {
					ret->data.metadata.cols[0] =
						fetchinfo_add_key (info, fetch, str, sp);
				}
			} else {
				ret->data.metadata.col_count = 1;
				ret->data.metadata.cols = malloc (sizeof (int) * ret->data.metadata.col_count);
				ret->data.metadata.cols[0] = fetchinfo_add_key (info, fetch, NULL, sp);
			}
			if (!xmmsv_dict_entry_get_string (fetch, "aggregate", &str)) {
				/* Default to first as the aggregation function */
				str = "first";
			}
			if (strcmp (str, "first") == 0) {
				ret->data.metadata.aggr_func = AGGREGATE_FIRST;
			} else if (strcmp (str, "sum") == 0) {
				ret->data.metadata.aggr_func = AGGREGATE_SUM;
			} else if (strcmp (str, "max") == 0) {
				ret->data.metadata.aggr_func = AGGREGATE_MAX;
			} else if (strcmp (str, "min") == 0) {
				ret->data.metadata.aggr_func = AGGREGATE_MIN;
			} else if (strcmp (str, "list") == 0) {
				ret->data.metadata.aggr_func = AGGREGATE_LIST;
			} else if (strcmp (str, "random") == 0) {
				ret->data.metadata.aggr_func = AGGREGATE_RANDOM;
			} else if (strcmp (str, "avg") == 0) {
				ret->data.metadata.aggr_func = AGGREGATE_AVG;
			} else { /* Unknown aggregation function */
				ret->data.metadata.aggr_func = AGGREGATE_FIRST;
			}

			s4_sourcepref_unref (sp);
		} else if (strcmp (type, "cluster-list") == 0
				|| strcmp (type, "cluster-dict") == 0) {
			if (xmmsv_dict_get (fetch, "cluster-by", &val)) {
				if (xmmsv_is_type (val, XMMSV_TYPE_LIST)) {
					ret->data.cluster.cluster_count = xmmsv_list_get_size (val);
					ret->data.cluster.cluster_by = malloc (
							sizeof (const char *) * ret->data.cluster.cluster_count);
					ret->data.cluster.cols = malloc (
							sizeof (int) * ret->data.cluster.cluster_count);
					for (i = 0; xmmsv_list_get_string (val, i, &str); i++) {
						ret->data.cluster.cluster_by[i] = str;
						ret->data.cluster.cols[i] = fetchinfo_add_key (info, val, str, default_sp);
					}
				} else if (xmmsv_get_string (val, &str)) {
					ret->data.cluster.cluster_count = 1;
					ret->data.cluster.cluster_by = malloc (
							sizeof (const char *) * ret->data.cluster.cluster_count);
					ret->data.cluster.cols = malloc (
							sizeof (int) * ret->data.cluster.cluster_count);
					ret->data.cluster.cluster_by[0] = str;
					ret->data.cluster.cols[0] = fetchinfo_add_key (info, val, str, default_sp);
				}
			}
			if (xmmsv_dict_get (fetch, "data", &val)) {
				ret->data.cluster.data = fetch_to_spec (val, info);
			}

			if (strcmp (type, "cluster-list") == 0) {
				ret->type = FETCH_CLUSTER_LIST;
			} else {
				ret->type = FETCH_CLUSTER_DICT;
			}
		} else if (strcmp (type, "organize") == 0) {
			xmmsv_dict_iter_t *it;
			xmmsv_get_dict_iter (fetch, &it);

			ret->type = FETCH_ORGANIZE;

			for (ret->data.organize.count = -1 /* We do not want to count "_type" */
					; xmmsv_dict_iter_valid (it)
					; ret->data.organize.count++, xmmsv_dict_iter_next (it));

			ret->data.organize.keys = malloc (sizeof (const char *) * ret->data.organize.count);
			ret->data.organize.data = malloc (sizeof (fetch_spec_t *) * ret->data.organize.count);

			for (i = 0, xmmsv_dict_iter_first (it)
					; xmmsv_dict_iter_valid (it)
					; xmmsv_dict_iter_next (it)) {
				xmmsv_dict_iter_pair (it, &str, &val);

				if (strcmp (str, "_type") != 0) {
					ret->data.organize.keys[i] = str;
					ret->data.organize.data[i] = fetch_to_spec (val, info);
					i++;
				}
			}

			xmmsv_dict_iter_explicit_destroy (it);
		}
		break;
	}
	default:
		break;
	}

	return ret;
}

static void
fetch_spec_free (fetch_spec_t *spec)
{
	int i;
	switch (spec->type) {
	case FETCH_METADATA:
		free (spec->data.metadata.cols);
		break;
	case FETCH_CLUSTER_DICT:
	case FETCH_CLUSTER_LIST:
		free (spec->data.cluster.cols);
		free (spec->data.cluster.cluster_by);
		fetch_spec_free (spec->data.cluster.data);
		break;
	case FETCH_ORGANIZE:
		for (i = 0; i < spec->data.organize.count; i++) {
			fetch_spec_free (spec->data.organize.data[i]);
		}

		free (spec->data.organize.keys);
		free (spec->data.organize.data);
		break;
	}

	free (spec);
}

/**
 * Queries the medialib and returns an xmmsv_t with the info requested
 *
 * @param dag The collection DAG
 * @param coll The collection to find
 * @param fetch Specifies what to fetch
 * @return An xmmsv_t with the structure requested in fetch
 */
xmmsv_t *
xmms_medialib_query (xmms_coll_dag_t *dag, xmmsv_coll_t *coll, xmmsv_t *fetch)
{
	s4_resultset_t *set;
	s4_condition_t *cond;
	xmmsv_t *order, *ret;
	fetch_info_t info;
	fetch_spec_t *spec;

	info.fs = s4_fetchspec_create ();
	info.ft = g_hash_table_new_full (g_direct_hash, g_direct_equal,
			NULL, (GDestroyNotify)g_hash_table_destroy);
	s4_fetchspec_add (info.fs, "song_id", default_sp);

	spec = fetch_to_spec (fetch, &info);
	order = xmmsv_new_list ();

	set = xmms_medialib_query_recurs (dag, coll, &info, order, &cond, 1);
	ret = resultset_to_xmmsv (set, spec);

	if (set != NULL)
		s4_resultset_free (set);
	s4_cond_free (cond);
	s4_fetchspec_free (info.fs);
	g_hash_table_destroy (info.ft);
	xmmsv_unref (order);
	fetch_spec_free (spec);

	return ret;
}
