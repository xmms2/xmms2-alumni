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

	s4_close (mlib->s4);

	xmms_medialib_unregister_ipc_commands ();
}

#define XMMS_MEDIALIB_SOURCE_SERVER "server"
#define XMMS_MEDIALIB_SOURCE_SERVER_ID 1


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

	default_sp = s4_sourcepref_create (medialib->s4, source_pref);

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
		s4_condition_t *cond = s4_cond_new_filter (S4_FILTER_EQUAL, "song_id", ival, default_sp, S4_COND_PARENT);
		s4_resultset_t *set = s4_query (medialib->s4, fs, cond);
		s4_fetchspec_free (fs);

		if (set != NULL) {
			const s4_result_t *res = s4_resultset_get_result (set, 0, 0);
			if (res != NULL)
				ret = s4_val_copy (s4_result_get_val (res));
		}

		s4_resultset_free (set);
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
	gint32 ret = -1;
	s4_val_t *prop;

	prop = xmms_medialib_entry_property_get (id_num, property);
	if (prop == NULL || !s4_val_get_int (prop, &ret))
		return -1;

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
	s4_sourcepref_t *sp = s4_sourcepref_create (medialib->s4, sources);
	s4_fetchspec_t *fs = s4_fetchspec_create ();
	s4_fetchspec_add (fs, key, sp);
	s4_condition_t *cond = s4_cond_new_filter (S4_FILTER_EQUAL, "song_id", id_val, sp, S4_COND_PARENT);
	s4_resultset_t *set = s4_query (medialib->s4, fs, cond);

	if (set != NULL) {
		const s4_result_t *res = s4_resultset_get_result (set, 0, 0);
		if (res != NULL)
			s4_del (medialib->s4, "song_id", id_val, key, s4_result_get_val (res), source);

		s4_resultset_free (set);
	}

	s4_add (medialib->s4, "song_id", id_val, key, new_prop, source);

	s4_sourcepref_free (sp);
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
int32_t
xmms_medialib_get_new_id (void)
{
	int32_t id = xmms_medialib_entry_property_get_int (0, "new_id");

	if (id == -1)
		id = 1;

	xmms_medialib_entry_property_set_int (0, "new_id", id + 1);

	return id;
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
	s4_fetchspec_add (fs, NULL, default_sp);
	cond = s4_cond_new_filter (S4_FILTER_EQUAL, "song_id", id_val, default_sp, S4_COND_PARENT);

	set = s4_query (medialib->s4, fs, cond);

	s4_cond_free (cond);
	s4_fetchspec_free (fs);

	if (set == NULL)
		return;

	for (i = 0; i < s4_resultset_get_rowcount (set); i++) {
		const s4_result_t *res = s4_resultset_get_result (set, i, 0);

		for (; res != NULL; res = s4_result_next (res)) {
			s4_del (medialib->s4, "song_id", id_val, s4_result_get_key (res),
					s4_result_get_val (res), s4_result_get_src (res));
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
	s4_fetchspec_add (fs, NULL, default_sp);
	cond = s4_cond_new_filter (S4_FILTER_EQUAL, "song_id", id_val, default_sp, S4_COND_PARENT);

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
			} else if (src != NULL && strcmp (src, "plugin/playlist") != 0) {
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
				ival, default_sp, 0);

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
	s4_val_t *url_val = s4_val_new_string_nocopy (url);
	s4_condition_t *cond = s4_cond_new_filter (S4_FILTER_EQUAL,
			XMMS_MEDIALIB_ENTRY_PROPERTY_URL, url_val, default_sp, 0);
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
 * Convert a entry and all properties to a hashtable that
 * could be feed to the client or somewhere else in the daemon.
 *
 * @param id_num Entry to convert.
 *
 * @returns Newly allocated hashtable with newly allocated strings
 * make sure to free them all.
 */

GList *
xmms_medialib_entry_to_list (xmms_medialib_entry_t id_num)
{
	GList *ret = NULL;
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
	s4_fetchspec_add (fs, NULL, default_sp);
	cond = s4_cond_new_filter (S4_FILTER_EQUAL, "song_id", s4_val_new_int (id_num), default_sp, S4_COND_PARENT);

	set = s4_query (medialib->s4, fs, cond);

	s4_cond_free (cond);
	s4_fetchspec_free (fs);

	if (set == NULL)
		return NULL;

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

			ret = g_list_prepend (ret, xmmsv_new_string (s4_result_get_src (res)));
			ret = g_list_prepend (ret, xmmsv_new_string (s4_result_get_key (res)));
			ret = g_list_prepend (ret, v_entry);

			res = s4_result_next (res);
		}

	}

	/* Source */
	ret = g_list_prepend (ret, xmmsv_new_string ("server"));

	/* Key */
	ret = g_list_prepend (ret, xmmsv_new_string ("id"));

	/* Value */
	ret = g_list_prepend (ret, xmmsv_new_int (id_num));

	return g_list_reverse (ret);
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
	s4_fetchspec_add (fs, NULL, default_sp);
	cond = s4_cond_new_filter (S4_FILTER_EQUAL, "song_id", s4_val_new_int (id_num), default_sp, S4_COND_PARENT);

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

/* Legacy, still used by collections. */
GList *
xmms_medialib_info_list (xmms_medialib_t *medialib, guint32 id, xmms_error_t *err)
{
	GList *ret = NULL;

	if (!id) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No such entry, 0");
	} else {
		ret = xmms_medialib_entry_to_list (id);

		if (!ret) {
			xmms_error_set (err, XMMS_ERROR_NOENT,
			                "Could not retrieve info for that entry!");
		}
	}

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
	s4_sourcepref_t *sp = s4_sourcepref_create (medialib->s4, sources);
	s4_fetchspec_t *fs = s4_fetchspec_create ();
	s4_fetchspec_add (fs, key, sp);
	s4_condition_t *cond = s4_cond_new_filter (S4_FILTER_EQUAL, "song_id", id_val, sp, S4_COND_PARENT);
	s4_resultset_t *set = s4_query (medialib->s4, fs, cond);
	s4_fetchspec_free (fs);

	if (set != NULL) {
		const s4_result_t *res = s4_resultset_get_result (set, 0, 0);
		if (res != NULL)
			s4_del (medialib->s4, "song_id", id_val, key, s4_result_get_val (res), source);

		s4_resultset_free (set);
	}

	s4_val_free (id_val);
	s4_sourcepref_free (sp);
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
			XMMS_MEDIALIB_ENTRY_PROPERTY_STATUS, val, default_sp, 0);
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
			XMMS_MEDIALIB_ENTRY_PROPERTY_STATUS, val, default_sp, 0);
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


s4_condition_t *xmms_coll_to_cond (xmms_coll_dag_t *dag, xmmsv_coll_t *coll, s4_sourcepref_t *sp);
GList*
xmms_medialib_query_ids (xmms_coll_dag_t *dag, xmmsv_coll_t *coll)
{
	GList *ret = NULL;
	int i;
	int32_t ival;


	/* If the top collection is an idlist we simply extract the ids
	 * directly, otherwise S4 will turn our idlist into a set.
	 * This is only important for top level idlist since otherwise
	 * they're input to some other collection that outputs a set anyway
	 */
	if (xmmsv_coll_get_type (coll) == XMMS_COLLECTION_TYPE_IDLIST) {
		int i, size = xmmsv_coll_idlist_get_size (coll);
		uint32_t *ids = xmmsv_coll_get_idlist (coll);
		for (i = 0; i < size; i++) {
			ret = g_list_prepend (ret, xmmsv_new_int (ids[i]));
		}
	} else {
		s4_fetchspec_t *fs = s4_fetchspec_create ();
		s4_fetchspec_add (fs, "song_id", default_sp);
		s4_condition_t *cond = xmms_coll_to_cond (dag, coll, default_sp);
		s4_resultset_t *set = s4_query (medialib->s4, fs, cond);

		s4_fetchspec_free (fs);
		s4_cond_free (cond);

		if (set == NULL)
			return NULL;

		for (i = 0; i < s4_resultset_get_rowcount (set); i++) {
			const s4_result_t *res = s4_resultset_get_result (set, i, 0);

			if (res != NULL && s4_val_get_int (s4_result_get_val (res), &ival)) {
				ret = g_list_prepend (ret, xmmsv_new_int (ival));
			}
		}

		s4_resultset_free (set);
	}

	return g_list_reverse (ret);
}

gint
xmms_medialib_query_random_id (xmms_coll_dag_t *dag, xmmsv_coll_t *coll)
{
	s4_fetchspec_t *fs = s4_fetchspec_create ();
	s4_fetchspec_add (fs, "song_id", default_sp);
	s4_condition_t *cond = xmms_coll_to_cond (dag, coll, default_sp);
	s4_resultset_t *set = s4_query (medialib->s4, fs, cond);
	int size = (set == NULL)?0:s4_resultset_get_rowcount (set);
	int32_t ret = 0;

	s4_fetchspec_free (fs);
	s4_cond_free (cond);

	if (size) {
		const s4_result_t *res = NULL;
		while (res == NULL)
			res = s4_resultset_get_result (set, g_random_int_range (0, size), 0);

		s4_val_get_int (s4_result_get_val (res), &ret);
	}

	if (set != NULL)
		s4_resultset_free (set);

	return ret;
}

GList*
xmms_medialib_query_infos (xmms_coll_dag_t *dag, xmmsv_coll_t *coll, xmmsv_t *fetch)
{
	GList *ret = NULL;
	xmmsv_t *dict, *prop;
	int i,j,size;
	const char *p, *s;
	int32_t ival;
	s4_fetchspec_t *fs = s4_fetchspec_create ();

	size = xmmsv_list_get_size (fetch);

	for (i = 0; i < size; i++) {
		xmmsv_list_get (fetch, i, &prop);

		if (!xmmsv_get_string (prop, &p))
			p = NULL;
		else {
			if (!strcmp (p, "id"))
				p = "song_id";

			s4_fetchspec_add (fs, p, default_sp);
		}
	}

	s4_condition_t *cond = xmms_coll_to_cond (dag, coll, default_sp);
	s4_resultset_t *set = s4_query (medialib->s4, fs, cond);

	s4_fetchspec_free (fs);
	s4_cond_free (cond);

	for (i = 0; i < s4_resultset_get_rowcount (set); i++) {
		for (dict = NULL, j = 0; j < size; j++) {
			const s4_result_t *res = s4_resultset_get_result (set, i, j);
			xmmsv_t *val = NULL;
			if (res == NULL)
				continue;

			if (s4_val_get_str (s4_result_get_val (res), &s)) {
				val = xmmsv_new_string (s);
			} else if (s4_val_get_int (s4_result_get_val (res), &ival)) {
				val = xmmsv_new_int (ival);
			}

			if (val == NULL)
				continue;

			if (dict == NULL) {
				dict = xmmsv_new_dict();
//				xmmsv_get_dict_iter (dict, &it);
			}

/*			xmmsv_dict_iter_find (it, fs[i]);

			xmmsv_list_iter_insert (*(xmmsv_list_iter_t**)it, keyvals[i]);
			xmmsv_list_iter_next (*(xmmsv_list_iter_t**)it);
			xmmsv_list_iter_insert (*(xmmsv_list_iter_t**)it, val);
			*/
			const char *key = s4_result_get_key (res);
			if (!strcmp (key, "song_id"))
				key = "id";
			xmmsv_dict_set (dict, key, val);

			xmmsv_unref (val);
		}

		if (dict != NULL)
			ret = g_list_prepend (ret, dict);
	}

	s4_resultset_free (set);

	return ret;
}
