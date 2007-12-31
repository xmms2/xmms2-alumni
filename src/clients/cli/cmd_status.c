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

#include "common.h"


/**
 * Function prototypes
 */
static void handle_current_id (xmmsc_result_t *res, void *userdata);
static void handle_playtime (xmmsc_result_t *res, void *userdata);
static void handle_mediainfo_update (xmmsc_result_t *res, void *userdata);
static void handle_status_change (xmmsc_result_t *res, void *userdata);
static void do_mediainfo (xmmsc_result_t *res, void *userdata);
static void update_display ();
static void quit (void *data);


/**
 * Globals
 */
extern gchar *statusformat;

static gboolean has_songname = FALSE;
static gboolean fetching_songname = FALSE;
static guint current_id = 0;
static guint last_dur = 0;
static gint curr_dur = 0;
static gchar songname[256];
static guint curr_status = 0;

static const gchar *status_messages[] = {
	"Stopped",
	"Playing",
	"Paused"
};

/**
 * Functions
 */
void
cmd_status (xmmsc_connection_t *conn, gint argc, gchar **argv)
{
	GMainLoop *ml;
	
	ml = g_main_loop_new (NULL, FALSE);

	has_songname = FALSE;

	/* Setup onchange signal for mediainfo */
	XMMS_CALLBACK_SET (conn, xmmsc_broadcast_playback_current_id,
	                   handle_current_id, conn);
	XMMS_CALLBACK_SET (conn, xmmsc_signal_playback_playtime,
	                   handle_playtime, NULL);
	XMMS_CALLBACK_SET (conn, xmmsc_playback_current_id,
	                   handle_current_id, conn);
	XMMS_CALLBACK_SET (conn, xmmsc_broadcast_medialib_entry_changed,
	                   handle_mediainfo_update, conn);
	XMMS_CALLBACK_SET (conn, xmmsc_broadcast_playback_status,
	                   handle_status_change, NULL);
	XMMS_CALLBACK_SET (conn, xmmsc_playback_status,
	                   handle_status_change, NULL);

	xmmsc_disconnect_callback_set (conn, quit, NULL);
	xmmsc_mainloop_gmain_init (conn);

	g_main_loop_run (ml);
}


void
cmd_current (xmmsc_connection_t *conn, gint argc, gchar **argv)
{ 
	xmmsc_result_t *res;
	gchar print_text[256];
	guint id;

	res = xmmsc_playback_current_id (conn);
	xmmsc_result_wait (res);

	if (xmmsc_result_iserror (res)) {
		print_error ("%s", xmmsc_result_get_error (res));
	}

	if (!xmmsc_result_get_uint (res, &id)) {
		print_error ("Broken resultset");
	}
	xmmsc_result_unref (res);

	res = xmmsc_medialib_get_info (conn, id);
	xmmsc_result_wait (res);

	if (xmmsc_result_iserror (res)) {
		print_error ("%s", xmmsc_result_get_error (res));
	}

	if (argc > 2) {
		xmmsc_entry_format (print_text, sizeof(print_text), argv[2], res);	
	} else {
		xmmsc_entry_format (print_text, sizeof(print_text), 
                            "${artist} - ${title}", res);
	}

	print_info ("%s", print_text);
	xmmsc_result_unref (res);
}

static void
handle_status_change (xmmsc_result_t *res, void *userdata)
{
	guint new_status;

	if (xmmsc_result_iserror (res)) {
		print_error ("%s", xmmsc_result_get_error (res));
	}

	if (!xmmsc_result_get_uint (res, &new_status)) {
		print_error ("Broken resultset");
	}

	curr_status = new_status;
	update_display ();
}

static void
handle_current_id (xmmsc_result_t *res, void *userdata)
{
	xmmsc_connection_t *conn = userdata;

	if (xmmsc_result_iserror (res)) {
		print_error ("%s", xmmsc_result_get_error (res));
	}

	if (!xmmsc_result_get_uint (res, &current_id)) {
		print_error ("Broken resultset");
	}

	if (current_id) {
		fetching_songname = TRUE;
		res = xmmsc_medialib_get_info (conn, current_id);
		xmmsc_result_notifier_set (res, do_mediainfo, NULL);
		xmmsc_result_unref (res);
	}
}


static void
handle_playtime (xmmsc_result_t *res, void *userdata)
{
	xmmsc_result_t *newres;
	guint dur;

	if (xmmsc_result_iserror (res)) {
		print_error ("%s", xmmsc_result_get_error (res));
	}
	
	if (!xmmsc_result_get_uint (res, &dur)) {
		print_error ("Broken resultset");
	}

	if (((dur / 1000) % 60) != ((last_dur / 1000) % 60)) {
		last_dur = dur;

		if (!fetching_songname)
			update_display ();

	}
	newres = xmmsc_result_restart (res);
	xmmsc_result_unref (res);
	xmmsc_result_unref (newres);
}

static void update_display ()
{
	gchar *conv;
	gsize r, w;
	GError *err = NULL;

	if (has_songname) {
		gchar buf_status[32];
		gchar buf_time[32];
		gint room, len, columns;

		columns = find_terminal_width ();

		g_snprintf (buf_status, sizeof (buf_status), "%7s: ",
		            status_messages[curr_status]);
		g_snprintf (buf_time, sizeof (buf_time), ": %02d:%02d of %02d:%02d",
		            last_dur / 60000, (last_dur / 1000) % 60, curr_dur / 60000,
		            (curr_dur / 1000) % 60);

		room = columns - strlen (buf_status) - strlen (buf_time);

		len = g_utf8_strlen (songname, -1);
		if (room >= len || room <= 4) { /* don't even try.. */
			conv =  g_locale_from_utf8 (songname, -1, &r, &w, &err);
			printf ("\r%s%s%s", buf_status, conv, buf_time);
		} else {
			gint t = g_utf8_offset_to_pointer (songname, room - 3) - songname;

			conv =  g_locale_from_utf8 (songname, t, &r, &w, &err);
			printf ("\r%s%s...%s", buf_status, conv, buf_time);
		}
		g_free (conv);

		fflush (stdout);
	}
}

static void
handle_mediainfo_update (xmmsc_result_t *res, void *userdata)
{
	guint id;
	xmmsc_connection_t *conn = userdata;

	if (xmmsc_result_iserror (res)) {
		print_error ("%s", xmmsc_result_get_error (res));
	}

	if (!xmmsc_result_get_uint (res, &id)) {
		print_error ("Broken resultset");
	}

	if (id == current_id) {
		res = xmmsc_medialib_get_info (conn, current_id);
		xmmsc_result_notifier_set (res, do_mediainfo, NULL);
		xmmsc_result_unref (res);
	}
}


static void
do_mediainfo (xmmsc_result_t *res, void *userdata)
{
	if (xmmsc_result_iserror (res)) {
		print_error ("%s", xmmsc_result_get_error (res));
	}

	print_info ("");
	if (res_has_key (res, "channel") && res_has_key (res, "title")) {
		xmmsc_entry_format (songname, sizeof (songname),
		                    "[stream] ${title}", res);
		has_songname = TRUE;
	} else if (res_has_key (res, "channel")) {
		xmmsc_entry_format (songname, sizeof (songname), "${title}", res);
		has_songname = TRUE;
	} else if (!res_has_key (res, "title")) {
		const gchar *url;

		if (xmmsc_result_get_dict_entry_string (res, "url", &url)) {
			gchar *filename = g_path_get_basename (url);

			if (filename) {
				g_snprintf (songname, sizeof (songname), "%s", filename);
				g_free (filename);
				has_songname = TRUE;
			}
		}
	} else {
		xmmsc_entry_format (songname, sizeof (songname),
		                    statusformat, res);
		has_songname = TRUE;
	}

	if (xmmsc_result_get_dict_entry_int (res, "duration", &curr_dur)) {
		/* rounding */
		curr_dur += 500;
	} else {
		curr_dur = 0;
	}

	xmmsc_result_unref (res);

	fetching_songname = FALSE;
}


static void
quit (void *data)
{
	print_info ("\nbye cruel world!");
	exit (EXIT_SUCCESS);
}
