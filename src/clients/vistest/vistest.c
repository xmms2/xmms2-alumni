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




/** @file
 * Extremly simple example of a visualisation for xmms2
 * (should probably just be put in /usr/share/doc/xmms2-dev/examples)
 */


#include <glib.h>
#include <math.h>
#include <stdlib.h>

#include <xmmsclient/xmmsclient.h>
#include "xmmsclient/xmmsclient-glib.h"

#include "cmd_status.h"

static GMainLoop *mainloop;

static xmmsc_connection_t *connection;
static int vis;

static char* config[][2] = {
	{"test", "yeah"},
	{"no", "remorse"},
	{"blackjack", "nutten"}
};

int
main (int argc, char **argv)
{
	gchar *path;

	connection = xmmsc_init ("XMMS2-VISTEST");

	if(!connection){
		printf ("bad\n");
		return 1;
	}

	path = getenv ("XMMS_PATH");
	if (!xmmsc_connect (connection, path)){
		printf ("couldn't connect to xmms2d: %s\n",
			xmmsc_get_last_error(connection));
		return 1;
	}

	mainloop = g_main_loop_new (NULL, FALSE);
	has_songname = FALSE;

	/* Setup onchange signal for mediainfo */
	XMMS_CALLBACK_SET (connection, xmmsc_broadcast_playback_current_id,
	                   handle_current_id, connection);
	XMMS_CALLBACK_SET (connection, xmmsc_signal_playback_playtime,
	                   handle_playtime, NULL);
	XMMS_CALLBACK_SET (connection, xmmsc_playback_current_id,
	                   handle_current_id, connection);
	XMMS_CALLBACK_SET (connection, xmmsc_broadcast_medialib_entry_changed,
	                   handle_mediainfo_update, connection);
	XMMS_CALLBACK_SET (connection, xmmsc_broadcast_playback_status,
	                   handle_status_change, NULL);
	XMMS_CALLBACK_SET (connection, xmmsc_playback_status,
	                   handle_status_change, NULL);

	xmmsc_disconnect_callback_set (connection, quit, NULL);

	uint32_t version;
	xmmsc_result_t *res = xmmsc_visualisation_version (connection);
	xmmsc_result_wait (res);

	if (xmmsc_result_iserror (res)) {
		puts (xmmsc_result_get_error (res));
		exit (EXIT_FAILURE);
	} else {
		xmmsc_result_get_uint (res, &version);
		/* insert the version you need here or instead of complaining,
		   reduce your feature set to fit the version */
		if (version < 1) {
			printf ("The server only supports formats up to version %d (needed is %d)!", version, 1);
			exit (EXIT_FAILURE);
		}
	}
	xmmsc_result_unref (res);

	vis = xmmsc_visualisation_init (connection);
	printf("vis: %d\n", vis);
	res = xmmsc_visualisation_connect (connection, vis);
	xmmsc_result_wait (res);
	if (xmmsc_result_iserror (res)) {
		puts (xmmsc_result_get_error (res));
		exit(EXIT_FAILURE);
	}
	xmmsc_result_unref (res);

//	xmmsc_mainloop_gmain_init (connection);
//	g_main_loop_run (mainloop);
	int bla; scanf("%d", &bla);

	res = xmmsc_visualisation_shutdown (connection, vis);
	xmmsc_result_wait (res);
	if (xmmsc_result_iserror (res)) {
		puts (xmmsc_result_get_error (res));
		exit(EXIT_FAILURE);
	}
	xmmsc_result_unref (res);

	if (connection) {
		xmmsc_unref (connection);
	}

	return 0;
}
