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
#include <signal.h>

#include <xmmsclient/xmmsclient.h>
#include "xmmsclient/xmmsclient-glib.h"

static xmmsc_connection_t *connection;
static int vis;

void
quit (void *data)
{
	xmmsc_visualisation_shutdown (connection, vis);
	puts ("\nbye cruel world!");
	exit (EXIT_SUCCESS);
}

void
quit2 (int sig)
{
	xmmsc_visualisation_shutdown (connection, vis);
	puts("");
	exit (EXIT_SUCCESS);
}


static char* config[] = {
	"type", "pcm",
	"stereo", "1",
	//"freq", "44100",
	//"pcm_samplecount", "512",
	NULL
};

short data[2];

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

	xmmsc_disconnect_callback_set (connection, quit, NULL);
	signal(SIGINT, quit2);

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
	res = xmmsc_visualisation_properties_set (connection, vis, config);
	xmmsc_result_wait (res);
	if (xmmsc_result_iserror (res)) {
		puts (xmmsc_result_get_error (res));
		exit(EXIT_FAILURE);
	}
	xmmsc_result_unref (res);

	res = xmmsc_visualisation_start (connection, vis);
	xmmsc_result_wait (res);
	if (xmmsc_result_iserror (res)) {
		puts (xmmsc_result_get_error (res));
		exit(EXIT_FAILURE);
	}
	xmmsc_result_unref (res);

//	xmmsc_mainloop_gmain_init (connection);
//	g_main_loop_run (mainloop);

	char buf[38+38];
	int i, w;
	while (xmmsc_visualisation_chunk_get (connection, vis, &data)) {
		w = (int)(((double)data[0] / (double)SHRT_MAX) * 36.0);
		for (i = 0; i < 36 - w; ++i) {
			switch (buf[i]) {
				case '#':
					buf[i] = '*';
					break;
				case '*':
					buf[i] = '+';
					break;
				case '+':
					buf[i] = '-';
					break;
				default:
					buf[i] = ' ';
			}
		}
		for (i = 36 - w; i < 36; ++i)
			buf[i] = '#';

		buf[36] = buf[37] = ' ';

		w = 38 + (int)(((double)data[0] / (double)SHRT_MAX) * 36.0);
		for (i = 38; i < w; ++i)
			buf[i] = '#';
		for (i = w; i < 38+38; ++i) {
			switch (buf[i]) {
				case '#':
					buf[i] = '*';
					break;
				case '*':
					buf[i] = '+';
					break;
				case '+':
					buf[i] = '-';
					break;
				default:
					buf[i] = ' ';
			}
		}
		fputs(buf, stdout);
		putchar('\r');
		fflush(stdout);
	}
	puts ("");

	res = xmmsc_visualisation_shutdown (connection, vis);
	xmmsc_result_wait (res);
	if (xmmsc_result_iserror (res)) {
		puts (xmmsc_result_get_error (res));
		exit (EXIT_FAILURE);
	}
	xmmsc_result_unref (res);

	if (connection) {
		xmmsc_unref (connection);
	}

	return 0;
}
