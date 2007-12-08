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
 * Extremly simple example of a visualization for xmms2
 * (should probably just be put in /usr/share/doc/xmms2-dev/examples)
 */


#include <glib.h>
#include <math.h>
#include <stdlib.h>
#include <signal.h>

#include <xmmsclient/xmmsclient.h>
#include <xmmsclient/xmmsclient-glib.h>

static GMainLoop *mainloop;
static xmmsc_connection_t *connection;
static int vis;
static int height;

xmmsc_visualization_properties_t config = {
	"type", "spectrum",
	"stereo", "0",
	NULL
};

short data[256];

void draw () {
	int i, z, val;
	int base = SHRT_MAX / height;

	printf ("\e[1m");
	for (z = height; z >= 1; z -= 2) {
		for (i = 0; i < 90; i += 1) {
			/*val = (data[i] + data[i+1]) / 2;*/
			val = data[i];
			if (val > (z * base)) {
				putchar ('|');
			} else if (val > ((z-1) * base)) {
				putchar ('.');
			} else {
				putchar (' ');
			}
		}
		putchar ('\n');
	}
	printf ("\e[m\e[%dA", height/2);
	fflush (stdout);
}

gboolean draw_gtk (gpointer stuff)
{
	int ret = xmmsc_visualization_chunk_get (connection, vis, data, 0, 0);
	if (ret == 256) {
		draw ();
	}
	return (ret >= 0);
}

void shutdown_gtk (gpointer stuff)
{
	g_main_loop_quit (mainloop);
}

void
quit (int signum)
{
	printf ("\e[%dB", height/2);

	xmmsc_visualization_shutdown (connection, vis);
	if (connection) {
		xmmsc_unref (connection);
	}

	exit (EXIT_SUCCESS);
}

void
setup_signal ()
{
	struct sigaction siga;
	siga.sa_handler = quit;
	sigemptyset (&siga.sa_mask);
	siga.sa_flags = SA_RESTART;
	sigaction(SIGINT, &siga, (struct sigaction*)NULL);
}

int
main (int argc, char **argv)
{
	uint32_t version;
	xmmsc_result_t *res;
	gchar *path = getenv ("XMMS_PATH");
	connection = xmmsc_init ("xmms2-vistest");

	if (argc != 2 || (height = atoi (argv[1]) * 2) < 1) {
		printf ("Usage: %s <lines>\n", argv[0]);
		exit (EXIT_SUCCESS);
	}

	if (!connection || !xmmsc_connect (connection, path)) {
		printf ("Couldn't connect to xmms2d: %s\n",
		        xmmsc_get_last_error (connection));
		exit (EXIT_FAILURE);
	}

	res = xmmsc_visualization_version (connection);
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

	vis = xmmsc_visualization_init (connection);
	res = xmmsc_visualization_properties_set (connection, vis, config);
	xmmsc_result_wait (res);
	if (xmmsc_result_iserror (res)) {
		puts (xmmsc_result_get_error (res));
		exit (EXIT_FAILURE);
	}
	xmmsc_result_unref (res);

	if (!xmmsc_visualization_start (connection, vis)) {
		printf ("Couldn't start visualization transfer: %s\n",
		        xmmsc_get_last_error (connection));
		exit (EXIT_FAILURE);
	}

	setup_signal ();

	/* using GTK mainloop */
	mainloop = g_main_loop_new (NULL, FALSE);
	xmmsc_mainloop_gmain_init (connection);
	g_timeout_add_full (G_PRIORITY_DEFAULT, 20, draw_gtk, NULL, shutdown_gtk);
	g_main_loop_run (mainloop);

	/* not using GTK mainloop */
	while (xmmsc_visualization_chunk_get (connection, vis, data, 0, 1000) == 256) {
		draw ();
	}

	quit (0);
	return 0;
}
