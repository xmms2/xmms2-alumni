/*
 *  XMMS2 Bluetooth Client.
 *  Copyright (C) 2005 Jens Taprogge
 *
 *  This code is heavily based on the XMMS2 CLI client by
 *  Peter Alm, Tobias Rundström, Anders Gustafsson.
 *
 *  It is inspired by Tom Gilbert's XMMS Bluetooth client 'bluexmms'
 */


#include "xmms2_blue.h"

#include <locale.h>

#define RETRY_INTERVAL 1 /* in seconds */

/**
 * Utils
 */

static gchar defaultconfig[] = "ipcpath=NULL\nbluedev=/dev/ircomm0\n";


void
print_error (const char *fmt, ...)
{
	char buf[1024];
	va_list ap;
	
	va_start (ap, fmt);
	vsnprintf (buf, 1024, fmt, ap);
	va_end (ap);

	printf ("ERROR: %s\n", buf);

	exit (-1);
}

	
void
print_info (const char *fmt, ...)
{
	char buf[8096];
	va_list ap;
	
	va_start (ap, fmt);
	vsnprintf (buf, 8096, fmt, ap);
	va_end (ap);

	printf ("%s\n", buf);
}


void
print_hash (const void *key, const void *value, void *udata)
{
	printf ("%s = %s\n", (char *)key, (char *)value);
}


static GHashTable *
read_config ()
{
	gchar *file;
	gchar *buffer;
	gchar **split;
	gint read_bytes;
	FILE *fp;
	struct stat st;
	GHashTable *config;
	int i = 0;

	file = g_strdup_printf ("%s/.xmms2/clients/blue.conf", g_get_home_dir ());

	if (!g_file_test (file, G_FILE_TEST_EXISTS)) {
		gchar *dir = g_strdup_printf ("%s/.xmms2/clients", g_get_home_dir ());
		mkdir (dir, 0755);
		g_free (dir);
		fp = fopen (file, "w+");
		if (!fp) {
			print_error ("Could not create default configfile!!");
		}
		fwrite (defaultconfig, strlen (defaultconfig), 1, fp);
		fclose (fp);
	}

	if (!(fp = fopen (file, "r"))) {
		print_error ("Could not open configfile %s", file);
	}

	if (fstat (fileno (fp), &st) == -1) {
		perror ("fstat");
		return NULL;
	}

	buffer = g_malloc0 (st.st_size + 1);

	read_bytes = 0;
	while (read_bytes < st.st_size) {
		guint ret = fread (buffer + read_bytes, st.st_size - read_bytes, 1, fp);

		if (ret == 0) {
			break;
		}

		read_bytes += ret;
		g_assert (read_bytes >= 0);
	}

	config = g_hash_table_new (g_str_hash, g_str_equal);

	split = g_strsplit (buffer, "\n", 0);
	while (split[i]) {
		if (!split[i])
			break;

		gchar **s = g_strsplit (split[i], "=", 2);
		if (!s || !s[0] || !s[1])
			break;
		if (g_strcasecmp (s[1], "NULL") == 0) {
			g_hash_table_insert (config, s[0], NULL);
		} else {
			g_hash_table_insert (config, s[0], s[1]);
		}

		i++;
	}

	g_free (buffer);
	return config;
}


/**
 * here comes all the cmd callbacks
 */

static int
cmd_shuffle (xmmsc_connection_t *connection, int user, int dev)
{
	xmmsc_result_t *res;
	
	res = xmmsc_playlist_shuffle (connection);
	xmmsc_result_wait (res);
	xmmsc_result_unref (res);
	
	return 0;
}


static int
cmd_play (xmmsc_connection_t *connection, int user, int dev)
{
	xmmsc_result_t *res;
	res = xmmsc_playback_start (connection);
	xmmsc_result_wait (res);
	if (xmmsc_result_iserror (res)) {
		fprintf (stderr, "Couldn't start playback: %s\n", xmmsc_result_get_error (res));
	}
	xmmsc_result_unref (res);
	
	return 0;
}


static int
cmd_stop (xmmsc_connection_t *connection, int user, int dev)
{
	xmmsc_result_t *res;
	res = xmmsc_playback_stop (connection);
	xmmsc_result_wait (res);
	if (xmmsc_result_iserror (res)) {
		fprintf (stderr, "Couldn't stop playback: %s\n", xmmsc_result_get_error (res));
	}
	xmmsc_result_unref (res);

	return 0;
}


static int
cmd_pause (xmmsc_connection_t *connection, int user, int dev)
{
	xmmsc_result_t *res;
	res = xmmsc_playback_pause (connection);
	xmmsc_result_wait (res);
	if (xmmsc_result_iserror (res)) {
		fprintf (stderr, "Couldn't pause playback: %s\n", xmmsc_result_get_error (res));
	}
	xmmsc_result_unref (res);

	return 0;
}


static void
do_reljump (xmmsc_connection_t *connection, int where)
{
	xmmsc_result_t *res;

	res = xmmsc_playlist_set_next_rel (connection, where);
	xmmsc_result_wait (res);

	if (xmmsc_result_iserror (res)) {
		fprintf (stderr, "Couldn't advance in playlist: %s\n", xmmsc_result_get_error (res));
		return;
	}
	xmmsc_result_unref (res);

	res = xmmsc_playback_tickle (connection);
	xmmsc_result_wait (res);
	xmmsc_result_unref (res);
}


static int
cmd_next (xmmsc_connection_t *connection, int user, int dev)
{
	do_reljump (connection, 1);
	return 0;
}


static int
cmd_prev (xmmsc_connection_t *connection, int user, int dev)
{
	do_reljump (connection, -1);
	return 0;
}


static int
cmd_rate (xmmsc_connection_t *connection, int user, int dev)
{
	int rating, oldrating;

/*	xmmsc_result_t *res;
	res = xmmsc_playback_stop (conn);
	xmmsc_result_wait (res);
	if (xmmsc_result_iserror (res)) {
		fprintf (stderr, "Couldn't stop playback: %s\n", xmmsc_result_get_error (res));
	}
	xmmsc_result_unref (res);
*/
	oldrating = 0;
	
	rating = blue_percent(dev, "xmms2 - rate", 5, oldrating, NULL, NULL);
	if (rating < 0) return rating;
	
	rating /= 20;
	debug("New Rating: %i.\n", rating);

	return 0;
}


static int
cmd_volupd (int perc, void *connection)
{
	debug("Volume upd: %i\n", perc);
	return 0;
}


static int
cmd_vol (xmmsc_connection_t *connection, int user, int dev)
{
	int vol, oldvol;

/*	xmmsc_result_t *res;
	res = xmmsc_playback_stop (conn);
	xmmsc_result_wait (res);
	if (xmmsc_result_iserror (res)) {
		fprintf (stderr, "Couldn't stop playback: %s\n", xmmsc_result_get_error (res));
	}
	xmmsc_result_unref (res);
*/
	oldvol = 5;
	
	vol = blue_percent(dev, "xmms2 - volume", 10, oldvol, cmd_volupd, connection);

	debug("New Volume: %i.\n", vol);

	return 0;
}




/**
 * Defines all available commands.
 */

struct cmds mainmenu[] = {
	/* Playback managment */
	{ "play", cmd_play, 0 },
	{ "stop", cmd_stop, 0 },
	{ "pause", cmd_pause, 0 },
	{ "next", cmd_next, 0 },
	{ "prev", cmd_prev, 0 },
	{ "rate", cmd_rate, 0 },
	{ "volume", cmd_vol, 0 },
	{ "shuffle", cmd_shuffle, 0 },
	{ NULL, NULL, 0 },
};


int
main (int argc, char **argv)
{
	xmmsc_connection_t *connection;
	GHashTable *config;
	char *path, *bluepath;
	int bluedev = -1;
	char buf[256];

	setlocale (LC_ALL, "");

	config = read_config ();

	connection = xmmsc_init ("XMMS2 Blue");

	if (!connection) {
		print_error ("Could not init xmmsc_connection, this is a memory problem, fix your os!");
	}

	path = getenv ("XMMS_PATH");
	if (!path) path = g_hash_table_lookup (config, "ipcpath");

	if (!xmmsc_connect (connection, path)) {
		print_error ("Could not connect to xmms2d: %s", xmmsc_get_last_error (connection));
	}

	bluepath = g_hash_table_lookup(config, "bluepath");
	if (!bluepath) bluepath = "/dev/rfcomm0";

	for (;;) {
		if ((bluedev = blue_open(bluepath)) < 0) {
			/* retry */
			sleep(RETRY_INTERVAL);
			continue;
		}
		
		if (blue_init(bluedev) < 0) {
			debug("init failed\n");
			if (close(bluedev)) perror("Close");

			/* retry */
			continue;
		}


		for (;;) {
			if (blue_get_block(bluedev, buf) < 0) {
				/* reopen device */
				break;
			}
			if (!strcasecmp("*EAAI", buf)) {
				if (blue_menu(bluedev, "xmms2", mainmenu, 1, connection) == -2) {
					/* reopen device */
					break;
				}
			} else {
				debug("unmatched sequence \"%s\"", buf);
			}
		}
		if (close(bluedev)) perror("Close");
	}

	return 0;
}

