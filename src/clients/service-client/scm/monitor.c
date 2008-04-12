/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2008 XMMS2 Team
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

#include <unistd.h>
#include <glib/gstdio.h>

#include "monitor.h"
#include "management.h"

#define ADD 0x00000001
#define DEL 0x00000002
#define BOTH (ADD | DEL)

static gboolean quit;

static void
do_file (GHashTable *clients, const gchar *filename, guint mask)
{
	config_t *config;
	GPid pid;

	if (mask & DEL) {
		if ((config = g_hash_table_lookup (clients, filename))) {
			pid = config->pid;
		}
		if (!g_hash_table_remove (clients, filename)) {
			print_error ("Unable to remove service client %s from the manager",
			             filename);
		}
	}

	if (mask & ADD) {
		if ((config = read_config (filename))) {
			g_hash_table_insert (clients, g_strdup (filename), config);
			if (!pid && config->autostart) {
				launch_single (config);
			} else {
				config->pid = pid;
			}
		}
	}
}

#ifdef INOTIFY
#define EVENT_SIZE (sizeof (struct inotify_event))
#define MAXLEN (256 * (EVENT_SIZE + 16))

static gint
start_inotify ()
{
	int fd, wd;

	fd = inotify_init ();
	if (fd < 0) {
		print_error ("Unable to init monitor: %s", strerror (errno));
		return -1;
	}

	wd = inotify_add_watch (fd, config_dir (), IN_CREATE | IN_DELETE |
	                        IN_MODIFY | IN_MOVE);
	if (wd < 0) {
		print_error ("Unable to watch on config dir: %s", strerror (errno));
		if ((wd = close (fd)) < 0) {
			print_error ("Unable to shutdown monitor: %s", strerror (errno));
		}
		return -1;
	}

	return fd;
}

static gboolean
handle_inotify (GIOChannel *source, GIOCondition cond, gpointer data)
{
	int fd;
	int len, i;
	gchar buf[MAXLEN];
	gchar *name = NULL;
	struct inotify_event *event;
	guint mask = 0;

	if (quit) {
		g_io_channel_shutdown (source, FALSE, NULL);
		g_io_channel_unref (source);
		return FALSE;
	}

	fd = g_io_channel_unix_get_fd (source);

	len = read (fd, buf, MAXLEN);
	if (len < 0) {
		print_error ("Unable to read fd: %s", strerror (errno));
		return FALSE;
	}

	for (i = 0; i < len; i += EVENT_SIZE + event->len) {
		event = (struct inotify_event *)&buf[i];

		if (!event->len) {
			continue;
		}

		if (name && g_strcasecmp (event->name, name) == 0) {
			continue;
		}

		g_free (name);
		name = g_strdup (event->name);

		if (event->mask & IN_DELETE || event->mask & IN_MODIFY ||
		    event->mask & IN_MOVED_FROM) {
			mask |= DEL;
		}

		if (event->mask & IN_CREATE || event->mask & IN_MODIFY ||
		    event->mask & IN_MOVED_TO) {
			mask |= ADD;
		}

		do_file ((GHashTable *)data, event->name, mask);
	}

	g_free (name);

	return TRUE;
}
#endif

static void
check_file (GHashTable *clients, const gchar *filename)
{
	config_t *config;
	guint mask = 0;
	struct stat buf;

	if (!(config = g_hash_table_lookup (clients, filename))) {
		mask |= ADD;
	} else {
		if (g_stat (g_build_path (G_DIR_SEPARATOR_S, config_dir (), filename,
		                          NULL), &buf) < 0) {
			print_error ("Unable to retreive file attributes");
			return;
		}

		if (config->mtime != buf.st_mtime) {
			mask |= BOTH;
		}
	}

	do_file (clients, filename, mask);
}

static gboolean
handle_poll (gpointer data)
{
	GHashTable *clients = data;
	GDir *dir;
	const gchar *filename;

	if (quit) {
		return FALSE;
	}

	dir = g_dir_open (config_dir (), 0, NULL);

	while (dir && (filename = g_dir_read_name (dir))) {
		check_file (clients, filename);
	}

	g_dir_close (dir);

	return TRUE;
}

gboolean
start_monitor (GHashTable *clients)
{
	quit = FALSE;

#ifdef INOTIFY
	int fd;
	GIOChannel *gio;

	if ((fd = start_inotify ()) < 0) {
		return FALSE;
	}
	gio = g_io_channel_unix_new (fd);
	g_io_add_watch (gio, G_IO_IN, handle_inotify, clients);
#else
	g_timeout_add (period, handle_poll, clients);
#endif

	return TRUE;
}

void
shutdown_monitor (void)
{
	quit = TRUE;
}

/**
 * Manually poll.
 */
void
cb_poll (xmmsc_connection_t *conn, xmmsc_result_t *res,
         xmmsc_service_method_t *method, void *data)
{
	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_change_argv: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	handle_poll (data);
}
