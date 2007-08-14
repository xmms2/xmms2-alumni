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

#include <unistd.h>

#include "monitor.h"
#include "management.h"

#ifdef INOTIFY
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
		if ((wd = close (fd)) < 0)
			print_error ("Unable to shutdown monitor: %s", strerror (errno));
		return -1;
	}

	return fd;
}

#else
static gboolean
start_poll ()
{
	return TRUE;
}
#endif

#ifdef INOTIFY
#define EVENT_SIZE (sizeof (struct inotify_event))
#define MAXLEN (256 * (EVENT_SIZE + 16))

static gboolean
handle_inotify (GIOChannel *source, GIOCondition cond, gpointer data)
{
	xmmsc_connection_t *conn = data;
	int fd;
	int len, i;
	gchar buf[MAXLEN];
	struct inotify_event *event;
	config_t *config;

	fd = g_io_channel_unix_get_fd (source);

	len = read (fd, buf, MAXLEN);
	if (len < 0) {
		print_error ("Unable to read fd: %s", strerror (errno));
		return FALSE;
	}

	for (i = 0; i < len; i += EVENT_SIZE + event->len) {
		event = (struct inotify_event *)&buf[i];

		if (!event->len)
			continue;

		if (event->mask & IN_DELETE || event->mask & IN_MODIFY ||
		    event->mask & IN_MOVED_FROM) {
			if ((config = g_hash_table_lookup (clients, event->name)) &&
			    config->pid)
				shutdown_single (conn, event->name);
			if (!g_hash_table_remove (clients, event->name))
				print_error ("Unable to remove service client %s from the"
				             " manager", event->name);
		}

		if (event->mask & IN_CREATE || event->mask & IN_MODIFY ||
		    event->mask & IN_MOVED_TO) {
			if ((config = read_config (event->name))) {
				g_hash_table_insert (clients, g_strdup (event->name), config);
				if (config->autostart)
					launch_single (config);
			}
		}
	}

	return TRUE;
}

#else
static gboolean
handle_poll (GIOChannel *source, GIOCondition cond, gpointer data)
{

}
#endif

static GIOChannel *
watch_fd (xmmsc_connection_t *conn, int fd)
{
	GIOChannel *gio;

	gio = g_io_channel_unix_new (fd);
#ifdef INOTIFY
	g_io_add_watch (gio, G_IO_IN, handle_inotify, conn);
#else
	g_io_add_watch (gio, G_IO_IN, handle_poll, NULL);
#endif

	return gio;
}

GIOChannel *
start_monitor (xmmsc_connection_t *conn)
{
	int fd;

#ifdef INOTIFY
	if ((fd = start_inotify ()) < 0)
#else
	if ((fd = start_poll ()) < 0)
#endif
		return NULL;

	return watch_fd (conn, fd);
}

void
shutdown_monitor (GIOChannel *gio)
{
	if (!gio)
		return;

	g_io_channel_shutdown (gio, FALSE, NULL);
}
