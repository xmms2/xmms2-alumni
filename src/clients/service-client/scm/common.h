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

#ifndef __COMMON_H__
#define __COMMON_H__

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include <xmmsclient/xmmsclient.h>
#include <xmmsclient/xmmsclient-glib.h>

typedef struct config_St {
	gchar *path;
	gchar *argv;
	GPid pid;
	gboolean autostart;
	gboolean registered;
	GHashTable *services;
} config_t;

typedef struct service_St {
	gchar *desc;
	guint major;
	guint minor;
	GHashTable *methods;
} service_t;

GMainLoop *ml;
xmmsc_connection_t *conn;

void print_info (const gchar *fmt, ...);
void print_error (const gchar *fmt, ...);
void print_error_and_exit (const gchar *fmt, ...);
void print_method (gpointer k, gpointer v, gpointer d);
void print_service (gpointer k, gpointer v, gpointer d);
void print_config (gpointer k, gpointer v, gpointer d);

#endif
