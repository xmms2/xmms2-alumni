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

void
print_info (const gchar *fmt, ...)
{
	gchar buf[8096];
	va_list ap;

	va_start (ap, fmt);
	g_vsnprintf (buf, 8096, fmt, ap);
	va_end (ap);

	g_print ("%s\n", buf);
}

void
print_error (const gchar *fmt, ...)
{
	gchar buf[1024];
	va_list ap;

	va_start (ap, fmt);
	g_vsnprintf (buf, 1024, fmt, ap);
	va_end (ap);

	g_printerr ("ERROR: %s\n", buf);
}

void
print_error_and_exit (const gchar *fmt, ...)
{
	gchar buf[1024];
	va_list ap;

	va_start (ap, fmt);
	g_vsnprintf (buf, 1024, fmt, ap);
	va_end (ap);

	g_printerr ("ERROR: %s\n", buf);

	exit (EXIT_FAILURE);
}

/**
 * Debugging functions.
 */
void
print_method (gpointer k, gpointer v, gpointer d)
{
	gchar *name = k;
	gchar *desc = v;

	print_info ("\t\t%s", name);
	print_info ("\t\tdesc: %s", desc);
}

void
print_service (gpointer k, gpointer v, gpointer d)
{
	gchar *name = k;
	service_t *serv = v;

	print_info ("\t%s", name);
	print_info ("\tdesc: %s\n\tmajor: %d\n\tminor: %d", serv->desc, serv->major,
	            serv->minor);
	g_hash_table_foreach (serv->methods, print_method, NULL);
}

void
print_config (gpointer k, gpointer v, gpointer d)
{
	gchar *name = k;
	config_t *conf = v;

	print_info ("%s", name);
	print_info ("path: %s\nargv: %s", conf->path, conf->argv);
	if (conf->autostart)
		print_info ("auto: yes");
	else
		print_info ("auto: no");
	g_hash_table_foreach (conf->services, print_service, NULL);
}
