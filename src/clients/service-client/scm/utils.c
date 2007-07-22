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

#include "utils.h"

/**
 * Insert each service client with autostart property set to true to the list.
 */
void
match_auto (gpointer key, gpointer value, gpointer data)
{
	GList **list = data;
	config_t *config = value;

	if (config->autostart)
		*list = g_list_prepend (*list, config);
}

/**
 * Insert each service client with a valid pid.
 */
void
match_pid (gpointer key, gpointer value, gpointer data)
{
	GList **list = data;
	gchar *name = key;
	config_t *config = value;

	if (config->pid)
		*list = g_list_prepend (*list, name);
}

/**
 * Insert any service name which is registered on the server.
 */
gboolean
match_registered (gpointer key, gpointer value, gpointer data)
{
	gchar **n = data;
	gchar *name = key;
	service_t *service = value;

	if (service->registered) {
		*n = name;
		return TRUE;
	}

	return FALSE;
}
