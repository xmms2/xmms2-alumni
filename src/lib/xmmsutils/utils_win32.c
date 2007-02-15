/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2006 XMMS2 Team
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
 * Miscellaneous internal utility functions.
 */

#include <stdlib.h>

#include "xmmsc/xmmsc_util.h"

/**
 * Get the absolute path to the user config dir.
 *
 * @param buf A char buffer
 * @param len The length of buf (PATH_MAX is a good choice)
 * @return A pointer to buf, or NULL if an error occurred.
 */
const char *
xmms_userconfdir_get (char *buf, int len)
{
	char *config_home;

	if (!buf || len <= 0)
		return NULL;

	config_home = getenv ("APPDATA");

	if (config_home && *config_home) {
		snprintf (buf, len, "%s\\xmms2", config_home);

		return buf;
	}

	return NULL;
}


/**
 * Get the default connection path.
 *
 * @param buf A char buffer
 * @param len The length of buf (PATH_MAX is a good choice)
 * @return A pointer to buf, or NULL if an error occured.
 */
const char *
xmms_default_ipcpath_get (char *buf, int len)
{
	snprintf (buf, len, "tcp://127.0.0.1:" XMMS_STRINGIFY (XMMS_DEFAULT_TCP_PORT));

	return buf;
}
