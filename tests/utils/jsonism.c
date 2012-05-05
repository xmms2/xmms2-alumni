/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2012 XMMS2 Team
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

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <glib.h>

#include "xmmsc/xmmsv.h"
#include "jsonism.h"

xmmsv_t *
xmmsv_from_xson (const char *json)
{
	gchar *normalized;
	char *p;
	xmmsv_t *dict;

	normalized = g_strdup (json);
	for (p = normalized; *p != '\0'; p++) {
		if (*p == '\'') {
			*p = '"';
		}
	}

	dict = xmmsv_from_json (normalized);

	g_free (normalized);

	return dict;
}
