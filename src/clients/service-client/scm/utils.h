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

#ifndef __UTILS_H__
#define __UTILS_H__

#include "common.h"

typedef struct query_info_St {
	gconstpointer target;
	GList *result;
} query_info_t;

void match_auto (gpointer key, gpointer value, gpointer data);
void match_pid (gpointer key, gpointer value, gpointer data);
void match_none (gpointer key, gpointer value, gpointer data);
gboolean match_registered (gpointer key, gpointer value, gpointer data);
void match_service (gpointer key, gpointer value, gpointer data);

#endif
