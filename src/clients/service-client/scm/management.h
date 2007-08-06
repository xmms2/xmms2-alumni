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

#ifndef __MANAGEMENT_H__
#define __MANAGEMENT_H__

#include "common.h"

#define TIMEOUT 30 * 1000		/* In milliseconds */

void cb_uninstall (xmmsc_connection_t *conn, xmmsc_result_t *res,
                   xmmsc_service_method_t *method, void *data);
void cb_change_argv (xmmsc_connection_t *conn, xmmsc_result_t *res,
                     xmmsc_service_method_t *method, void *data);
void cb_launch (xmmsc_connection_t *conn, xmmsc_result_t *res,
                xmmsc_service_method_t *method, void *data);
void cb_shutdown (xmmsc_connection_t *conn, xmmsc_result_t *res,
                  xmmsc_service_method_t *method, void *data);
void cb_toggle_autostart (xmmsc_connection_t *conn, xmmsc_result_t *res,
                          xmmsc_service_method_t *method, void *data);

gboolean launch_all (void);
gboolean shutdown_all (xmmsc_connection_t *conn);

#endif
