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
#include <sys/types.h>

#include <xmmsclient/xmmsclient.h>
#include <xmmsclient/xmmsclient-glib.h>

#define SCM_NAME "scm"

#define CONFIGVAL_TIMEOUT SCM_NAME ".timeout"
#define CONFIGVAL_MONITOR SCM_NAME ".monitor"

#define SERVICE_MANAGEMENT "se.xmms." SCM_NAME ".management"
#define SERVICE_QUERY "se.xmms." SCM_NAME ".query"
#define SERVICE_MISC "se.xmms." SCM_NAME ".misc"

#define ARG_CLIENT_NAME "client_name"
#define ARG_SERVICE_NAME "service_name"
#define ARG_METHOD_NAME "method_name"
#define ARG_ARGV "argv"
#define ARG_AUTO "auto"
#define ARG_SERVICES "services"
#define ARG_DESC "desc"
#define ARG_MAJOR "major"
#define ARG_MINOR "minor"
#define ARG_REGISTERED "registered"
#define ARG_METHODS "methods"
#define ARG_RET "ret"
#define ARG_IDS "ids"

guint timeout;
guint period;

typedef struct {
	gchar *path;
	gchar *argv;
	time_t mtime;
	GPid pid;
	gboolean autostart;
	GHashTable *services;
} config_t;

typedef struct {
	gchar *desc;
	guint major;
	guint minor;
	gboolean registered;
	GHashTable *methods;
} service_t;

typedef struct {
	gchar *desc;
	gboolean registered;
} method_t;

GHashTable *clients;

void print_info (const gchar *fmt, ...);
void print_error (const gchar *fmt, ...);
void print_error_and_exit (xmmsc_connection_t *conn, const gchar *fmt, ...);
void print_method (gpointer k, gpointer v, gpointer d);
void print_service (gpointer k, gpointer v, gpointer d);
void print_config (gpointer k, gpointer v, gpointer d);

void method_return (xmmsc_connection_t *conn, xmmsc_result_t *res,
                    xmmsc_service_method_t *method);
config_t *lookup_client (xmmsc_result_t *res, gchar **name,
                         xmmsc_service_method_t *method);
service_t *lookup_service (xmmsc_result_t *res, const config_t *config,
                           gchar **name, xmmsc_service_method_t *method);
method_t *lookup_method (xmmsc_result_t *res, const service_t *service,
                      gchar **name, xmmsc_service_method_t *method);

#endif
