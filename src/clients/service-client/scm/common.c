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
print_error_and_exit (xmmsc_connection_t *conn, const gchar *fmt, ...)
{
	gchar buf[1024];
	va_list ap;

	va_start (ap, fmt);
	g_vsnprintf (buf, 1024, fmt, ap);
	va_end (ap);

	g_printerr ("ERROR: %s\n", buf);

	g_hash_table_destroy (clients);
	if (conn)
		xmmsc_unref (conn);
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

/**
 * Return and reset arg list.
 */
void
method_return (xmmsc_connection_t *conn, xmmsc_result_t *res,
               xmmsc_service_method_t *method)
{
	xmmsc_result_t *result;

	x_return_if_fail (res);
	x_return_if_fail (method);

	result = xmmsc_service_return (conn, res, method);
	xmmsc_result_unref (result);
}

/**
 * Get client name and look it up.
 */
config_t *
lookup_client (xmmsc_result_t *res, gchar **name, xmmsc_service_method_t *method)
{
	config_t *config = NULL;

	x_return_null_if_fail (res);
	x_return_null_if_fail (name);
	x_return_null_if_fail (method);

	if (!xmmsc_result_get_dict_entry_string (res, ARG_CLIENT_NAME, name))
		xmmsc_service_method_error_set (method,
		                                "Service client name not given.");
	else
		if (!(config = g_hash_table_lookup (clients, *name)))
			xmmsc_service_method_error_set (method,
			                                "Service client does not exist or it"
			                                " is a remote service client.");

	return config;
}

/**
 * Get service name and look it up.
 */
service_t *
lookup_service (xmmsc_result_t *res, const config_t *config, gchar **name,
                xmmsc_service_method_t *method)
{
	service_t *service = NULL;

	x_return_null_if_fail (res);
	x_return_null_if_fail (config);
	x_return_null_if_fail (name);
	x_return_null_if_fail (method);

	if (!xmmsc_result_get_dict_entry_string (res, ARG_SERVICE_NAME, name))
		xmmsc_service_method_error_set (method, "Service name not given.");
	else
		if (!(service = g_hash_table_lookup (config->services, *name)))
			xmmsc_service_method_error_set (method, "Service does not exist");

	return service;
}

/**
 * Get method name and look it up.
 */
gchar *
lookup_method (xmmsc_result_t *res, const service_t *service, gchar **name,
               xmmsc_service_method_t *method)
{
	gchar *method_name = NULL;

	x_return_null_if_fail (res);
	x_return_null_if_fail (service);
	x_return_null_if_fail (name);
	x_return_null_if_fail (method);

	if (!xmmsc_result_get_dict_entry_string (res, ARG_METHOD_NAME, name))
		xmmsc_service_method_error_set (method,
		                                "Service method name not given.");
	else
		if (!(method_name = g_hash_table_lookup (service->methods, *name)))
			xmmsc_service_method_error_set (method,
			                                "Service method does not exist");

	return method_name;
}
