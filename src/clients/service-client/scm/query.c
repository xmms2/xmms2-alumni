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

#include "query.h"
#include "utils.h"

/**
 * List the names of all installed service clients.
 */
void
cb_list_sc_ids (xmmsc_result_t *res, void *data)
{
	xmmsc_service_arg_list_t *ret;
	gchar **retval;
	GList *list = NULL;
	GList *n;
	gint i;

	ret = xmmsc_service_args_new (1, "ids", XMMSC_SERVICE_ARG_TYPE_STRINGLIST);

	g_hash_table_foreach (clients, match_none, &list);

	retval = g_new0 (gchar *, g_list_length (list));
	for (i = 0, n = list; n; i++, n = g_list_next (n))
		retval[i] = (gchar *)n->data;
	g_list_free (list);

	xmmsc_service_arg_value_set (ret, "ids", retval);

	return_and_free (res, ret);
	g_free (retval);
}

/**
 * List details of an installed service client.
 */
void
cb_list_sc (xmmsc_result_t *res, void *data)
{
	config_t *config;
	gchar *name = NULL;
	xmmsc_service_arg_list_t *ret;
	guint retval_auto, retval_services;

	ret = xmmsc_service_args_new (3, "argv", XMMSC_SERVICE_ARG_TYPE_STRING,
	                              "auto", XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              "services", XMMSC_SERVICE_ARG_TYPE_UINT32);

	if ((config = lookup_client (res, &name, ret))) {
		retval_auto = config->autostart;
		retval_services = g_hash_table_size (config->services);

		xmmsc_service_arg_value_set (ret, "argv", config->argv);
		xmmsc_service_arg_value_set (ret, "auto", &retval_auto);
		xmmsc_service_arg_value_set (ret, "services", &retval_services);
	}

	return_and_free (res, ret);
}

/**
 * List names of all installed service.
 */
void
cb_list_service_ids (xmmsc_result_t *res, void *data)
{
	config_t *config;
	gchar *name = NULL;
	xmmsc_service_arg_list_t *ret;
	gchar **retval = NULL;
	GList *list = NULL;
	GList *n;
	gint i;

	ret = xmmsc_service_args_new (1, "ids", XMMSC_SERVICE_ARG_TYPE_STRINGLIST);

	if ((config = lookup_client (res, &name, ret))) {
		g_hash_table_foreach (config->services, match_none, &list);

		retval = g_new0 (gchar *, g_list_length (list));
		for (i = 0, n = list; n; i++, n = g_list_next (n))
			retval[i] = (gchar *)n->data;
		g_list_free (list);

		xmmsc_service_arg_value_set (ret, "ids", retval);
	}

	return_and_free (res, ret);
	g_free (retval);
}

/**
 * List details of an installed service.
 */
void
cb_list_service (xmmsc_result_t *res, void *data)
{
	service_t *service;
	gchar *name = NULL;
	xmmsc_service_arg_list_t *ret;
	guint retval_major, retval_minor, retval_registered, retval_methods;

	ret = xmmsc_service_args_new (5, "desc", XMMSC_SERVICE_ARG_TYPE_STRING,
	                              "major", XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              "minor", XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              "registered", XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              "methods", XMMSC_SERVICE_ARG_TYPE_UINT32);

	if ((service = lookup_service (res,
	                               lookup_client (res, &name, ret),
	                               &name, ret))) {
		retval_major = service->major;
		retval_minor = service->minor;
		retval_registered = service->registered;
		retval_methods = g_hash_table_size (service->methods);

		xmmsc_service_arg_value_set (ret, "desc", service->desc);
		xmmsc_service_arg_value_set (ret, "major", &retval_major);
		xmmsc_service_arg_value_set (ret, "minor", &retval_minor);
		xmmsc_service_arg_value_set (ret, "registered", &retval_registered);
		xmmsc_service_arg_value_set (ret, "methods", &retval_methods);
	}

	return_and_free (res, ret);
}

/**
 * List the names of all installed methods.
 */
void
cb_list_method_ids (xmmsc_result_t *res, void *data)
{
	service_t *service;
	gchar *name = NULL;
	xmmsc_service_arg_list_t *ret;
	gchar **retval = NULL;
	GList *list = NULL;
	GList *n;
	gint i;

	ret = xmmsc_service_args_new (1, "ids", XMMSC_SERVICE_ARG_TYPE_STRINGLIST);

	if ((service = lookup_service (res,
	                               lookup_client (res, &name, ret),
	                               &name, ret))) {
		g_hash_table_foreach (service->methods, match_none, &list);

		retval = g_new0 (gchar *, g_list_length (list));
		for (i = 0, n = list; n; i++, n = g_list_next (n))
			retval[i] = (gchar *)n->data;
		g_list_free (list);

		xmmsc_service_arg_value_set (ret, "ids", retval);
	}

	return_and_free (res, ret);
	g_free (retval);
}

/**
 * List details of an installed method.
 */
void
cb_list_method (xmmsc_result_t *res, void *data)
{
	gchar *method;
	gchar *name = NULL;
	xmmsc_service_arg_list_t *ret;

	ret = xmmsc_service_args_new (1, "desc", XMMSC_SERVICE_ARG_TYPE_STRING);

	if ((method = lookup_method (res,
	                             lookup_service (res,
	                                             lookup_client (res, &name, ret),
	                                             &name, ret),
	                             &name, ret)))
		xmmsc_service_arg_value_set (ret, "desc", method);

	return_and_free (res, ret);
}
