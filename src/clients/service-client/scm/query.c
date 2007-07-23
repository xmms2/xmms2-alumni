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

	ret = xmmsc_service_args_new (1, ARG_IDS, XMMSC_SERVICE_ARG_TYPE_STRINGLIST);

	g_hash_table_foreach (clients, match_none, &list);

	retval = g_new0 (gchar *, g_list_length (list));
	for (i = 0, n = list; n; i++, n = g_list_next (n))
		retval[i] = (gchar *)n->data;
	g_list_free (list);

	xmmsc_service_arg_value_set (ret, ARG_IDS, retval);

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

	ret = xmmsc_service_args_new (3, ARG_ARGV, XMMSC_SERVICE_ARG_TYPE_STRING,
	                              ARG_AUTO, XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              ARG_SERVICES, XMMSC_SERVICE_ARG_TYPE_UINT32);

	if ((config = lookup_client (res, &name, ret))) {
		retval_auto = config->autostart;
		retval_services = g_hash_table_size (config->services);

		xmmsc_service_arg_value_set (ret, ARG_ARGV, config->argv);
		xmmsc_service_arg_value_set (ret, ARG_AUTO, &retval_auto);
		xmmsc_service_arg_value_set (ret, ARG_SERVICES, &retval_services);
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

	ret = xmmsc_service_args_new (1, ARG_IDS, XMMSC_SERVICE_ARG_TYPE_STRINGLIST);

	if ((config = lookup_client (res, &name, ret))) {
		g_hash_table_foreach (config->services, match_none, &list);

		retval = g_new0 (gchar *, g_list_length (list));
		for (i = 0, n = list; n; i++, n = g_list_next (n))
			retval[i] = (gchar *)n->data;
		g_list_free (list);

		xmmsc_service_arg_value_set (ret, ARG_IDS, retval);
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

	ret = xmmsc_service_args_new (5, ARG_DESC, XMMSC_SERVICE_ARG_TYPE_STRING,
	                              ARG_MAJOR, XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              ARG_MINOR, XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              ARG_REGISTERED, XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              ARG_METHODS, XMMSC_SERVICE_ARG_TYPE_UINT32);

	if ((service = lookup_service (res,
	                               lookup_client (res, &name, ret),
	                               &name, ret))) {
		retval_major = service->major;
		retval_minor = service->minor;
		retval_registered = service->registered;
		retval_methods = g_hash_table_size (service->methods);

		xmmsc_service_arg_value_set (ret, ARG_DESC, service->desc);
		xmmsc_service_arg_value_set (ret, ARG_MAJOR, &retval_major);
		xmmsc_service_arg_value_set (ret, ARG_MINOR, &retval_minor);
		xmmsc_service_arg_value_set (ret, ARG_REGISTERED, &retval_registered);
		xmmsc_service_arg_value_set (ret, ARG_METHODS, &retval_methods);
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

	ret = xmmsc_service_args_new (1, ARG_IDS, XMMSC_SERVICE_ARG_TYPE_STRINGLIST);

	if ((service = lookup_service (res,
	                               lookup_client (res, &name, ret),
	                               &name, ret))) {
		g_hash_table_foreach (service->methods, match_none, &list);

		retval = g_new0 (gchar *, g_list_length (list));
		for (i = 0, n = list; n; i++, n = g_list_next (n))
			retval[i] = (gchar *)n->data;
		g_list_free (list);

		xmmsc_service_arg_value_set (ret, ARG_IDS, retval);
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

	ret = xmmsc_service_args_new (1, ARG_DESC, XMMSC_SERVICE_ARG_TYPE_STRING);

	if ((method = lookup_method (res,
	                             lookup_service (res,
	                                             lookup_client (res, &name, ret),
	                                             &name, ret),
	                             &name, ret)))
		xmmsc_service_arg_value_set (ret, ARG_DESC, method);

	return_and_free (res, ret);
}

/**
 * Lookup a service client's which provides the given service.
 *
 * If more than one service client contains the same service, all matching
 * service clients' names will be returned.
 */
void
cb_lookup_client (xmmsc_result_t *res, void *data)
{
	xmmsc_service_arg_list_t *ret;
	gchar *name = NULL;
	query_info_t info;
	gchar **retval = NULL;
	GList *n;
	gint i;

	ret = xmmsc_service_args_new (1, ARG_IDS, XMMSC_SERVICE_ARG_TYPE_STRINGLIST);

	if (!xmmsc_result_get_dict_entry_string (res, ARG_SERVICE_NAME, &name))
		xmmsc_service_error_set (ret, "Service name not given.");
	info.target = name;
	info.result = NULL;

	g_hash_table_foreach (clients, match_service, &info);

	retval = g_new0 (gchar *, g_list_length (info.result));
	for (i = 0, n = info.result; n; i++, n = g_list_next (n))
		retval[i] = (gchar *)n->data;
	g_list_free (info.result);

	xmmsc_service_arg_value_set (ret, ARG_IDS, retval);

	return_and_free (res, ret);
	g_free (retval);
}
