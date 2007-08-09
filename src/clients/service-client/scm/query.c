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
cb_list_sc_ids (xmmsc_connection_t *conn, xmmsc_result_t *res,
                xmmsc_service_method_t *method, void *data)
{
	gchar **retval;
	GList *list = NULL;
	GList *n;
	gint i;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_list_sc_ids: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	g_hash_table_foreach (clients, match_none, &list);

	retval = g_new0 (gchar *, g_list_length (list));
	for (i = 0, n = list; n; i++, n = g_list_next (n))
		retval[i] = (gchar *)n->data;
	g_list_free (list);

	xmmsc_service_method_ret_add_stringlist (method, ARG_IDS, retval);

	method_return (conn, res, method);
	g_free (retval);
}

/**
 * List details of an installed service client.
 */
void
cb_list_sc (xmmsc_connection_t *conn, xmmsc_result_t *res,
            xmmsc_service_method_t *method, void *data)
{
	config_t *config;
	gchar *name = NULL;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_list_sc: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if ((config = lookup_client (res, &name, method))) {
		xmmsc_service_method_ret_add_string (method, ARG_ARGV, config->argv);
		xmmsc_service_method_ret_add_uint32 (method, ARG_AUTO,
		                                     config->autostart);
		xmmsc_service_method_ret_add_uint32 (method, ARG_SERVICES,
		                                     g_hash_table_size (config->services));
	}

	method_return (conn, res, method);
}

/**
 * List names of all installed service.
 */
void
cb_list_service_ids (xmmsc_connection_t *conn, xmmsc_result_t *res,
                     xmmsc_service_method_t *method, void *data)
{
	config_t *config;
	gchar *name = NULL;
	gchar **retval = NULL;
	GList *list = NULL;
	GList *n;
	gint i;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_list_service_ids: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if ((config = lookup_client (res, &name, method))) {
		g_hash_table_foreach (config->services, match_none, &list);

		retval = g_new0 (gchar *, g_list_length (list));
		for (i = 0, n = list; n; i++, n = g_list_next (n))
			retval[i] = (gchar *)n->data;
		g_list_free (list);

		xmmsc_service_method_ret_add_stringlist (method, ARG_IDS, retval);
	}

	method_return (conn, res, method);
	g_free (retval);
}

/**
 * List details of an installed service.
 */
void
cb_list_service (xmmsc_connection_t *conn, xmmsc_result_t *res,
                 xmmsc_service_method_t *method, void *data)
{
	service_t *service;
	gchar *name = NULL;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_list_service: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if ((service = lookup_service (res,
	                               lookup_client (res, &name, method),
	                               &name, method))) {
		xmmsc_service_method_ret_add_string (method, ARG_DESC, service->desc);
		xmmsc_service_method_ret_add_uint32 (method, ARG_MAJOR, service->major);
		xmmsc_service_method_ret_add_uint32 (method, ARG_MINOR, service->minor);
		xmmsc_service_method_ret_add_uint32 (method, ARG_REGISTERED,
		                                     service->registered);
		xmmsc_service_method_ret_add_uint32 (method, ARG_METHODS,
		                                     g_hash_table_size (service->methods));
	}

	method_return (conn, res, method);
}

/**
 * List the names of all installed methods.
 */
void
cb_list_method_ids (xmmsc_connection_t *conn, xmmsc_result_t *res,
                    xmmsc_service_method_t *method, void *data)
{
	service_t *service;
	gchar *name = NULL;
	gchar **retval = NULL;
	GList *list = NULL;
	GList *n;
	gint i;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_list_method_ids: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if ((service = lookup_service (res,
	                               lookup_client (res, &name, method),
	                               &name, method))) {
		g_hash_table_foreach (service->methods, match_none, &list);

		retval = g_new0 (gchar *, g_list_length (list));
		for (i = 0, n = list; n; i++, n = g_list_next (n))
			retval[i] = (gchar *)n->data;
		g_list_free (list);

		xmmsc_service_method_ret_add_stringlist (method, ARG_IDS, retval);
	}

	method_return (conn, res, method);
	g_free (retval);
}

/**
 * List details of an installed method.
 */
void
cb_list_method (xmmsc_connection_t *conn, xmmsc_result_t *res,
                xmmsc_service_method_t *method, void *data)
{
	method_t *ret;
	gchar *name = NULL;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_list_method: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if ((ret = lookup_method (res,
	                          lookup_service (res,
	                                          lookup_client (res, &name, method),
	                                          &name, method),
	                          &name, method))) {
		xmmsc_service_method_ret_add_string (method, ARG_DESC, ret->desc);
		xmmsc_service_method_ret_add_uint32 (method, ARG_REGISTERED,
		                                     ret->registered);
	}

	method_return (conn, res, method);
}

/**
 * Lookup a service client's which provides the given service.
 *
 * If more than one service client contains the same service, all matching
 * service clients' names will be returned.
 */
void
cb_lookup_client (xmmsc_connection_t *conn, xmmsc_result_t *res,
                  xmmsc_service_method_t *method, void *data)
{
	gchar *name = NULL;
	query_info_t info;
	gchar **retval = NULL;
	GList *n;
	gint i;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_lookup_client: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if (!xmmsc_result_get_dict_entry_string (res, ARG_SERVICE_NAME, &name))
		xmmsc_service_method_error_set (method, "Service name not given.");
	info.target = name;
	info.result = NULL;

	g_hash_table_foreach (clients, match_service, &info);

	retval = g_new0 (gchar *, g_list_length (info.result));
	for (i = 0, n = info.result; n; i++, n = g_list_next (n))
		retval[i] = (gchar *)n->data;
	g_list_free (info.result);

	xmmsc_service_method_ret_add_stringlist (method, ARG_IDS, retval);

	method_return (conn, res, method);
	g_free (retval);
}
