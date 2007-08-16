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

#include "callback.h"
#include "utils.h"
#include "monitor.h"

/**
 * Update configval.
 *
 * In order to pass conn, I use a nasty tricky way to tell which configval I'm
 * dealing with.
 */
static void
update_configval (xmmsc_result_t *res, void *data)
{
	xmmsc_connection_t *conn = data;
	gchar *value;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering update_configval: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if (!xmmsc_result_get_string (res, &value)) {
		print_error ("Couldn't get configval from server");
		return;
	}

	if (g_ascii_isdigit (*value))
		timeout = g_ascii_strtoull (value, NULL, 10);
	else {
		if (g_strcasecmp (value, "yes") == 0) {
			if (!monitor) {
				monitor = TRUE;
				start_monitor (conn);
			}
		} else {
			if (monitor) {
				monitor = FALSE;
				shutdown_monitor ();
			}
		}
	}

	xmmsc_result_unref (res);
}

/**
 * Update service infos.
 */
static void
update_service_infos (xmmsc_result_t *res, void *data)
{
	service_t *service = (service_t *)data;
	gchar *desc = NULL;
	guint major, minor;

	if (!service) {
		print_error ("No service given.");
		return;
	}

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering update_service_infos: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if (!xmmsc_result_get_dict_entry_string (res, "description", &desc)) {
		print_error ("Service description not given.");
		return;
	}

	if (!xmmsc_result_get_dict_entry_uint (res, "major_version", &major)) {
		print_error ("Service major version not given.");
		return;
	}

	if (!xmmsc_result_get_dict_entry_uint (res, "minor_version", &minor)) {
		print_error ("Service minor version not given.");
		return;
	}

	g_free (service->desc);
	service->desc = g_strdup (desc);
	service->major = major;
	service->minor = minor;
}

/**
 * Update method infos.
 */
static void
update_method_infos (xmmsc_result_t *res, void *data)
{
	method_t *method = (method_t *)data;
	gchar *desc;

	if (!method) {
		print_error ("No method given.");
		return;
	}

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering update_method_infos: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if (!xmmsc_result_get_dict_entry_string (res, "description", &desc)) {
		print_error ("Method description not given.");
		return;
	}

	g_free (method->desc);
	method->desc = g_strdup (desc);
}

/**
 * Handle configval
 */
void
cb_configval (xmmsc_result_t *res, void *data)
{
	xmmsc_connection_t *conn = data;
	gchar *name;
	xmmsc_result_t *result;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_configval: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if (!xmmsc_result_get_string (res, &name)) {
		print_error ("Couldn't register configval in server");
		return;
	}

	result = xmmsc_configval_get (conn, name);
	xmmsc_result_notifier_set_full (result, update_configval, conn, NULL);
	xmmsc_result_unref (result);
	xmmsc_result_unref (res);
}

/**
 * Handle configval changes.
 */
void
cb_configval_changed (xmmsc_result_t *res, void *data)
{
	xmmsc_connection_t *conn = data;
	gchar *value;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_configval_changed: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if (xmmsc_result_get_dict_entry_string (res, CONFIGVAL_TIMEOUT, &value))
		timeout = g_ascii_strtoull (value, NULL, 10);
	else if (xmmsc_result_get_dict_entry_string (res, CONFIGVAL_MONITOR,
	                                             &value)) {
		if (g_strcasecmp (value, "yes") == 0) {
			if (!monitor) {
				monitor = TRUE;
				start_monitor (conn);
			}
		} else {
			if (monitor) {
				monitor = FALSE;
				shutdown_monitor ();
			}
		}
	}
}

/**
 * Handle service changes.
 */
void
cb_service_changed (xmmsc_result_t *res, void *data)
{
	xmms_service_changed_actions_t type;
	gchar *name = NULL;
	xmmsc_connection_t *conn = (xmmsc_connection_t *)data;
	xmmsc_result_t *result;
	query_info_t info;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_service_changed: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if (!xmmsc_result_get_dict_entry_uint (res, "type", &type)) {
		print_error ("Service changed type not given.");
		return;
	}

	if (!xmmsc_result_get_dict_entry_string (res, "service", &name)) {
		print_error ("Service name not given.");
		return;
	}

	if (g_strcasecmp (name, SERVICE_MANAGEMENT) == 0 ||
	    g_strcasecmp (name, SERVICE_QUERY) == 0 ||
	    g_strcasecmp (name, SERVICE_MISC) == 0)
		return;

	info.target = name;
	info.result = NULL;

	switch (type) {
	case XMMS_SERVICE_CHANGED_REGISTER:
		result = xmmsc_service_describe (conn, name);

		if (xmmsc_result_iserror (res)) {
			print_error ("Error requesting service details: %s",
						 xmmsc_result_get_error (res));
			return;
		}

		g_hash_table_foreach (clients, match_unregistered_service, &info);
		if (info.result)
			((service_t *)info.result->data)->registered = TRUE;
		else {
			print_error ("Unable to find an unregistered service as: %s", name);
			return;
		}

		xmmsc_result_notifier_set (result, update_service_infos,
		                           info.result->data);
		xmmsc_result_unref (result);
		break;
	case XMMS_SERVICE_CHANGED_UNREGISTER:
		g_hash_table_foreach (clients, match_registered_service, &info);
		if (info.result)
			((service_t *)info.result->data)->registered = FALSE;
		else {
			print_error ("Unable to find a registered service as: %s", name);
			return;
		}
		break;
	default:
		print_error ("Unknown service changed type.");
		break;
	}

	if (info.result)
		g_list_free (info.result);
}

/**
 * Handle method changes.
 */
void
cb_method_changed (xmmsc_result_t *res, void *data)
{
	xmms_service_changed_actions_t type;
	gchar *service_name = NULL;
	gchar *method_name = NULL;
	xmmsc_connection_t *conn = (xmmsc_connection_t *)data;
	xmmsc_result_t *result;
	service_t *service;
	method_t *method;
	query_info_t info;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_service_changed: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if (!xmmsc_result_get_dict_entry_uint (res, "type", &type)) {
		print_error ("Service changed type not given.");
		return;
	}

	if (!xmmsc_result_get_dict_entry_string (res, "service", &service_name)) {
		print_error ("Service name not given.");
		return;
	}

	if (g_strcasecmp (service_name, SERVICE_MANAGEMENT) == 0 ||
	    g_strcasecmp (service_name, SERVICE_QUERY) == 0 ||
	    g_strcasecmp (service_name, SERVICE_MISC) == 0)
		return;

	info.target = service_name;
	info.result = NULL;
	g_hash_table_foreach (clients, match_registered_service, &info);
	if (info.result) {
		service = (service_t *)info.result->data;
		g_list_free (info.result);
	} else {
		print_error ("Unable to find a registered service as: %s", service_name);
		return;
	}

	if (!xmmsc_result_get_dict_entry_string (res, "method", &method_name)) {
		print_error ("Method name not given.");
		return;
	}

	if (!(method = g_hash_table_lookup (service->methods, method_name))) {
		print_error ("Unable to find method %s.", method_name);
		return;
	}

	switch (type) {
	case XMMS_SERVICE_CHANGED_REGISTER:
		result = xmmsc_service_method_describe (conn, service_name, method_name);

		if (xmmsc_result_iserror (res)) {
			print_error ("Error requesting method details: %s",
						 xmmsc_result_get_error (res));
			return;
		}

		if (!method->registered)
			method->registered = TRUE;
		else {
			print_error ("Unable to find an unregistered method as: %s",
			             method_name);
			return;
		}

		xmmsc_result_notifier_set (result, update_method_infos, method);
		xmmsc_result_unref (result);
		break;
	case XMMS_SERVICE_CHANGED_UNREGISTER:
		if (method->registered)
			method->registered = FALSE;
		else {
			print_error ("Unable to find an unregistered method as: %s",
			             method_name);
			return;
		}
		break;
	default:
		print_error ("Unknown method changed type.");
		break;
	}
}
