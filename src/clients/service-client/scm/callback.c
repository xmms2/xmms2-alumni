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
 * Free data.
 */
static void
free_data (void *data)
{
	info_t *info = data;

	g_free (info->data);
	free (info);
}

/**
 * Update configval.
 */
static void
update_configval (xmmsc_result_t *res, void *data)
{
	info_t *info = data;
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

	if (g_strcasecmp (info->data, "clients." CONFIGVAL_TIMEOUT) == 0)
		timeout = g_ascii_strtoull (value, NULL, 10);
	else if (g_strcasecmp (info->data, "clients." CONFIGVAL_PERIOD) == 0)
		period = g_ascii_strtoull (value, NULL, 10);
	else {
		if (g_strcasecmp (value, "yes") == 0) {
			if (!monitor) {
				monitor = TRUE;
				start_monitor (info->clients);
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
	info_t *info;
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

	info = x_new0 (info_t, 1);
	info->clients = ((info_t *)data)->clients;
	info->data = g_strdup (name);

	result = xmmsc_configval_get (((info_t *)data)->conn, name);
	xmmsc_result_notifier_set_full (result, update_configval, info, free_data);
	xmmsc_result_unref (result);
	xmmsc_result_unref (res);
}

/**
 * Handle configval changes.
 */
void
cb_configval_changed (xmmsc_result_t *res, void *data)
{
	gchar *value;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_configval_changed: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if (xmmsc_result_get_dict_entry_string (res, CONFIGVAL_TIMEOUT, &value))
		timeout = g_ascii_strtoull (value, NULL, 10);
	else if (xmmsc_result_get_dict_entry_string (res, CONFIGVAL_PERIOD, &value))
		period = g_ascii_strtoull (value, NULL, 10);
	else if (xmmsc_result_get_dict_entry_string (res, CONFIGVAL_MONITOR,
	                                             &value)) {
		if (g_strcasecmp (value, "yes") == 0) {
			if (!monitor) {
				monitor = TRUE;
				start_monitor ((GHashTable *)data);
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
	info_t info;
	xmmsc_result_t *result;

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

	info.conn = ((info_t *)data)->conn;
	info.clients = ((info_t *)data)->clients;
	info.data = name;
	info.ret = NULL;

	switch (type) {
	case XMMS_SERVICE_CHANGED_REGISTER:
		result = xmmsc_service_describe (info.conn, name);

		if (xmmsc_result_iserror (res)) {
			print_error ("Error requesting service details: %s",
						 xmmsc_result_get_error (res));
			return;
		}

		g_hash_table_foreach (info.clients, match_unregistered_service, &info);
		if (info.ret)
			((service_t *)((GList *)info.ret)->data)->registered = TRUE;
		else {
			print_error ("Unable to find an unregistered service as: %s", name);
			return;
		}

		xmmsc_result_notifier_set (result, update_service_infos,
		                           ((GList *)info.ret)->data);
		xmmsc_result_unref (result);
		break;
	case XMMS_SERVICE_CHANGED_UNREGISTER:
		g_hash_table_foreach (info.clients, match_registered_service, &info);
		if (info.ret)
			((service_t *)((GList *)info.ret)->data)->registered = FALSE;
		else {
			print_error ("Unable to find a registered service as: %s", name);
			return;
		}
		break;
	default:
		print_error ("Unknown service changed type.");
		break;
	}

	if (info.ret)
		g_list_free ((GList *)info.ret);
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
	info_t info;
	xmmsc_result_t *result;
	service_t *service;
	method_t *method;

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

	info.conn = ((info_t *)data)->conn;
	info.clients = ((info_t *)data)->clients;
	info.data = service_name;
	info.ret = NULL;
	g_hash_table_foreach (info.clients, match_registered_service, &info);
	if (info.ret) {
		service = (service_t *)((GList *)info.ret)->data;
		g_list_free ((GList *)info.ret);
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
		result = xmmsc_service_method_describe (info.conn, service_name,
		                                        method_name);

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
