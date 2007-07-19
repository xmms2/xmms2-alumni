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

#include "management.h"
#include "config.h"

static config_t *lookup_client (xmmsc_result_t *res, GHashTable *clients,
                                gchar **name, xmmsc_service_arg_list_t *err);
static void return_and_free (xmmsc_result_t *res, xmmsc_service_arg_list_t *ret);
static gboolean shutdown_single (GPid pid);

/**
 * Remove the corresponding config file.
 */
void
cb_uninstall (xmmsc_result_t *res, void *data)
{
	GHashTable *clients = data;
	config_t *config;
	gchar *name = NULL;
	xmmsc_service_arg_list_t *ret;
	uint32_t retval = 1;

	ret = xmmsc_service_args_new (1, "ret", XMMSC_SERVICE_ARG_TYPE_UINT32);

	if ((config = lookup_client (res, clients, &name, ret))) {
		if (config->registered && !shutdown_single (config->pid))
			xmmsc_service_error_set (ret, "Failed to shutdown service client.");
		if (!xmmsc_service_error_isset (ret) && !remove_config (name))
			xmmsc_service_error_set (ret, "Failed to uninstall service client.");

		xmmsc_service_arg_value_set (ret, "ret", &retval);
	}

	return_and_free (res, ret);
	free (name);
}

/**
 * Change startup argument passed to the service client.
 */
void
cb_change_startup_arg (xmmsc_result_t *res, void *data)
{
	GHashTable *clients = data;
	config_t *config;
	gchar *name = NULL;
	gchar *new_argv = NULL;
	xmmsc_service_arg_list_t *ret;
	uint32_t retval = 1;

	ret = xmmsc_service_args_new (1, "ret", XMMSC_SERVICE_ARG_TYPE_UINT32);

	if ((config = lookup_client (res, clients, &name, ret))) {
		if (!xmmsc_result_get_dict_entry_string (res, "argv", &new_argv))
			xmmsc_service_error_set (ret, "New startup arguments not given.");
		else {
			if (config->argv)
				g_free (config->argv);
			config->argv = g_strdup (new_argv);

			xmmsc_service_arg_value_set (ret, "ret", &retval);
		}
	}

	return_and_free (res, ret);
	free (name);
	free (new_argv);
}

/**
 * Start a service client.
 */
void
cb_launch (xmmsc_result_t *res, void *data)
{
	GHashTable *clients = data;
}

/**
 * Shutdown a service client.
 */
void
cb_shutdown (xmmsc_result_t *res, void *data)
{
	GHashTable *clients = data;
}

/**
 * Toggle a service client's autostart.
 */
void
cb_toggle_autostart (xmmsc_result_t *res, void *data)
{
	GHashTable *clients = data;
}

/**
 * Shutdown all service clients.
 */
gboolean
shutdown_all (GHashTable *clients)
{
	return FALSE;
}

/**
 * internal
 */

/**
 * Get client name and look it up.
 */
static config_t *
lookup_client (xmmsc_result_t *res, GHashTable *clients, gchar **name,
               xmmsc_service_arg_list_t *err)
{
	config_t *config;

	x_return_null_if_fail (res);
	x_return_null_if_fail (name);
	x_return_null_if_fail (err);

	if (!xmmsc_result_get_dict_entry_string (res, "name", name))
		xmmsc_service_error_set (err, "Service client name not given.");
	else
		if (!(config = g_hash_table_lookup (clients, *name)))
			xmmsc_service_error_set (err, "Service client does not exist"
			                         " or it is a remote service client.");

	return config;
}

/**
 * Return and free arg list.
 */
static void
return_and_free (xmmsc_result_t *res, xmmsc_service_arg_list_t *ret)
{
	xmmsc_result_t *result;

	x_return_if_fail (res);
	x_return_if_fail (ret);

	result = xmmsc_service_return (conn, res, ret);
	xmmsc_result_unref (result);
	xmmsc_service_args_free (ret);
}

/**
 * Shutdown a single service client.
 */
static gboolean
shutdown_single (GPid pid)
{
	return FALSE;
}
