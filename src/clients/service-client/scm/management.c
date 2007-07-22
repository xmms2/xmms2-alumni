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

#include <sys/types.h>
#include <signal.h>
#include <errno.h>

#include "management.h"
#include "config.h"
#include "utils.h"

/**
 * internal
 */

/**
 * Launch a single service client.
 */
static gboolean
launch_single (config_t *config)
{
	GError *err = NULL;
	gchar *argv[2] = {NULL, NULL};

	x_return_val_if_fail (config, FALSE);
	x_return_val_if_fail (!config->pid, FALSE);

	argv[0] = config->path;
	argv[1] = config->argv;
	if (g_file_test (config->path, G_FILE_TEST_IS_EXECUTABLE)) {
		if (!g_spawn_async (g_get_home_dir (),
		                    argv, NULL, 0,
		                    NULL, NULL, &config->pid, &err)) {
			print_info ("Failed to launch client (%s)", config->path);
			return FALSE;
		}
	}

	return TRUE;
}

/**
 * Force a service client to shutdown.
 */
static gboolean
force_shutdown (gpointer data)
{
	gchar *name = data;
	config_t *config;

	if (!(config = g_hash_table_lookup (clients, name)))
		return FALSE;

	if (kill (config->pid, SIGTERM)) {
		print_info ("Failed to shutdown service client: %s", strerror (errno));
		return TRUE;
	}

	return FALSE;
}

/**
 * Shutdown a single service client.
 */
static gboolean
shutdown_single (gchar *client)
{
	xmmsc_result_t *result;
	config_t *config;
	gchar *name = NULL;

	x_return_val_if_fail (client, FALSE);

	if (!(config = g_hash_table_lookup (clients, client))) {
		print_info ("Service client (%s) is not installed.", client);
		return FALSE;
	}

	if (!config->pid) {
		print_info ("Service client (%s) is not running.", config->path);
		return FALSE;
	}

	if (!g_hash_table_find (config->services, match_registered, &name)) {
		print_info ("No service is registered from that service client.");
		return FALSE;
	}
	result = xmmsc_service_shutdown (conn, name);
	xmmsc_result_wait (result);
	if (xmmsc_result_iserror (result))
		print_error ("Failed to send shutdown request: %s",
		             xmmsc_result_get_error (result));
	xmmsc_result_unref (result);

	g_timeout_add (TIMEOUT, force_shutdown, client);

	return TRUE;
}

/**
 * public
 */

/**
 * Remove the corresponding config file.
 */
void
cb_uninstall (xmmsc_result_t *res, void *data)
{
	config_t *config;
	gchar *name = NULL;
	xmmsc_service_arg_list_t *ret;
	uint32_t retval = 1;

	ret = xmmsc_service_args_new (1, "ret", XMMSC_SERVICE_ARG_TYPE_UINT32);

	if ((config = lookup_client (res, &name, ret))) {
		if (config->pid && !shutdown_single (name))
			xmmsc_service_error_set (ret, "Failed to shutdown service client.");
		else if (!remove_config (name))
			xmmsc_service_error_set (ret, "Failed to uninstall service client.");

		xmmsc_service_arg_value_set (ret, "ret", &retval);
	}

	return_and_free (res, ret);
}

/**
 * Change startup argument passed to the service client.
 */
void
cb_change_argv (xmmsc_result_t *res, void *data)
{
	config_t *config;
	gchar *name = NULL;
	gchar *new_argv = NULL;
	xmmsc_service_arg_list_t *ret;
	uint32_t retval = 1;

	ret = xmmsc_service_args_new (1, "ret", XMMSC_SERVICE_ARG_TYPE_UINT32);

	if ((config = lookup_client (res, &name, ret))) {
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
}

/**
 * Start a service client.
 */
void
cb_launch (xmmsc_result_t *res, void *data)
{
	config_t *config;
	gchar *name = NULL;
	xmmsc_service_arg_list_t *ret;
	uint32_t retval = 1;

	ret = xmmsc_service_args_new (1, "ret", XMMSC_SERVICE_ARG_TYPE_UINT32);

	if ((config = lookup_client (res, &name, ret))) {
		if (config->pid)
			xmmsc_service_error_set (ret, "Service client is already running.");
		else if (!launch_single (config))
			xmmsc_service_error_set (ret, "Failed to launch service client.");

		xmmsc_service_arg_value_set (ret, "ret", &retval);
	}

	return_and_free (res, ret);
}

/**
 * Shutdown a service client.
 */
void
cb_shutdown (xmmsc_result_t *res, void *data)
{
	config_t *config;
	gchar *name = NULL;
	xmmsc_service_arg_list_t *ret;
	uint32_t retval = 1;

	ret = xmmsc_service_args_new (1, "ret", XMMSC_SERVICE_ARG_TYPE_UINT32);

	if ((config = lookup_client (res, &name, ret))) {
		if (config->pid && !shutdown_single (name))
			xmmsc_service_error_set (ret, "Failed to shutdown service client.");

		xmmsc_service_arg_value_set (ret, "ret", &retval);
	}

	return_and_free (res, ret);
}

/**
 * Toggle a service client's autostart.
 */
void
cb_toggle_autostart (xmmsc_result_t *res, void *data)
{
	config_t *config;
	gchar *name = NULL;
	guint new_auto;
	xmmsc_service_arg_list_t *ret;
	uint32_t retval = 1;

	ret = xmmsc_service_args_new (1, "ret", XMMSC_SERVICE_ARG_TYPE_UINT32);

	if ((config = lookup_client (res, &name, ret))) {
		if (!xmmsc_result_get_dict_entry_uint (res, "auto", &new_auto))
			xmmsc_service_error_set (ret, "New autostart value not given.");
		else {
			config->autostart = new_auto;

			xmmsc_service_arg_value_set (ret, "ret", &retval);
		}
	}

	return_and_free (res, ret);
}

/**
 * Launch all service clients with autostart set to true.
 */
gboolean
launch_all (void)
{
	GList *list = NULL;
	GList *n;

	g_hash_table_foreach (clients, match_auto, &list);

	for (n = list; n; n = g_list_next (n)) {
		if (!launch_single ((config_t *)n->data))
			return FALSE;
	}

	g_list_free (list);
	return TRUE;
}

/**
 * Shutdown all service clients.
 */
gboolean
shutdown_all (void)
{
	GList *list = NULL;
	GList *n;

	g_hash_table_foreach (clients, match_pid, &list);

	for (n = list; n; n = g_list_next (n)) {
		if (!shutdown_single ((gchar *)n->data))
			return FALSE;
	}

	g_list_free (list);
	return TRUE;
}
