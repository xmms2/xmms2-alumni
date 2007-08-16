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
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>

#include "management.h"
#include "config.h"
#include "utils.h"

/**
 * internal
 */

/**
 * Force a service client to shutdown.
 */
static gboolean
force_shutdown (gpointer data)
{
	GPid pid = GPOINTER_TO_INT (data);
	int status = 0;

	if (waitpid (pid, &status,
	             WNOHANG | WUNTRACED | WCONTINUED) == pid &&
	    (WIFEXITED (status) || WIFSIGNALED (status) || WIFSTOPPED (status))) {
		g_spawn_close_pid (pid);
		return FALSE;
	}

	if (pid > 0 && kill (pid, SIGTERM)) {
		print_info ("Failed to shutdown service client: %s", strerror (errno));
		return TRUE;
	}
	if (waitpid (pid, &status, WNOHANG | WUNTRACED | WCONTINUED) == pid) {
		g_spawn_close_pid (pid);
		return FALSE;
	}

	return TRUE;
}

static void
timeout_kill (gpointer key, gpointer value, gpointer data)
{
	gchar *name = key;
	config_t *config;

	if (!(config = g_hash_table_lookup (clients, name)))
		return;

	g_timeout_add (timeout, force_shutdown, GINT_TO_POINTER (config->pid));
}

/**
 * public
 */

/**
 * Change startup argument passed to the service client.
 */
void
cb_change_argv (xmmsc_connection_t *conn, xmmsc_result_t *res,
                xmmsc_service_method_t *method, void *data)
{
	config_t *config;
	gchar *name = NULL;
	gchar *new_argv = NULL;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_change_argv: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if ((config = lookup_client (res, &name, method))) {
		if (!xmmsc_result_get_dict_entry_string (res, ARG_ARGV, &new_argv))
			xmmsc_service_method_error_set (method,
			                                "New startup arguments not given.");
		else {
			if (config->argv)
				g_free (config->argv);
			config->argv = g_strdup (new_argv);

			xmmsc_service_method_ret_add_uint32 (method, ARG_RET, 1);
		}
	}

	method_return (conn, res, method);
}

/**
 * Start a service client.
 */
void
cb_launch (xmmsc_connection_t *conn, xmmsc_result_t *res,
           xmmsc_service_method_t *method, void *data)
{
	config_t *config;
	gchar *name = NULL;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_launch: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if ((config = lookup_client (res, &name, method))) {
		if (config->pid)
			xmmsc_service_method_error_set (method, "Service client is already"
			                                " running.");
		else if (!launch_single (config))
			xmmsc_service_method_error_set (method,
			                                "Failed to launch service client.");

		xmmsc_service_method_ret_add_uint32 (method, ARG_RET, 1);
	}

	method_return (conn, res, method);
}

/**
 * Shutdown a service client.
 */
void
cb_shutdown (xmmsc_connection_t *conn, xmmsc_result_t *res,
             xmmsc_service_method_t *method, void *data)
{
	config_t *config;
	gchar *name = NULL;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_shutdown: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if ((config = lookup_client (res, &name, method))) {
		if (config->pid && !shutdown_single (conn, name))
			xmmsc_service_method_error_set (method, "Failed to shutdown service"
			                                " client.");

		xmmsc_service_method_ret_add_uint32 (method, ARG_RET, 1);
	}

	method_return (conn, res, method);
}

/**
 * Toggle a service client's autostart.
 */
void
cb_toggle_autostart (xmmsc_connection_t *conn, xmmsc_result_t *res,
                     xmmsc_service_method_t *method, void *data)
{
	config_t *config;
	gchar *name = NULL;
	guint new_auto;

	if (xmmsc_result_iserror (res)) {
		print_error ("Error entering cb_toggle_autostart: %s",
		             xmmsc_result_get_error (res));
		return;
	}

	if ((config = lookup_client (res, &name, method))) {
		if (!xmmsc_result_get_dict_entry_uint (res, ARG_AUTO, &new_auto))
			xmmsc_service_method_error_set (method,
			                                "New autostart value not given.");
		else {
			config->autostart = new_auto;

			xmmsc_service_method_ret_add_uint32 (method, ARG_RET, 1);
		}
	}

	method_return (conn, res, method);
}

/**
 * Launch a single service client.
 */
gboolean
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
		                    argv, NULL, G_SPAWN_DO_NOT_REAP_CHILD,
		                    NULL, NULL, &config->pid, &err)) {
			print_info ("Failed to launch client (%s)", config->path);
			return FALSE;
		}
	}

	return TRUE;
}

/**
 * Shutdown a single service client.
 */
gboolean
shutdown_single (xmmsc_connection_t *conn, const gchar *client)
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

	if (!g_hash_table_find (config->services, match_service_registered, &name)) {
		print_info ("No service is registered from that service client.");
		return FALSE;
	}
	result = xmmsc_service_shutdown (conn, name);
	xmmsc_result_wait (result);
	if (xmmsc_result_iserror (result))
		print_error ("Failed to send shutdown request: %s",
		             xmmsc_result_get_error (result));
	xmmsc_result_unref (result);

	g_timeout_add (timeout, force_shutdown, GINT_TO_POINTER (config->pid));

	return TRUE;
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
shutdown_all (xmmsc_connection_t *conn)
{
	GList *list = NULL;
	GList *n;

	g_hash_table_foreach (clients, match_pid, &list);

	for (n = list; n; n = g_list_next (n)) {
		if (!shutdown_single (conn, (gchar *)n->data))
			return FALSE;
	}

	g_list_free (list);
	return TRUE;
}

/**
 * Shutdown all local service clients if they are still running after #timeout
 * seconds after the server shut down.
 */
void
kill_all (void)
{
	g_hash_table_foreach (clients, timeout_kill, NULL);
}
