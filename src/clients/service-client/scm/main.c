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

#include "common.h"
#include "config.h"
#include "management.h"
#include "query.h"
#include "callback.h"
#include "monitor.h"

static gboolean
register_single (info_t *info, const gchar *service,
                 xmmsc_service_method_t *method)
{
	gchar *name = NULL;
	xmmsc_result_t *result;

	xmmsc_service_method_attribute_get (method, &name, NULL);

	result = xmmsc_service_method_register (info->conn, service, method);
	if (xmmsc_result_iserror (result)) {
		print_error ("Unable to register method (%s): %s", name,
		             xmmsc_result_get_error (result));
		xmmsc_result_unref (result);
		return FALSE;
	}
	xmmsc_result_unref (result);

	return TRUE;
}

static gboolean
register_all (info_t *info)
{
	xmmsc_service_method_t *method;
	xmmsc_result_t *result;

	result = xmmsc_service_register (info->conn, SERVICE_MANAGEMENT,
	                                 "A simple service client manager.",
	                                 0, 1);
	if (xmmsc_result_iserror (result)) {
		print_error ("Unable to register service (%s): %s", SERVICE_MANAGEMENT,
		             xmmsc_result_get_error (result));
		xmmsc_result_unref (result);
		return FALSE;
	}
	xmmsc_result_unref (result);

	method = xmmsc_service_method_new ("launch", "Launch a service client",
	                                   cb_launch, info->clients);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_RET,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32, 0)) {
		print_error ("Unable to push types");
		xmmsc_service_method_unref (method);
		return FALSE;
	}
	if (!register_single (info, SERVICE_MANAGEMENT, method)) {
		return FALSE;
	}

	method = xmmsc_service_method_new ("shutdown", "Shutdown a service client",
	                                   cb_shutdown, NULL);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_RET,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32, 0)) {
		print_error ("Unable to push types");
		xmmsc_service_method_unref (method);
		return FALSE;
	}
	if (!register_single (info, SERVICE_MANAGEMENT, method)) {
		return FALSE;
	}

	method = xmmsc_service_method_new ("change_argv", "Change the command-line"
	                                   " argument to pass to a service client",
	                                   cb_change_argv, info->clients);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_arg_type_add (method, ARG_ARGV,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_RET,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32, 0)) {
		print_error ("Unable to push types");
		xmmsc_service_method_unref (method);
		return FALSE;
	}
	if (!register_single (info, SERVICE_MANAGEMENT, method)) {
		return FALSE;
	}

	method = xmmsc_service_method_new ("toggle_autostart", "Toggle the autostart"
	                                   " property of a service client",
	                                   cb_toggle_autostart, info->clients);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_arg_type_add (method, ARG_AUTO,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_RET,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32, 0)) {
		print_error ("Unable to push types");
		xmmsc_service_method_unref (method);
		return FALSE;
	}
	if (!register_single (info, SERVICE_MANAGEMENT, method)) {
		return FALSE;
	}

	/* Query methods */
	result = xmmsc_service_register (info->conn, SERVICE_QUERY, "Query"
	                                 " information from service client manager.",
	                                 0, 1);
	if (xmmsc_result_iserror (result)) {
		print_error ("Unable to register service (%s): %s", SERVICE_QUERY,
		             xmmsc_result_get_error (result));
		xmmsc_result_unref (result);
		return FALSE;
	}
	xmmsc_result_unref (result);

	method = xmmsc_service_method_new ("sc_ids", "List the names of all"
	                                   " installed service clients",
	                                   cb_list_sc_ids, info->clients);
	if (!xmmsc_service_method_ret_type_add (method, ARG_IDS,
	                                        XMMSC_SERVICE_ARG_TYPE_STRINGLIST,
	                                        1)) {
		print_error ("Unable to push types");
		xmmsc_service_method_unref (method);
		return FALSE;
	}
	if (!register_single (info, SERVICE_QUERY, method)) {
		return FALSE;
	}

	method = xmmsc_service_method_new ("service_ids", "List the names of"
	                                   " all services of a service client",
	                                   cb_list_service_ids, info->clients);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_IDS,
	                                        XMMSC_SERVICE_ARG_TYPE_STRINGLIST,
	                                        0)) {
		print_error ("Unable to push types");
		xmmsc_service_method_unref (method);
		return FALSE;
	}
	if (!register_single (info, SERVICE_QUERY, method)) {
		return FALSE;
	}

	method = xmmsc_service_method_new ("method_ids", "List the names of"
	                                   " all methods of a service",
	                                   cb_list_method_ids, info->clients);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_arg_type_add (method, ARG_SERVICE_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_IDS,
	                                        XMMSC_SERVICE_ARG_TYPE_STRINGLIST,
	                                        0)) {
		print_error ("Unable to push types");
		xmmsc_service_method_unref (method);
		return FALSE;
	}
	if (!register_single (info, SERVICE_QUERY, method)) {
		return FALSE;
	}

	method = xmmsc_service_method_new ("lookup_client", "Search for all service"
	                                   " clients which provide the given"
	                                   " service",
	                                   cb_lookup_client, info->clients);
	if (!xmmsc_service_method_arg_type_add (method, ARG_SERVICE_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_IDS,
	                                        XMMSC_SERVICE_ARG_TYPE_STRINGLIST,
	                                        0)) {
		print_error ("Unable to push types");
		xmmsc_service_method_unref (method);
		return FALSE;
	}
	if (!register_single (info, SERVICE_QUERY, method)) {
		return FALSE;
	}

	method = xmmsc_service_method_new ("sc", "List details of an installed"
	                                   " service client",
	                                   cb_list_sc, info->clients);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_ARGV,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_AUTO,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_SERVICES,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32, 0)) {
		print_error ("Unable to push types");
		xmmsc_service_method_unref (method);
		return FALSE;
	}
	if (!register_single (info, SERVICE_QUERY, method)) {
		return FALSE;
	}

	method = xmmsc_service_method_new ("service", "List details of a"
	                                   " service",
	                                   cb_list_service, info->clients);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_arg_type_add (method, ARG_SERVICE_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_DESC,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_MAJOR,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_MINOR,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_REGISTERED,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_METHODS,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32, 0)) {
		print_error ("Unable to push types");
		xmmsc_service_method_unref (method);
		return FALSE;
	}
	if (!register_single (info, SERVICE_QUERY, method)) {
		return FALSE;
	}

	method = xmmsc_service_method_new ("method", "List details of a method",
	                                   cb_list_method, info->clients);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_arg_type_add (method, ARG_SERVICE_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_arg_type_add (method, ARG_METHOD_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_DESC,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING, 0) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_REGISTERED,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32, 0)) {
		print_error ("Unable to push types");
		xmmsc_service_method_unref (method);
		return FALSE;
	}
	if (!register_single (info, SERVICE_QUERY, method)) {
		return FALSE;
	}

	/* Miscellaneous methods */
	result = xmmsc_service_register (info->conn, SERVICE_MISC,
	                                 "Miscellaneous methods",
	                                 0, 1);
	if (xmmsc_result_iserror (result)) {
		print_error ("Unable to register service (%s): %s", SERVICE_MISC,
		             xmmsc_result_get_error (result));
		xmmsc_result_unref (result);
		return FALSE;
	}
	xmmsc_result_unref (result);

	method = xmmsc_service_method_new ("poll", "Force the manager to check for"
	                                   " any changes in config dir",
	                                   cb_poll, info->clients);
	if (!register_single (info, SERVICE_MISC, method)) {
		return FALSE;
	}

	return TRUE;
}

static gboolean
timeout_quit (gpointer data)
{
	if (waitpid (-1, NULL, WNOHANG | WUNTRACED | WCONTINUED) == -1) {
		if (errno == ECHILD) {
			g_main_loop_quit ((GMainLoop *)data);
			return FALSE;
		}
	}

	return TRUE;
}

static void
cb_quit (gpointer data)
{
	kill_all (((info_t *)data)->clients);
	if (timeout_quit (((info_t *)data)->ml)) {
		g_timeout_add (timeout / 5, timeout_quit, ((info_t *)data)->ml);
	}
}

static gboolean
subscribe_broadcasts (info_t *info)
{
	xmmsc_disconnect_callback_set (info->conn, cb_quit, info);

	XMMS_CALLBACK_SET (info->conn, xmmsc_broadcast_service_changed,
	                   cb_service_changed, info);
	XMMS_CALLBACK_SET (info->conn, xmmsc_broadcast_service_method_changed,
	                   cb_method_changed, info);
	XMMS_CALLBACK_SET (info->conn, xmmsc_broadcast_configval_changed,
	                   cb_configval_changed, info->clients);

	return TRUE;
}

static void
quit (info_t *info)
{
	if (monitor) {
		shutdown_monitor ();
	}
	g_hash_table_destroy (info->clients);
	g_main_loop_unref (info->ml);
	xmmsc_unref (info->conn);
}

int
main (void)
{
	info_t info;
	xmmsc_result_t *res;
	info.clients = NULL;
	monitor = FALSE;

	info.conn = xmmsc_init (SCM_NAME);
	if (!info.conn) {
		print_error_and_exit (&info, "Unable to initialize connection.");
	}
	if (!xmmsc_connect (info.conn, getenv ("XMMS_PATH"))) {
		print_error_and_exit (&info, "Unable to connect to server.");
	}

	info.ml = g_main_loop_new (NULL, FALSE);

	if (!subscribe_broadcasts (&info)) {
		quit (&info);
		return EXIT_FAILURE;
	}

	if (!read_all (&info) || !launch_all (&info)) {
		quit (&info);
		return EXIT_FAILURE;
	}

	res = xmmsc_configval_register (info.conn, CONFIGVAL_TIMEOUT, "10000");
	xmmsc_result_notifier_set (res, cb_configval, &info);
	xmmsc_result_unref (res);
	res = xmmsc_configval_register (info.conn, CONFIGVAL_PERIOD, "10000");
	xmmsc_result_notifier_set (res, cb_configval, &info);
	xmmsc_result_unref (res);
	res = xmmsc_configval_register (info.conn, CONFIGVAL_MONITOR, "no");
	xmmsc_result_notifier_set (res, cb_configval, &info);
	xmmsc_result_unref (res);

	if (!register_all (&info)) {
		quit (&info);
		return EXIT_FAILURE;
	}

	if (monitor) {
		if (!start_monitor (info.clients)) {
			quit (&info);
			return EXIT_FAILURE;
		}
	}

	xmmsc_mainloop_gmain_init (info.conn);
	g_main_loop_run (info.ml);

	quit (&info);

	return EXIT_SUCCESS;
}
