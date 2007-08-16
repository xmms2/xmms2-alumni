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
register_single (xmmsc_connection_t *conn, const gchar *service,
                 xmmsc_service_method_t *method,
                 xmmsc_user_data_free_func_t func)
{
	gchar *name = NULL;
	xmmsc_result_t *result;

	xmmsc_service_method_attribute_get (method, &name, NULL);

	result = xmmsc_service_method_register_full (conn, service, method, func);
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
register_all (xmmsc_connection_t *conn)
{
	xmmsc_service_method_t *method;
	xmmsc_result_t *result;

	result = xmmsc_service_register (conn, SERVICE_MANAGEMENT,
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
	                                   cb_launch, NULL);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_RET,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING)) {
		print_error ("Unable to push types");
		xmmsc_service_method_free (method);
		return FALSE;
	}
	if (!register_single (conn, SERVICE_MANAGEMENT, method, NULL))
		return FALSE;

	method = xmmsc_service_method_new ("shutdown", "Shutdown a service client",
	                                   cb_shutdown, NULL);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_RET,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING)) {
		print_error ("Unable to push types");
		xmmsc_service_method_free (method);
		return FALSE;
	}
	if (!register_single (conn, SERVICE_MANAGEMENT, method, NULL))
		return FALSE;

	method = xmmsc_service_method_new ("change_argv", "Change the command-line"
	                                   " argument to pass to a service client",
	                                   cb_change_argv, NULL);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_arg_type_add (method, ARG_ARGV,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_RET,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING)) {
		print_error ("Unable to push types");
		xmmsc_service_method_free (method);
		return FALSE;
	}
	if (!register_single (conn, SERVICE_MANAGEMENT, method, NULL))
		return FALSE;

	method = xmmsc_service_method_new ("toggle_autostart", "Toggle the autostart"
	                                   " property of a service client",
	                                   cb_toggle_autostart, NULL);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_arg_type_add (method, ARG_AUTO,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_RET,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING)) {
		print_error ("Unable to push types");
		xmmsc_service_method_free (method);
		return FALSE;
	}
	if (!register_single (conn, SERVICE_MANAGEMENT, method, NULL))
		return FALSE;

	/* Query methods */
	result = xmmsc_service_register (conn, SERVICE_QUERY, "Query information"
	                                 " from service client manager.",
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
	                                   cb_list_sc_ids, NULL);
	if (!xmmsc_service_method_ret_type_add (method, ARG_IDS,
	                                        XMMSC_SERVICE_ARG_TYPE_STRINGLIST)) {
		print_error ("Unable to push types");
		xmmsc_service_method_free (method);
		return FALSE;
	}
	if (!register_single (conn, SERVICE_QUERY, method, NULL))
		return FALSE;

	method = xmmsc_service_method_new ("service_ids", "List the names of"
	                                   " all services of a service client",
	                                   cb_list_service_ids, NULL);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_IDS,
	                                        XMMSC_SERVICE_ARG_TYPE_STRINGLIST)) {
		print_error ("Unable to push types");
		xmmsc_service_method_free (method);
		return FALSE;
	}
	if (!register_single (conn, SERVICE_QUERY, method, NULL))
		return FALSE;

	method = xmmsc_service_method_new ("method_ids", "List the names of"
	                                   " all methods of a service",
	                                   cb_list_method_ids, NULL);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_arg_type_add (method, ARG_SERVICE_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_IDS,
	                                        XMMSC_SERVICE_ARG_TYPE_STRINGLIST)) {
		print_error ("Unable to push types");
		xmmsc_service_method_free (method);
		return FALSE;
	}
	if (!register_single (conn, SERVICE_QUERY, method, NULL))
		return FALSE;

	method = xmmsc_service_method_new ("lookup_client", "Search for all service"
	                                   " clients which provide the given"
	                                   " service",
	                                   cb_lookup_client, NULL);
	if (!xmmsc_service_method_arg_type_add (method, ARG_SERVICE_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_IDS,
	                                        XMMSC_SERVICE_ARG_TYPE_STRINGLIST)) {
		print_error ("Unable to push types");
		xmmsc_service_method_free (method);
		return FALSE;
	}
	if (!register_single (conn, SERVICE_QUERY, method, NULL))
		return FALSE;

	method = xmmsc_service_method_new ("sc", "List details of an installed"
	                                   " service client",
	                                   cb_list_sc, NULL);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_ARGV,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_AUTO,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_SERVICES,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32)) {
		print_error ("Unable to push types");
		xmmsc_service_method_free (method);
		return FALSE;
	}
	if (!register_single (conn, SERVICE_QUERY, method, NULL))
		return FALSE;

	method = xmmsc_service_method_new ("service", "List details of a"
	                                   " service",
	                                   cb_list_service, NULL);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_arg_type_add (method, ARG_SERVICE_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_DESC,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_MAJOR,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_MINOR,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_REGISTERED,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_METHODS,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32)) {
		print_error ("Unable to push types");
		xmmsc_service_method_free (method);
		return FALSE;
	}
	if (!register_single (conn, SERVICE_QUERY, method, NULL))
		return FALSE;

	method = xmmsc_service_method_new ("method", "List details of a method",
	                                   cb_list_method, NULL);
	if (!xmmsc_service_method_arg_type_add (method, ARG_CLIENT_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_arg_type_add (method, ARG_SERVICE_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_arg_type_add (method, ARG_METHOD_NAME,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_DESC,
	                                        XMMSC_SERVICE_ARG_TYPE_STRING) ||
	    !xmmsc_service_method_ret_type_add (method, ARG_REGISTERED,
	                                        XMMSC_SERVICE_ARG_TYPE_UINT32)) {
		print_error ("Unable to push types");
		xmmsc_service_method_free (method);
		return FALSE;
	}
	if (!register_single (conn, SERVICE_QUERY, method, NULL))
		return FALSE;

	return TRUE;
}

static gboolean
timeout_quit (void *data)
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
cb_quit (void *data)
{
	kill_all ();
	if (timeout_quit (data))
		g_timeout_add (timeout / 5, timeout_quit, data);
}

static gboolean
subscribe_broadcasts (xmmsc_connection_t *conn, GMainLoop *ml)
{
	xmmsc_disconnect_callback_set (conn, cb_quit, ml);

	XMMS_CALLBACK_SET (conn, xmmsc_broadcast_service_changed,
	                   cb_service_changed, conn);
	XMMS_CALLBACK_SET (conn, xmmsc_broadcast_service_method_changed,
	                   cb_method_changed, conn);

	return TRUE;
}

static void
quit (xmmsc_connection_t *conn, GIOChannel *gio)
{
	if (monitor)
		shutdown_monitor (gio);
	g_hash_table_destroy (clients);
	xmmsc_unref (conn);
}

int
main (void)
{
	xmmsc_connection_t *conn;
	GMainLoop *ml;
	GIOChannel *gio;
	xmmsc_result_t *res;
	clients = NULL;
	monitor = FALSE;

	conn = xmmsc_init (SCM_NAME);
	if (!conn)
		print_error_and_exit (conn, "Unable to initialize connection.");
	if (!xmmsc_connect (conn, getenv ("XMMS_PATH")))
		print_error_and_exit (conn, "Unable to connect to server.");

	res = xmmsc_configval_register (conn, CONFIGVAL_TIMEOUT, "30000");
	xmmsc_result_notifier_set (res, cb_configval, conn);
	xmmsc_result_unref (res);
	res = xmmsc_configval_register (conn, CONFIGVAL_MONITOR, "no");
	xmmsc_result_notifier_set (res, cb_configval, conn);
	xmmsc_result_unref (res);
	res = xmmsc_broadcast_configval_changed (conn);
	xmmsc_result_notifier_set (res, cb_configval_changed, NULL);
	xmmsc_result_unref (res);

	ml = g_main_loop_new (NULL, FALSE);

	if (!subscribe_broadcasts (conn, ml)) {
		quit (conn, NULL);
		return EXIT_FAILURE;
	}

	if (!read_all () || !launch_all ()) {
		quit (conn, NULL);
		return EXIT_FAILURE;
	}

	if (!register_all (conn)) {
		quit (conn, NULL);
		return EXIT_FAILURE;
	}

	if (monitor) {
		if (!(gio = start_monitor (conn))) {
			quit (conn, NULL);
			return EXIT_FAILURE;
		}
	}

	xmmsc_mainloop_gmain_init (conn);
	g_main_loop_run (ml);

	quit (conn, gio);

	return EXIT_SUCCESS;
}
