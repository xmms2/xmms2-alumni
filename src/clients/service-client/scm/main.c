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

#include "common.h"
#include "config.h"
#include "management.h"
#include "query.h"
#include "callback.h"

static void
free_args (void *data)
{
	xmmsc_service_args_free ((xmmsc_service_arg_list_t *)data);
}

static gboolean
register_single (xmmsc_service_t *service, xmmsc_service_method_t *method,
                 gpointer data, xmmsc_user_data_free_func_t func)
{
	gchar *name = NULL;
	xmmsc_result_t *result;

	xmmsc_service_method_attribute_get (method, "name", &name);

	result = xmmsc_service_register_full (conn, service, method, data, func);
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
register_all (void)
{
	xmmsc_service_t *service;
	xmmsc_service_method_t *method;
	xmmsc_service_arg_list_t *args;
	xmmsc_service_arg_list_t *ret;

	service = xmmsc_service_new ("scm_management",
	                             "A simple service client manager.",
	                             0, 1);

	args = xmmsc_service_args_new (1, ARG_CLIENT_NAME,
	                               XMMSC_SERVICE_ARG_TYPE_STRING);
	ret = xmmsc_service_args_new (1, ARG_RET, XMMSC_SERVICE_ARG_TYPE_UINT32);

	method = xmmsc_service_method_new ("uninstall", "Uninstall a service client",
	                                   ret, args, cb_uninstall);
	if (!register_single (service, method, ret, free_args))
		return FALSE;
	xmmsc_service_method_free (method);
	method = xmmsc_service_method_new ("launch", "Launch a service client",
	                                   ret, args, cb_launch);
	if (!register_single (service, method, ret, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	method = xmmsc_service_method_new ("shutdown", "Shutdown a service client",
	                                   ret, args, cb_shutdown);
	if (!register_single (service, method, ret, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	args = xmmsc_service_args_new (1, ARG_CLIENT_NAME,
	                               XMMSC_SERVICE_ARG_TYPE_STRING,
	                               ARG_ARGV, XMMSC_SERVICE_ARG_TYPE_STRING);
	method = xmmsc_service_method_new ("change_argv", "Change the startup"
	                                   " argument of a service client",
	                                   ret, args, cb_change_argv);
	if (!register_single (service, method, ret, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	args = xmmsc_service_args_new (1, ARG_CLIENT_NAME,
	                               XMMSC_SERVICE_ARG_TYPE_STRING,
	                               ARG_AUTO, XMMSC_SERVICE_ARG_TYPE_UINT32);
	method = xmmsc_service_method_new ("toggle_autostart", "Toggle the autostart"
	                                   " property of a service client",
	                                   ret, args, cb_toggle_autostart);
	if (!register_single (service, method, ret, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	xmmsc_service_free (service);

	/* Query methods */
	service = xmmsc_service_new ("scm_query",
	                             "Query information from service client manager.",
	                             0, 1);

	args = xmmsc_service_args_new (0);
	ret = xmmsc_service_args_new (1, ARG_IDS, XMMSC_SERVICE_ARG_TYPE_STRINGLIST);

	method = xmmsc_service_method_new ("sc_ids", "List the names of all"
	                                   " installed service clients",
	                                   ret, args, cb_list_sc_ids);
	if (!register_single (service, method, ret, free_args))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	args = xmmsc_service_args_new (1, ARG_CLIENT_NAME,
	                               XMMSC_SERVICE_ARG_TYPE_STRING);
	method = xmmsc_service_method_new ("service_ids", "List the names of"
	                                   " all services of a service client",
	                                   ret, args, cb_list_service_ids);
	if (!register_single (service, method, ret, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	args = xmmsc_service_args_new (2, ARG_CLIENT_NAME,
	                               XMMSC_SERVICE_ARG_TYPE_STRING,
	                               ARG_SERVICE_NAME,
	                               XMMSC_SERVICE_ARG_TYPE_STRING);
	method = xmmsc_service_method_new ("method_ids", "List the names of"
	                                   " all methods of a service",
	                                   ret, args, cb_list_method_ids);
	if (!register_single (service, method, ret, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	args = xmmsc_service_args_new (1, ARG_SERVICE_NAME,
	                               XMMSC_SERVICE_ARG_TYPE_STRING);
	method = xmmsc_service_method_new ("lookup_client", "Search for all service"
	                                   " clients which provide a service",
	                                   ret, args, cb_lookup_client);
	if (!register_single (service, method, ret, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	args = xmmsc_service_args_new (1, ARG_CLIENT_NAME,
	                               XMMSC_SERVICE_ARG_TYPE_STRING);
	ret = xmmsc_service_args_new (3, ARG_ARGV, XMMSC_SERVICE_ARG_TYPE_STRING,
	                              ARG_AUTO, XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              ARG_SERVICES, XMMSC_SERVICE_ARG_TYPE_UINT32);
	method = xmmsc_service_method_new ("sc", "List details of an installed"
	                                   " service client",
	                                   ret, args, cb_list_sc);
	if (!register_single (service, method, ret, free_args))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	args = xmmsc_service_args_new (2, ARG_CLIENT_NAME,
	                               XMMSC_SERVICE_ARG_TYPE_STRING,
	                               ARG_SERVICE_NAME,
	                               XMMSC_SERVICE_ARG_TYPE_STRING);
	ret = xmmsc_service_args_new (5, ARG_DESC, XMMSC_SERVICE_ARG_TYPE_STRING,
	                              ARG_MAJOR, XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              ARG_MINOR, XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              ARG_REGISTERED, XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              ARG_METHODS, XMMSC_SERVICE_ARG_TYPE_UINT32);
	method = xmmsc_service_method_new ("service", "List details of a"
	                                   " service",
	                                   ret, args, cb_list_service);
	if (!register_single (service, method, ret, free_args))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	args = xmmsc_service_args_new (3, ARG_CLIENT_NAME,
	                               XMMSC_SERVICE_ARG_TYPE_STRING,
	                               ARG_SERVICE_NAME,
	                               XMMSC_SERVICE_ARG_TYPE_STRING,
	                               ARG_METHOD_NAME,
	                               XMMSC_SERVICE_ARG_TYPE_STRING);
	ret = xmmsc_service_args_new (1, ARG_DESC, XMMSC_SERVICE_ARG_TYPE_STRING);
	method = xmmsc_service_method_new ("method", "List details of a method",
	                                   ret, args, cb_list_method);
	if (!register_single (service, method, ret, free_args))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	xmmsc_service_free (service);

	return TRUE;
}

static void
cb_quit (xmmsc_result_t *res, void *data)
{
	g_main_loop_quit ((GMainLoop *) data);
}

static gboolean
subscribe_broadcasts (GMainLoop *ml)
{
	XMMS_CALLBACK_SET (conn, xmmsc_broadcast_quit, cb_quit, ml);

	XMMS_CALLBACK_SET (conn, xmmsc_broadcast_service_changed,
	                   cb_service_changed, NULL);
	XMMS_CALLBACK_SET (conn, xmmsc_broadcast_service_method_changed,
	                   cb_method_changed, NULL);

	return TRUE;
}

static void
quit (void)
{
	shutdown_all ();
	g_hash_table_destroy (clients);
	xmmsc_unref (conn);
}

int
main ()
{
	GMainLoop *ml;
	clients = NULL;

	conn = xmmsc_init (SCM_NAME);
	if (!conn)
		print_error_and_exit ("Unable to initialize connection.");
	if (!xmmsc_connect (conn, getenv ("XMMS_PATH")))
		print_error_and_exit ("Unable to connect to server.");

	ml = g_main_loop_new (NULL, FALSE);

	if (!subscribe_broadcasts (ml)) {
		quit ();
		return 1;
	}

	if (!read_all () || !launch_all ()) {
		quit ();
		return 1;
	}

	if (!register_all ()) {
		quit ();
		return 1;
	}

	xmmsc_mainloop_gmain_init (conn);
	g_main_loop_run (ml);

	quit ();

	return 0;
}
