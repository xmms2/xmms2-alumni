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

static gboolean
register_single (xmmsc_service_t *service, xmmsc_service_method_t *method,
                 void *data)
{
	gchar *name = NULL;
	xmmsc_result_t *result;

	xmmsc_service_method_attribute_get (method, "name", &name);

	result = xmmsc_service_register (conn, service, method, data);
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

	args = xmmsc_service_args_new (1, "client_name",
	                               XMMSC_SERVICE_ARG_TYPE_STRING);
	ret = xmmsc_service_args_new (1, "ret", XMMSC_SERVICE_ARG_TYPE_UINT32);

	method = xmmsc_service_method_new ("uninstall", "Uninstall a service client",
	                                   ret, args, cb_uninstall);
	if (!register_single (service, method, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	method = xmmsc_service_method_new ("launch", "Launch a service client",
	                                   ret, args, cb_launch);
	if (!register_single (service, method, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	method = xmmsc_service_method_new ("shutdown", "Shutdown a service client",
	                                   ret, args, cb_shutdown);
	if (!register_single (service, method, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	args = xmmsc_service_args_new (1, "client_name",
	                               XMMSC_SERVICE_ARG_TYPE_STRING,
	                               "argv", XMMSC_SERVICE_ARG_TYPE_STRING);
	method = xmmsc_service_method_new ("change_argv", "Change the startup"
	                                   " argument of a service client",
	                                   ret, args, cb_change_argv);
	if (!register_single (service, method, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	args = xmmsc_service_args_new (1, "client_name",
	                               XMMSC_SERVICE_ARG_TYPE_STRING,
	                               "auto", XMMSC_SERVICE_ARG_TYPE_UINT32);
	method = xmmsc_service_method_new ("toggle_autostart", "Toggle the autostart"
	                                   " property of a service client",
	                                   ret, args, cb_toggle_autostart);
	if (!register_single (service, method, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	xmmsc_service_args_free (ret);
	xmmsc_service_free (service);

	/* Query methods */
	service = xmmsc_service_new ("scm_query",
	                             "Query information from service client manager.",
	                             0, 1);

	args = xmmsc_service_args_new (0);
	ret = xmmsc_service_args_new (1, "ids", XMMSC_SERVICE_ARG_TYPE_STRINGLIST);

	method = xmmsc_service_method_new ("sc_ids", "List the names of all"
	                                   " installed service clients",
	                                   ret, args, cb_list_sc_ids);
	if (!register_single (service, method, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	args = xmmsc_service_args_new (1, "client_name",
	                               XMMSC_SERVICE_ARG_TYPE_STRING);
	method = xmmsc_service_method_new ("service_ids", "List the names of"
	                                   " all services of a service client",
	                                   ret, args, cb_list_service_ids);
	if (!register_single (service, method, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);

	args = xmmsc_service_args_new (2, "client_name",
	                               XMMSC_SERVICE_ARG_TYPE_STRING,
	                               "service_name",
	                               XMMSC_SERVICE_ARG_TYPE_STRING);
	method = xmmsc_service_method_new ("method_ids", "List the names of"
	                                   " all methods of a service",
	                                   ret, args, cb_list_method_ids);
	if (!register_single (service, method, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);
	xmmsc_service_args_free (ret);

	args = xmmsc_service_args_new (1, "client_name",
	                               XMMSC_SERVICE_ARG_TYPE_STRING);
	ret = xmmsc_service_args_new (3, "argv", XMMSC_SERVICE_ARG_TYPE_STRING,
	                              "auto", XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              "services", XMMSC_SERVICE_ARG_TYPE_UINT32);
	method = xmmsc_service_method_new ("sc", "List details of an installed"
	                                   " service client",
	                                   ret, args, cb_list_sc);
	if (!register_single (service, method, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);
	xmmsc_service_args_free (ret);

	args = xmmsc_service_args_new (2, "client_name",
	                               XMMSC_SERVICE_ARG_TYPE_STRING,
	                               "service_name",
	                               XMMSC_SERVICE_ARG_TYPE_STRING);
	ret = xmmsc_service_args_new (5, "desc", XMMSC_SERVICE_ARG_TYPE_STRING,
	                              "major", XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              "minor", XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              "registered", XMMSC_SERVICE_ARG_TYPE_UINT32,
	                              "methods", XMMSC_SERVICE_ARG_TYPE_UINT32);
	method = xmmsc_service_method_new ("service", "List details of a"
	                                   " service",
	                                   ret, args, cb_list_service);
	if (!register_single (service, method, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);
	xmmsc_service_args_free (ret);

	args = xmmsc_service_args_new (3, "client_name",
	                               XMMSC_SERVICE_ARG_TYPE_STRING,
	                               "service_name",
	                               XMMSC_SERVICE_ARG_TYPE_STRING,
	                               "method_name",
	                               XMMSC_SERVICE_ARG_TYPE_STRING);
	ret = xmmsc_service_args_new (1, "desc", XMMSC_SERVICE_ARG_TYPE_STRING);
	method = xmmsc_service_method_new ("method", "List details of a method",
	                                   ret, args, cb_list_method);
	if (!register_single (service, method, NULL))
		return FALSE;
	xmmsc_service_method_free (method);
	xmmsc_service_args_free (args);
	xmmsc_service_args_free (ret);

	xmmsc_service_free (service);

	return TRUE;
}

int
main ()
{
	GMainLoop *ml;

	conn = xmmsc_init ("scm");
	if (!conn)
		print_error_and_exit ("Unable to initialize connection.");
	if (!xmmsc_connect (conn, getenv ("XMMS_PATH")))
		print_error_and_exit ("Unable to connect to server.");

	if (!register_all ()) {
		xmmsc_unref (conn);
		return 1;
	}

	ml = g_main_loop_new (NULL, FALSE);
	xmmsc_mainloop_gmain_init (conn);
	g_main_loop_run (ml);

	xmmsc_unref (conn);

	return 0;
}
