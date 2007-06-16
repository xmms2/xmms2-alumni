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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "xmmsclient/xmmsclient.h"
#include "xmmsclientpriv/xmmsclient.h"
#include "xmmsc/xmmsc_idnumbers.h"


/**
 * @defgroup Service Service
 * @ingroup XMMSClient
 * @brief All modules related to service client.
 * The API to create and communicate with service clients;
 * please refer to the wiki for more infos on this.
 */


/**
 * @defgroup ServiceManagement ServiceManagement
 * @ingroup Service
 * @brief Functions to manage services on the server.
 * @{
 */

/**
 * Register a new method.
 *
 * If the service does not exist yet, it will be registered, too.
 *
 * @param conn The connection to the server.
 * @param service #xmmsc_service_t which the method belongs to.
 * @param method #xmmsc_service_method_t which contains the method's information.
 * @param user_data Optional data to pass to the method.
 */
xmmsc_result_t *
xmmsc_service_register (xmmsc_connection_t *conn,
                        xmmsc_service_t *service,
                        xmmsc_service_method_t *method,
                        void *user_data)
{
	return xmmsc_service_register_full (conn, service, method, user_data, NULL);
}

/**
 * Register a new method.
 *
 * If the service does not exist yet, it will be registered, too.
 *
 * @param conn The connection to the server.
 * @param service #xmmsc_service_t which the method belongs to.
 * @param method #xmmsc_service_method_t which contains the method's information.
 * @param user_data Optional data to pass to the method.
 * @param free_func Optional function that should be called to free user_data.
 */
xmmsc_result_t *
xmmsc_service_register_full (xmmsc_connection_t *conn,
                             xmmsc_service_t *service,
                             xmmsc_service_method_t *method,
                             void *user_data,
                             xmmsc_user_data_free_func_t free_func)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);
	x_api_error_if (!service, "with a NULL service", NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_REGISTER);
	xmms_ipc_msg_put_string (msg, service->name);
	xmms_ipc_msg_put_string (msg, service->description);
	xmms_ipc_msg_put_uint32 (msg, service->major_version);
	xmms_ipc_msg_put_uint32 (msg, service->minor_version);

	if (method) {
		xmms_ipc_msg_put_string (msg, method->name);
		xmms_ipc_msg_put_string (msg, method->description);
		xmms_ipc_msg_put_string (msg, method->ret_type);
		xmms_ipc_msg_put_string (msg, method->arg_type);
	}

	res = xmmsc_send_msg (conn, msg);

	if (xmmsc_result_iserror (res))
		return res;
	xmmsc_result_unref (res);

	res = xmmsc_send_broadcast_msg (conn, XMMS_IPC_SIGNAL_SERVICE);
	xmmsc_result_notifier_set_full (res, method->func, user_data, free_func);

	return res;
}

/**
 * Unregister an existing method.
 *
 * If the service does not have any methods left after removal of this method,
 * the service will be unregistered, too.
 *
 * @param conn The connection to the server.
 * @param service The name of the service which the method belongs to.
 * @param method The name of the method to be removed.
 */
xmmsc_result_t *
xmmsc_service_unregister (xmmsc_connection_t *conn,
                          char *service,
                          char *method)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);
	x_api_error_if (!service, "with a NULL service", NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_UNREGISTER);
	xmms_ipc_msg_put_string (msg, service);

	if (method)
		xmms_ipc_msg_put_string (msg, method);

	res = xmmsc_send_msg (conn, msg);

	return res;
}

/* @} */
