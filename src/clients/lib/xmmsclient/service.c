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
	xmmsc_service_argument_t *arg = NULL;
	uint32_t count;

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

		xmms_ipc_msg_put_uint32 (msg, method->num_rets);
		for (count = 0; count < method->num_rets; count++) {
			arg = method->rets + count;
			xmms_ipc_msg_put_string (msg, arg->name);
			xmms_ipc_msg_put_uint32 (msg, arg->type);
			xmms_ipc_msg_put_uint32 (msg, arg->optional);
		}

		xmms_ipc_msg_put_uint32 (msg, method->num_args);
		for (count = 0; count < method->num_args; count++) {
			arg = method->args + count;
			xmms_ipc_msg_put_string (msg, arg->name);
			xmms_ipc_msg_put_uint32 (msg, arg->type);
			xmms_ipc_msg_put_uint32 (msg, arg->optional);
		}
	}

	res = xmmsc_send_msg (conn, msg);

	if (xmmsc_result_iserror (res))
		return res;

	if (method) {
		xmmsc_result_restartable (res, XMMS_IPC_SIGNAL_SERVICE);
		xmmsc_result_notifier_set_full (res, method->func, user_data, free_func);
	}

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

/**
 * List available service ids.
 *
 * The list can be used to further query for service details.
 *
 * @param conn The connection to the server.
 */
xmmsc_result_t *
xmmsc_service_list_service_ids (xmmsc_connection_t *conn)
{
	x_check_conn (conn, NULL);

	return xmmsc_send_msg_no_arg (conn, XMMS_IPC_OBJECT_SERVICE,
	                              XMMS_IPC_CMD_SERVICE_LIST);
}

/**
 * List a service's details.
 *
 * @param conn The connection to the server.
 * @param service The id of the service.
 */
xmmsc_result_t *
xmmsc_service_list_service (xmmsc_connection_t *conn, char *service)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE, XMMS_IPC_CMD_SERVICE_LIST);
	xmms_ipc_msg_put_string (msg, service);

	return xmmsc_send_msg (conn, msg);
}

/**
 * List available method ids of a given service.
 *
 * @param conn The connection to the server.
 * @param service The id of the service.
 */
xmmsc_result_t *
xmmsc_service_list_method_ids (xmmsc_connection_t *conn, char *service)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_LIST_METHOD);
	xmms_ipc_msg_put_string (msg, service);

	return xmmsc_send_msg (conn, msg);
}

/**
 * List a method's details not including method's arguments.
 *
 * @param conn The connection to the server.
 * @param service The id of the service.
 * @param method The id of the method.
 */
xmmsc_result_t *
xmmsc_service_list_method (xmmsc_connection_t *conn, char *service, char *method)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_LIST_METHOD);
	xmms_ipc_msg_put_string (msg, service);
	xmms_ipc_msg_put_string (msg, method);
	xmms_ipc_msg_put_uint32 (msg, 0);

	return xmmsc_send_msg (conn, msg);
}

/**
 * List a method's arguments.
 *
 * @param conn The connection to the server.
 * @param service The id of the service.
 * @param method The id of the method.
 */
xmmsc_result_t *
xmmsc_service_list_method_args (xmmsc_connection_t *conn, char *service,
                                char *method)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_LIST_METHOD);
	xmms_ipc_msg_put_string (msg, service);
	xmms_ipc_msg_put_string (msg, method);
	xmms_ipc_msg_put_uint32 (msg, 1);

	return xmmsc_send_msg (conn, msg);
}

/**
 * Parse a method's argument types.
 *
 * @param res The #xmmsc_result_t returned by #xmmsc_service_list_method_args.
 * @param num The total number of arguments this method has.
 * @return The list of #xmmsc_service_argument_t. Caller is responsible for freeing the list. Elements are owned by result.
 */
xmmsc_service_argument_t *
xmmsc_service_parse_arg_types (xmmsc_result_t *res, uint32_t num)
{
	xmmsc_service_argument_t *ret = NULL;

	if (xmmsc_result_iserror (res) || !xmmsc_result_list_valid (res) || num <= 0)
		return ret;

	ret = x_new0 (xmmsc_service_argument_t, num);

	while (xmmsc_result_list_valid (res) && num-- > 0) {
		if (!xmmsc_result_get_dict_entry_string (res, "name", &ret[num].name))
			goto err;
		if (!xmmsc_result_get_dict_entry_uint (res, "type", &ret[num].type))
			goto err;
		if (!xmmsc_result_get_dict_entry_uint (res, "optional",
		                                       &ret[num].optional))
			goto err;

		xmmsc_result_list_next (res);
	}

	return ret;

err:
	free (ret);
	return NULL;
}

/* @} */
