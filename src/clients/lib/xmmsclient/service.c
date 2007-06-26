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
 * Parse a service's information.
 *
 * Caller is responsible for freeing the list and all the elements, or simply
 * call the helper function #xmmsc_service_free.
 * 
 * @param res The #xmmsc_result_t returned by #xmmsc_service_list.
 * @return The #xmmsc_service_t structure or NULL if failed.
 */
xmmsc_service_t *
xmmsc_service_parse_service (xmmsc_result_t *res)
{
	char *name = NULL;
	char *desc = NULL;
	xmmsc_service_t *ret = NULL;

	if (xmmsc_result_iserror (res))
		return ret;

	ret = x_new0 (xmmsc_service_t, 1);

	if (!xmmsc_result_get_dict_entry_string (res, "name", &name))
		goto err;
	if (!xmmsc_result_get_dict_entry_string (res, "description", &desc))
		goto err;
	if (!xmmsc_result_get_dict_entry_uint (res, "major_version",
	                                       &ret->major_version))
		goto err;
	if (!xmmsc_result_get_dict_entry_uint (res, "minor_version",
										   &ret->minor_version))
		goto err;
	if (!xmmsc_result_get_dict_entry_uint (res, "count", &ret->count))
		goto err;
	ret->name = x_new0 (char, strlen (name) + 1);
	strcpy (ret->name, name);
	ret->description = x_new0 (char, strlen (desc) + 1);
	strcpy (ret->description, desc);

	return ret;

err:
	free (ret);
	return NULL;
}

/**
 * Parse a method's information.
 *
 * Caller is responsible for freeing the list and all the elements, or simply
 * call the helper function #xmmsc_service_free_method.
 * 
 * @param res The #xmmsc_result_t returned by #xmmsc_service_list_method.
 * @return The #xmmsc_service_method_t structure or NULL if failed.
 */
xmmsc_service_method_t *
xmmsc_service_parse_method (xmmsc_result_t *res)
{
	char *name = NULL;
	char *desc = NULL;
	xmmsc_service_method_t *ret = NULL;

	if (xmmsc_result_iserror (res))
		return ret;

	ret = x_new0 (xmmsc_service_method_t, 1);

	if (!xmmsc_result_get_dict_entry_string (res, "name", &name))
		goto err;
	if (!xmmsc_result_get_dict_entry_string (res, "description", &desc))
		goto err;
	if (!xmmsc_result_get_dict_entry_uint (res, "num_rets", &ret->num_rets))
		goto err;
	if (!xmmsc_result_get_dict_entry_uint (res, "num_args", &ret->num_args))
		goto err;
	ret->name = x_new0 (char, strlen (name) + 1);
	strcpy (ret->name, name);
	ret->description = x_new0 (char, strlen (desc) + 1);
	strcpy (ret->description, desc);

	return ret;

err:
	free (ret);
	return NULL;
}

/**
 * Parse a method's argument types.
 *
 * Caller is responsible for freeing the list and all the elements, or simply
 * call the helper function #xmmsc_service_free_args.
 * 
 * @param res The #xmmsc_result_t returned by #xmmsc_service_list_method_args.
 * @param num The total number of arguments this method has.
 * @return The list of #xmmsc_service_argument_t or NULL if failed.
 */
xmmsc_service_argument_t *
xmmsc_service_parse_arg_types (xmmsc_result_t *res, uint32_t num)
{
	char *name = NULL;
	uint32_t i = num;
	xmmsc_service_argument_t *ret = NULL;

	if (xmmsc_result_iserror (res) || !xmmsc_result_list_valid (res) || num <= 0)
		return ret;

	ret = x_new0 (xmmsc_service_argument_t, num);

	while (xmmsc_result_list_valid (res) && i-- > 0) {
		if (!xmmsc_result_get_dict_entry_string (res, "name", &name))
			goto err;
		if (!xmmsc_result_get_dict_entry_uint (res, "type", &ret[i].type))
			goto err;
		if (!xmmsc_result_get_dict_entry_uint (res, "optional",
		                                       &ret[i].optional))
			goto err;
		ret[i].name = x_new0 (char, strlen (name) + 1);
		strcpy (ret[i].name, name);

		xmmsc_result_list_next (res);
	}

	return ret;

err:
	while (++i < num)
		free (ret[i].name);
	free (ret);
	return NULL;
}

/**
 * Free the given #xmmsc_service_t.
 *
 * @param service The #xmmsc_service_t list.
 */
void
xmmsc_service_free_service (xmmsc_service_t *service)
{
	free (service->name);
	free (service->description);
	free (service);
}

/**
 * Free the given #xmmsc_service_method_t.
 *
 * @param method The #xmmsc_service_method_t list.
 */
void
xmmsc_service_free_method (xmmsc_service_method_t *method)
{
	free (method->name);
	free (method->description);
	free (method);
}

/**
 * Free the given argument list.
 *
 * @param args The #xmmsc_service_argument_t list.
 * @param num The total number of arguments in the list.
 */
void
xmmsc_service_free_args (xmmsc_service_argument_t *args, uint32_t num)
{
	uint32_t i = num;

	while (i-- > 0)
		free (args[i].name);
	free (args);
}

/* @} */
