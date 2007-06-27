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
                          const char *service,
                          const char *method)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);
	x_api_error_if (!service, "with a NULL service", NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_UNREGISTER);
	xmms_ipc_msg_put_string (msg, service);

	if (method)
		xmms_ipc_msg_put_string (msg, method);

	return xmmsc_send_msg (conn, msg);
}

/**
 * List available service ids.
 *
 * The list can be used to further query for service details.
 *
 * @param conn The connection to the server.
 */
xmmsc_result_t *
xmmsc_service_service_ids_list (xmmsc_connection_t *conn)
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
xmmsc_service_list (xmmsc_connection_t *conn, const char *service)
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
xmmsc_service_method_ids_list (xmmsc_connection_t *conn, const char *service)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_METHOD_LIST);
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
xmmsc_service_method_list (xmmsc_connection_t *conn, const char *service,
                           const char *method)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_METHOD_LIST);
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
xmmsc_service_method_args_list (xmmsc_connection_t *conn, const char *service,
                                const char *method)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_METHOD_LIST);
	xmms_ipc_msg_put_string (msg, service);
	xmms_ipc_msg_put_string (msg, method);
	xmms_ipc_msg_put_uint32 (msg, 1);

	return xmmsc_send_msg (conn, msg);
}

/**
 * Make service method call.
 *
 * @param conn The connection to the server.
 * @param service The id of the service.
 * @param method The id of the method.
 * @param args The arguments passing to the method.
 */
xmmsc_result_t *xmmsc_service_request (xmmsc_connection_t *conn,
                                       const xmmsc_service_t *service,
                                       const xmmsc_service_method_t *method,
                                       const xmmsc_service_argument_t *args)
{
	xmms_ipc_msg_t *msg;
	int i;

	x_check_conn (conn, NULL);
	x_return_val_if_fail (service, NULL);
	x_return_val_if_fail (method, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_REQUEST);
	xmms_ipc_msg_put_string (msg, service->name);
	xmms_ipc_msg_put_string (msg, method->name);
	if (args) {
		for (i = 0; i < method->num_args; i++) {
			xmms_ipc_msg_put_string (msg, args[i].name);
			xmms_ipc_msg_put_uint32 (msg, args[i].none);
			switch (args[i].type) {
			case XMMSC_SERVICE_ARG_TYPE_UINT32:
				xmms_ipc_msg_put_uint32 (msg, args[i].value.uint32);
				break;
			case XMMSC_SERVICE_ARG_TYPE_INT32:
				xmms_ipc_msg_put_int32 (msg, args[i].value.int32);
				break;
			case XMMSC_SERVICE_ARG_TYPE_STRING:
				xmms_ipc_msg_put_string (msg, args[i].value.string);
				break;
			case XMMSC_SERVICE_ARG_TYPE_STRINGLIST:
				xmms_ipc_msg_put_string_list (msg, (const char **)args[i].value.strings);
				break;
			case XMMSC_SERVICE_ARG_TYPE_COLL:
				xmms_ipc_msg_put_collection (msg, args[i].value.coll);
				break;
			case XMMSC_SERVICE_ARG_TYPE_BIN:
				xmms_ipc_msg_put_bin (msg, args[i].value.bin, args[i].len);
				break;
			default:
				return NULL;
			}
		}
	}

	return xmmsc_send_msg (conn, msg);
}

/**
 * Create a new #xmmsc_service_t.
 *
 * Caller is responsible for freeing the returned structure using
 * #xmmsc_service_free.
 *
 * @param name Service name (will be duplicated).
 * @param description Service description (will be duplicated).
 * @param major Service major version.
 * @param minor Service minor version.
 * @return The newly created #xmmsc_service_t.
 */
xmmsc_service_t *
xmmsc_service_new (const char *name, const char *description,
                   uint32_t major, uint32_t minor)
{
	xmmsc_service_t *ret = NULL;

	if (!name || !description)
		return NULL;

	ret = x_new0 (xmmsc_service_t, 1);

	ret->name = strdup (name);
	ret->description = strdup (description);
	ret->major_version = major;
	ret->minor_version = minor;

	return ret;
}

/**
 * Create a new #xmmsc_service_method_t.
 *
 * Caller is responsible for freeing the returned structure using
 * #xmmsc_service_method_free.
 *
 * @param name Method name (will be duplicated).
 * @param description Method description (will be duplicated).
 * @param rets Pointer to return #xmmsc_service_argument_t.
 * @param args Pointer to argument list #xmmsc_service_argument_t.
 * @param func Callback function.
 * @return The newly created #xmmsc_service_method_t.
 */
xmmsc_service_method_t *
xmmsc_service_method_new (const char *name, const char *description,
                          uint32_t num_rets, xmmsc_service_argument_t *rets,
                          uint32_t num_args, xmmsc_service_argument_t *args,
                          xmmsc_result_notifier_t func)
{
	xmmsc_service_method_t *ret = NULL;

	if (!name || !description || !func)
		return NULL;

	ret = x_new0 (xmmsc_service_method_t, 1);

	ret->name = strdup (name);
	ret->description = strdup (description);
	ret->num_rets = num_rets;
	ret->rets = rets;
	ret->num_args = num_args;
	ret->args = args;
	ret->func = func;

	return ret;
}

/**
 * Free the given #xmmsc_service_t.
 *
 * @param service The #xmmsc_service_t list.
 */
void
xmmsc_service_free (xmmsc_service_t *service)
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
xmmsc_service_method_free (xmmsc_service_method_t *method)
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
xmmsc_service_args_free (xmmsc_service_argument_t *args, uint32_t num)
{
	uint32_t i = num;

	while (i-- > 0)
		free (args[i].name);
	free (args);
}

/**
 * Get an attribute from a service.
 *
 * The returned value is owned by the service.
 *
 * @param service The service containing the attribute.
 * @param key The name of the attribute.
 * @param value Pointer to the place where the value should be stored to.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_attribute_get (xmmsc_service_t *service, const char *key,
                             void *value)
{
	x_return_val_if_fail (service, 0);
	x_return_val_if_fail (key, 0);
	x_return_val_if_fail (value, 0);

	if (strcasecmp (key, "name") == 0)
		*(char **)value = service->name;
	else if (strcasecmp (key, "description") == 0)
		*(char **)value = service->description;
	else if (strcasecmp (key, "major_version") == 0)
		*(uint32_t *)value = service->major_version;
	else if (strcasecmp (key, "minor_version") == 0)
		*(uint32_t *)value = service->minor_version;
	else if (strcasecmp (key, "count") == 0)
		*(uint32_t *)value = service->count;
	else
		return 0;

	return 1;
}

/**
 * Get an attribute from a method.
 *
 * The returned value is owned by the method.
 *
 * @param method The method containing the attribute.
 * @param key The name of the attribute.
 * @param value Pointer to the place where the value should be stored to.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_attribute_get (xmmsc_service_method_t *method,
                                    const char *key, void *value)
{
	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (key, 0);
	x_return_val_if_fail (value, 0);

	if (strcasecmp (key, "name") == 0)
		*(char **)value = method->name;
	else if (strcasecmp (key, "description") == 0)
		*(char **)value = method->description;
	else if (strcasecmp (key, "num_rets") == 0)
		*(uint32_t *)value = method->num_rets;
	else if (strcasecmp (key, "rets") == 0)
		*(xmmsc_service_argument_t **)value = method->rets;
	else if (strcasecmp (key, "num_args") == 0)
		*(uint32_t *)value = method->num_args;
	else if (strcasecmp (key, "args") == 0)
		*(xmmsc_service_argument_t **)value = method->args;
	else if (strcasecmp (key, "func") == 0)
		*(xmmsc_result_notifier_t *)value = method->func;
	else
		return 0;

	return 1;
}

/* @} */
