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
#include <stdarg.h>
#include <string.h>

#include "xmmsclient/xmmsclient.h"
#include "xmmsclientpriv/xmmsclient.h"
#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_errorcodes.h"

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

static int arg_attribute_get (xmmsc_service_argument_t *arg, const char *key,
                              void *value);
static int arg_attribute_set (xmmsc_service_argument_t *arg, const char *key,
                              const void *value);
/* static int arg_value_get (xmmsc_service_argument_t *arg, void *value); */
static int argument_write (xmms_ipc_msg_t *msg, xmmsc_service_argument_t *arg);
static int arg_lookup (const void *arg, const void *name);

/**
 * Register a new service.
 *
 * @param conn The connection to the server.
 * @param name Service name (will be duplicated).
 * @param description Service description (will be duplicated).
 * @param major Service major version.
 * @param minor Service minor version.
 */
xmmsc_result_t *
xmmsc_service_register (xmmsc_connection_t *conn,
                        const char *name, const char *description,
                        uint32_t major, uint32_t minor)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);
	x_return_null_if_fail (name);
	x_return_null_if_fail (description);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_REGISTER);
	xmms_ipc_msg_put_string (msg, name);
	xmms_ipc_msg_put_string (msg, description);
	xmms_ipc_msg_put_uint32 (msg, major);
	xmms_ipc_msg_put_uint32 (msg, minor);

	res = xmmsc_send_msg (conn, msg);

	return res;
}

/**
 * Register a new method.
 *
 * @param conn The connection to the server.
 * @param service Service name of which the method belongs to.
 * @param method #xmmsc_service_method_t which contains the method's information.
 * @param user_data Optional data to pass to the method.
 */
xmmsc_result_t *
xmmsc_service_method_register (xmmsc_connection_t *conn, const char *service,
                               xmmsc_service_method_t *method, void *user_data)
{
	return xmmsc_service_method_register_full (conn, service, method, user_data,
	                                           NULL);
}

/**
 * Register a new method.
 *
 * @param conn The connection to the server.
 * @param service Service name of which the method belongs to.
 * @param method #xmmsc_service_method_t which contains the method's information.
 * @param user_data Optional data to pass to the method.
 * @param free_func Optional function that should be called to free user_data.
 */
xmmsc_result_t *
xmmsc_service_method_register_full (xmmsc_connection_t *conn,
                                    const char *service,
                                    xmmsc_service_method_t *method,
                                    void *user_data,
                                    xmmsc_user_data_free_func_t free_func)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;
	x_list_t *n;
	xmmsc_service_argument_t *arg = NULL;

	x_check_conn (conn, NULL);
	x_return_null_if_fail (service);
	x_return_null_if_fail (method);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_METHOD_REGISTER);
	xmms_ipc_msg_put_string (msg, service);
	xmms_ipc_msg_put_string (msg, method->name);
	xmms_ipc_msg_put_string (msg, method->description);

	xmms_ipc_msg_put_uint32 (msg, x_list_length (method->ret_list));
	for (n = method->ret_list; n; n = x_list_next (n)) {
		arg = (xmmsc_service_argument_t *)n->data;
		xmms_ipc_msg_put_string (msg, arg->name);
		xmms_ipc_msg_put_uint32 (msg, arg->type);
		xmms_ipc_msg_put_int32 (msg, arg->optional);
	}

	xmms_ipc_msg_put_uint32 (msg, x_list_length (method->arg_list));
	for (n = method->arg_list; n; n = x_list_next (n)) {
		arg = (xmmsc_service_argument_t *)n->data;
		xmms_ipc_msg_put_string (msg, arg->name);
		xmms_ipc_msg_put_uint32 (msg, arg->type);
		xmms_ipc_msg_put_uint32 (msg, arg->optional);
	}

	res = xmmsc_send_msg (conn, msg);

	if (xmmsc_result_iserror (res))
		return res;

	xmmsc_result_restartable (res, XMMS_IPC_SIGNAL_SERVICE);
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
                          const char *service,
                          const char *method)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);
	x_return_null_if_fail (service);

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
xmmsc_service_ids_list (xmmsc_connection_t *conn)
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

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_INFO_LIST);
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
	                        XMMS_IPC_CMD_SERVICE_METHOD_INFO_LIST);
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
	                        XMMS_IPC_CMD_SERVICE_METHOD_INFO_LIST);
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
 * @param method The #xmmsc_service_method_t structure.
 */
xmmsc_result_t *
xmmsc_service_request (xmmsc_connection_t *conn, const char *service,
                       const xmmsc_service_method_t *method)
{
	xmms_ipc_msg_t *msg;
	xmmsc_result_t *res;
	x_list_t *n;
	xmmsc_service_argument_t *arg;

	x_check_conn (conn, NULL);
	x_return_null_if_fail (service);
	x_return_null_if_fail (method);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_REQUEST);
	xmms_ipc_msg_put_string (msg, service);
	xmms_ipc_msg_put_string (msg, method->name);
	if (method->arg_list) {
		for (n = method->arg_list; n; n = x_list_next (n)) {
			arg = (xmmsc_service_argument_t *)n->data;

			xmms_ipc_msg_put_string (msg, arg->name);
			xmms_ipc_msg_put_uint32 (msg, arg->none);
			if (arg->none)
				continue;
			if (!argument_write (msg, arg))
				return 0;
		}
	}

	res = xmmsc_send_msg (conn, msg);
	xmmsc_result_restartable (res, XMMS_IPC_SIGNAL_SERVICE);

	return res;
}

/**
 * Return service method request.
 *
 * @param conn The connection to the server.
 * @param res The result containing the service cookie.
 * @param method The #xmmsc_service_method_t structure.
 */
xmmsc_result_t *
xmmsc_service_return (xmmsc_connection_t *conn, xmmsc_result_t *res,
                      const xmmsc_service_method_t *method)
{
	xmms_ipc_msg_t *msg;
	uint32_t cookie;
	x_list_t *n;
	xmmsc_service_argument_t *arg;

	x_check_conn (conn, NULL);
	x_return_null_if_fail (res);
	x_return_null_if_fail (method);

	if (!xmmsc_result_get_service_cookie (res, &cookie))
		return NULL;

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_RETURN);
	xmms_ipc_msg_put_uint32 (msg, cookie);
	if (method->error) {
		xmms_ipc_msg_put_uint32 (msg, XMMS_IPC_CMD_ERROR);
		xmms_ipc_msg_put_string (msg, method->error_str);
	} else if (method->ret_list) {
		xmms_ipc_msg_put_uint32 (msg, XMMS_IPC_CMD_REPLY);
		for (n = method->ret_list; n; n = x_list_next (n)) {
			arg = (xmmsc_service_argument_t *)n->data;

			xmms_ipc_msg_put_string (msg, arg->name);
			xmms_ipc_msg_put_uint32 (msg, arg->none);
			if (arg->none)
				continue;
			if (!argument_write (msg, arg))
				return 0;
		}
	}

	return xmmsc_send_msg (conn, msg);
}

/**
 * Send shutdown broadcast to a service client.
 *
 * @param conn The connection to the server.
 * @param service Any service name the service client provides.
 */
xmmsc_result_t *
xmmsc_service_shutdown (xmmsc_connection_t *conn, const char *service)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);
	x_return_val_if_fail (service, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_SHUTDOWN);
	xmms_ipc_msg_put_string (msg, service);

	return xmmsc_send_msg (conn, msg);
}

/**
 * Request the service changed broadcast from the server.
 *
 * Everytime a service is registered, unregistered or modified, the broadcast
 * will be emitted. The returned #xmmsc_result_t of the broadcast is a dict
 * containing the action type #xmms_service_changed_actions_t as well as the ID
 * of the changed service.
 */
xmmsc_result_t *
xmmsc_broadcast_service_changed (xmmsc_connection_t *c)
{
	x_check_conn (c, NULL);

	return xmmsc_send_broadcast_msg (c, XMMS_IPC_SIGNAL_SERVICE_CHANGED);
}

/**
 * Request the service method changed broadcast from the server.
 *
 * Everytime a service method is registered, unregistered or modified, the
 * broadcast will be emitted. The returned #xmmsc_result_t is a dict containing
 * the action type #xmms_service_changed_actions_t as well as the ID of service
 * and the changed method.
 */
xmmsc_result_t *
xmmsc_broadcast_service_method_changed (xmmsc_connection_t *c)
{
	x_check_conn (c, NULL);

	return xmmsc_send_broadcast_msg (c, XMMS_IPC_SIGNAL_SERVICE_METHOD_CHANGED);
}

/**
 * Request the service client shutdown broadcast from the server.
 *
 * When a client sends a shutdown command to the service client, this broadcast
 * will be received.
 */
xmmsc_result_t *
xmmsc_broadcast_service_shutdown (xmmsc_connection_t *c)
{
	x_check_conn (c, NULL);

	return xmmsc_send_broadcast_msg (c, XMMS_IPC_SIGNAL_SERVICE_SHUTDOWN);
}

/**
 * Create a new #xmmsc_service_method_t.
 *
 * Caller is responsible for freeing the returned structure using
 * #xmmsc_service_method_free.
 *
 * @param name Method name (will be duplicated).
 * @param description Method description (will be duplicated).
 * @param func Callback function.
 * @return The newly created #xmmsc_service_method_t.
 */
xmmsc_service_method_t *
xmmsc_service_method_new (const char *name, const char *description,
                          xmmsc_result_notifier_t func)
{
	xmmsc_service_method_t *ret = NULL;

	if (!name || !description || !func)
		return NULL;

	ret = x_new0 (xmmsc_service_method_t, 1);

	ret->name = strdup (name);
	ret->description = strdup (description);
	ret->arg_list = NULL;
	ret->ret_list = NULL;
	ret->func = func;

	return ret;
}

/**
 * Free the given #xmmsc_service_method_t.
 *
 * @param method The #xmmsc_service_method_t list.
 */
void
xmmsc_service_method_free (xmmsc_service_method_t *method)
{
	x_list_t *n;

	if (!method)
		return;

	free (method->name);
	free (method->description);
	for (n = method->ret_list; n; n = x_list_next (n)) {
		free (((xmmsc_service_argument_t *)n->data)->name);
		free ((xmmsc_service_argument_t *)n->data);
	}
	for (n = method->arg_list; n; n = x_list_next (n)) {
		free (((xmmsc_service_argument_t *)n->data)->name);
		free ((xmmsc_service_argument_t *)n->data);
	}
	x_list_free (method->ret_list);
	x_list_free (method->arg_list);
	free (method->error_str);
	free (method);
}

/**
 * Reset all arguments and error message.
 *
 * @param method The method which contains the arguments.
 */
void
xmmsc_service_method_arg_reset (xmmsc_service_method_t *method)
{
	x_list_t *n;

	if (!method)
		return;

	xmmsc_service_method_error_reset (method);

	for (n = method->arg_list; n; n = x_list_next (n))
		((xmmsc_service_argument_t *)n->data)->none = 0;
}

/**
 * Reset all return values and error message.
 *
 * @param method The method which contains the return values.
 */
void
xmmsc_service_method_ret_reset (xmmsc_service_method_t *method)
{
	x_list_t *n;

	if (!method)
		return;

	xmmsc_service_method_error_reset (method);

	for (n = method->ret_list; n; n = x_list_next (n))
		((xmmsc_service_argument_t *)n->data)->none = 0;
}

/**
 * Get an attribute of a method.
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

	if (strcasecmp (key, "name") == 0 && method->name)
		*(char **)value = method->name;
	else if (strcasecmp (key, "description") == 0 && method->description)
		*(char **)value = method->description;
	else if (strcasecmp (key, "num_rets") == 0)
		return xmmsc_service_method_ret_size (method, (uint32_t *)value);
	else if (strcasecmp (key, "num_args") == 0)
		return xmmsc_service_method_arg_size (method, (uint32_t *)value);
	else if (strcasecmp (key, "func") == 0 && method->func)
		*(xmmsc_result_notifier_t *)value = method->func;
	else
		return 0;

	return 1;
}

/**
 * Set an attribute of a method.
 *
 * @param method The method containing the attribute.
 * @param key The name of the attribute.
 * @param value The new value.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_attribute_set (xmmsc_service_method_t *method,
                                    const char *key, const void *value)
{
	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (key, 0);
	x_return_val_if_fail (value, 0);

	if (strcasecmp (key, "name") == 0)
		method->name = *(char **)value;
	else if (strcasecmp (key, "description") == 0)
		method->description = *(char **)value;
	else if (strcasecmp (key, "func") == 0)
		method->func = *(xmmsc_result_notifier_t *)value;
	else
		return 0;

	return 1;
}

/**
 * Push new type to a method's argument list.
 *
 * @param name The name of the new type.
 * @param type The new type.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_arg_type_add (xmmsc_service_method_t *method,
                                   const char *name,
                                   xmmsc_service_arg_type_t type)
{
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (name, 0);

	arg = x_new0 (xmmsc_service_argument_t, 1);
	if (!arg)
		return 0;

	arg->name = strdup (name);
	arg->type = type;
	method->arg_list = x_list_append (method->arg_list, arg);

	return 1;
}

/**
 * Push new type to a method's return value list.
 *
 * @param name The name of the new type.
 * @param type The new type.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_ret_type_add (xmmsc_service_method_t *method,
                                   const char *name,
                                   xmmsc_service_arg_type_t type)
{
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (name, 0);

	arg = x_new0 (xmmsc_service_argument_t, 1);
	if (!arg)
		return 0;

	arg->name = strdup (name);
	arg->type = type;
	method->ret_list = x_list_append (method->ret_list, arg);

	return 1;
}

/**
 * Add value to argument list.
 *
 * @param method The method which contains the argument list.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_add_arg_uint32 (xmmsc_service_method_t *method,
                                     const char *name, uint32_t value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_UINT32)
		return 0;
	arg->value.uint32 = value;

	return 1;
}

/**
 * Add value to argument list.
 *
 * @param method The method which contains the argument list.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_add_arg_int32 (xmmsc_service_method_t *method,
                                    const char *name, int32_t value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_INT32)
		return 0;
	arg->value.int32 = value;

	return 1;
}

/**
 * Add value to argument list.
 *
 * @param method The method which contains the argument list.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_add_arg_string (xmmsc_service_method_t *method,
                                     const char *name, char *value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_STRING)
		return 0;
	arg->value.string = value;

	return 1;
}

/**
 * Add value to argument list.
 *
 * @param method The method which contains the argument list.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_add_arg_stringlist (xmmsc_service_method_t *method,
                                         const char *name, char **value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_STRINGLIST)
		return 0;
	arg->value.strings = value;

	return 1;
}

/**
 * Add value to argument list.
 *
 * @param method The method which contains the argument list.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_add_arg_coll (xmmsc_service_method_t *method,
                                   const char *name, xmmsc_coll_t *value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_COLL)
		return 0;
	arg->value.coll = value;

	return 1;
}

/**
 * Add value to argument list.
 *
 * @param method The method which contains the argument list.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_add_arg_bin (xmmsc_service_method_t *method,
                                  const char *name, unsigned char *value,
                                  uint32_t len)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_BIN || len == 0)
		return 0;
	arg->value.bin = value;
	arg->len = len;

	return 1;
}

/**
 * Add value to return value list.
 *
 * @param method The method which contains the return value list.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_add_ret_uint32 (xmmsc_service_method_t *method,
                                     const char *name, uint32_t value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_UINT32)
		return 0;
	arg->value.uint32 = value;

	return 1;
}

/**
 * Add value to return value list.
 *
 * @param method The method which contains the return value list.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_add_ret_int32 (xmmsc_service_method_t *method,
                                    const char *name, int32_t value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_INT32)
		return 0;
	arg->value.int32 = value;

	return 1;
}

/**
 * Add value to return value list.
 *
 * @param method The method which contains the return value list.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_add_ret_string (xmmsc_service_method_t *method,
                                     const char *name, char *value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_STRING)
		return 0;
	arg->value.string = value;

	return 1;
}

/**
 * Add value to return value list.
 *
 * @param method The method which contains the return value list.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_add_ret_stringlist (xmmsc_service_method_t *method,
                                         const char *name, char **value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_STRINGLIST)
		return 0;
	arg->value.strings = value;

	return 1;
}

/**
 * Add value to return value list.
 *
 * @param method The method which contains the return value list.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_add_ret_coll (xmmsc_service_method_t *method,
                                   const char *name, xmmsc_coll_t *value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_COLL)
		return 0;
	arg->value.coll = value;

	return 1;
}

/**
 * Add value to return value list.
 *
 * @param method The method which contains the return value list.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_add_ret_bin (xmmsc_service_method_t *method,
                                  const char *name, unsigned char *value,
                                  uint32_t len)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_BIN || len == 0)
		return 0;
	arg->value.bin = value;
	arg->len = len;

	return 1;
}

/**
 * Get the size of an argument list.
 *
 * @param method The method which contains the argument list.
 * @param size Pointer to the place to store the size.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_arg_size (xmmsc_service_method_t *method, uint32_t *size)
{
	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (size, 0);

	*size = x_list_length (method->arg_list);

	return 1;
}

/**
 * Get the size of an return value list.
 *
 * @param method The method which contains the return value list.
 * @param size Pointer to the place to store the size.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_ret_size (xmmsc_service_method_t *method, uint32_t *size)
{
	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (size, 0);

	*size = x_list_length (method->ret_list);

	return 1;
}

/**
 * Get an attribute of an argument.
 *
 * @param method The method which contains the argument list.
 * @param name The name of the argument.
 * @param key The name of the attribute.
 * @param value The new value.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_arg_attribute_get (xmmsc_service_method_t *method,
                                        const char *name, const char *key,
                                        void *value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (name, 0);
	x_return_val_if_fail (key, 0);
	x_return_val_if_fail (value, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	return arg_attribute_get (arg, key, value);
}

/**
 * Set an attribute of an argument.
 *
 * @param method The method which contains the argument list.
 * @param name The name of the argument.
 * @param key The name of the attribute.
 * @param value The new value.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_arg_attribute_set (xmmsc_service_method_t *method,
                                        const char *name, const char *key,
                                        const void *value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (name, 0);
	x_return_val_if_fail (key, 0);
	x_return_val_if_fail (value, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	return arg_attribute_set (arg, key, value);
}

/**
 * Get an attribute of an argument.
 *
 * @param method The method which contains the return value list.
 * @param name The name of the argument.
 * @param key The name of the attribute.
 * @param value The new value.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_ret_attribute_get (xmmsc_service_method_t *method,
                                        const char *name, const char *key,
                                        void *value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (name, 0);
	x_return_val_if_fail (key, 0);
	x_return_val_if_fail (value, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	return arg_attribute_get (arg, key, value);
}

/**
 * Set an attribute of an argument.
 *
 * @param method The method which contains the return value list.
 * @param name The name of the argument.
 * @param key The name of the attribute.
 * @param value The new value.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_ret_attribute_set (xmmsc_service_method_t *method,
                                        const char *name, const char *key,
                                        const void *value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (name, 0);
	x_return_val_if_fail (key, 0);
	x_return_val_if_fail (value, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup)))
		return 0;
	arg = (xmmsc_service_argument_t *)item->data;

	return arg_attribute_set (arg, key, value);
}

/* /\** */
/*  * Get an argument value of a method. */
/*  * */
/*  * @param method The method which contains the argument. */
/*  * @param key The name of the argument. */
/*  * @param value Pointer to the place where the value will be stored to. */
/*  * @return 1 for success, 0 otherwise. */
/*  *\/ */
/* int */
/* xmmsc_service_method_arg_value_get (xmmsc_service_method_t *method, */
/*                                     const char *key, void *value) */
/* { */
/* 	x_list_t *item; */
/* 	xmmsc_service_argument_t *arg; */

/* 	x_return_val_if_fail (method, 0); */
/* 	x_return_val_if_fail (key, 0); */
/* 	x_return_val_if_fail (value, 0); */

/* 	if (!(item = x_list_find_custom (method->arg_list, key, arg_lookup))) */
/* 		return 0; */
/* 	arg = (xmmsc_service_argument_t *)item->data; */

/* 	return arg_value_get (arg, value); */
/* } */

/* /\** */
/*  * Get a return argument's value of a method. */
/*  * */
/*  * @param method The method which contains the return argument. */
/*  * @param key The name of the argument. */
/*  * @param value Pointer to the place where the value will be stored to. */
/*  * @return 1 for success, 0 otherwise. */
/*  *\/ */
/* int */
/* xmmsc_service_method_ret_value_get (xmmsc_service_method_t *method, */
/*                                     const char *key, void *value) */
/* { */
/* 	x_list_t *item; */
/* 	xmmsc_service_argument_t *arg; */

/* 	x_return_val_if_fail (method, 0); */
/* 	x_return_val_if_fail (key, 0); */
/* 	x_return_val_if_fail (value, 0); */

/* 	if (!(item = x_list_find_custom (method->ret_list, key, arg_lookup))) */
/* 		return 0; */
/* 	arg = (xmmsc_service_argument_t *)item->data; */

/* 	return arg_value_get (arg, value); */
/* } */

/**
 * Set an argument value to none.
 *
 * @param method The method which contains the argument.
 * @param key The name of the argument.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_arg_value_setnone (xmmsc_service_method_t *method,
                                        const char *key)
{
	x_list_t *item;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (key, 0);

	if (!(item = x_list_find_custom (method->arg_list, key, arg_lookup)))
		return 0;
	((xmmsc_service_argument_t *)item->data)->none = 1;

	return 1;
}

/**
 * Set a return argument value to none.
 *
 * @param method The method which contains the return argument.
 * @param key The name of the argument.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_ret_value_setnone (xmmsc_service_method_t *method,
                                        const char *key)
{
	x_list_t *item;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (key, 0);

	if (!(item = x_list_find_custom (method->ret_list, key, arg_lookup)))
		return 0;
	((xmmsc_service_argument_t *)item->data)->none = 1;

	return 1;
}

/**
 * Set an error message.
 *
 * @param method The method which you want to set the error message to.
 * @param err The error message willing to set.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_error_set (xmmsc_service_method_t *method, const char *err)
{
	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (err, 0);

	if (method->error && method->error_str)
		free (method->error_str);

	method->error = 1;
	if (!(method->error_str = strdup (err)))
		return 0;

	return 1;
}

/**
 * Reset error message.
 *
 * @param method The method that will contain the error message.
 */
void
xmmsc_service_method_error_reset (xmmsc_service_method_t *method)
{
	x_return_if_fail (method);

	method->error = 0;
	free (method->error_str);
	method->error_str = NULL;
}

/**
 * Check if a method already contains an error message.
 *
 * @param method The method to check.
 * @return 1 means yes, 0 otherwise.
 */
int
xmmsc_service_method_error_isset (xmmsc_service_method_t *method)
{
	x_return_val_if_fail (method, 0);

	return method->error;
}

/**
 * @internal
 */
static int
arg_attribute_get (xmmsc_service_argument_t *arg, const char *key, void *value)
{
	if (strcasecmp (key, "type") == 0)
		*(xmmsc_service_arg_type_t *)value = arg->type;
	else if (strcasecmp (key, "optional") == 0)
		*(uint32_t *)value = arg->optional;
	else if (strcasecmp (key, "none") == 0)
		*(uint32_t *)value = arg->none;
	else
		return 0;

	return 1;
}

static int
arg_attribute_set (xmmsc_service_argument_t *arg, const char *key,
                   const void *value)
{
	if (strcasecmp (key, "type") == 0)
		arg->type = *(xmmsc_service_arg_type_t *)value;
	else if (strcasecmp (key, "optional") == 0)
		arg->optional = *(uint32_t *)value;
	else if (strcasecmp (key, "none") == 0)
		arg->none = *(uint32_t *)value;
	else
		return 0;

	return 1;
}

/* static int */
/* arg_value_get (xmmsc_service_argument_t *arg, void *value) */
/* { */
/* 	switch (arg->type) { */
/* 	case XMMSC_SERVICE_ARG_TYPE_UINT32: */
/* 		*(uint32_t *)value = arg->value.uint32; */
/* 		break; */
/* 	case XMMSC_SERVICE_ARG_TYPE_INT32: */
/* 		*(int32_t *)value = arg->value.int32; */
/* 		break; */
/* 	case XMMSC_SERVICE_ARG_TYPE_STRING: */
/* 		*(char **)value = arg->value.string; */
/* 		break; */
/* 	case XMMSC_SERVICE_ARG_TYPE_STRINGLIST: */
/* 		*(char ***)value = arg->value.strings; */
/* 		break; */
/* 	case XMMSC_SERVICE_ARG_TYPE_COLL: */
/* 		*(xmmsc_coll_t **)value = arg->value.coll; */
/* 		break; */
/* 	case XMMSC_SERVICE_ARG_TYPE_BIN: */
/* 		*(unsigned char **)value = arg->value.bin; */
/* 		break; */
/* 	default: */
/* 		return 0; */
/* 	} */

/* 	return 1; */
/* } */

static int
argument_write (xmms_ipc_msg_t *msg, xmmsc_service_argument_t *arg)
{
	x_return_val_if_fail (msg, 0);
	x_return_val_if_fail (arg, 0);

	switch (arg->type) {
	case XMMSC_SERVICE_ARG_TYPE_UINT32:
		xmms_ipc_msg_put_uint32 (msg, arg->value.uint32);
		break;
	case XMMSC_SERVICE_ARG_TYPE_INT32:
		xmms_ipc_msg_put_int32 (msg, arg->value.int32);
		break;
	case XMMSC_SERVICE_ARG_TYPE_STRING:
		xmms_ipc_msg_put_string (msg, arg->value.string);
		break;
	case XMMSC_SERVICE_ARG_TYPE_STRINGLIST:
		xmms_ipc_msg_put_string_list (msg, (const char **)arg->value.strings);
		break;
	case XMMSC_SERVICE_ARG_TYPE_COLL:
		xmms_ipc_msg_put_collection (msg, arg->value.coll);
		break;
	case XMMSC_SERVICE_ARG_TYPE_BIN:
		xmms_ipc_msg_put_bin (msg, arg->value.bin,
							  arg->len);
		break;
	default:
		return 0;
	}

	return 1;
}

static int
arg_lookup (const void *arg, const void *name)
{
	if (strcasecmp (((const xmmsc_service_argument_t *)arg)->name,
	                (const char *)name) == 0)
		return 1;

	return 0;
}

/* @} */
