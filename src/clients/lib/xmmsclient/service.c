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

static int xmmsc_service_argument_write (xmms_ipc_msg_t *msg,
                                         xmmsc_service_argument_t *arg);

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

		xmms_ipc_msg_put_uint32 (msg, method->ret_list->size);
		for (count = 0; count < method->ret_list->size; count++) {
			arg = method->ret_list->args + count;
			xmms_ipc_msg_put_string (msg, arg->name);
			xmms_ipc_msg_put_uint32 (msg, arg->type);
			xmms_ipc_msg_put_uint32 (msg, arg->optional);
		}

		xmms_ipc_msg_put_uint32 (msg, method->arg_list->size);
		for (count = 0; count < method->arg_list->size; count++) {
			arg = method->arg_list->args + count;
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
 * @param arg_list The argument list passing to the method.
 */
xmmsc_result_t *xmmsc_service_request (xmmsc_connection_t *conn,
                                       const char *service,
                                       const char *method,
                                       const xmmsc_service_arg_list_t *arg_list)
{
	xmms_ipc_msg_t *msg;
	xmmsc_result_t *res;
	int i;

	x_check_conn (conn, NULL);
	x_return_val_if_fail (service, NULL);
	x_return_val_if_fail (method, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_REQUEST);
	xmms_ipc_msg_put_string (msg, service);
	xmms_ipc_msg_put_string (msg, method);
	if (arg_list) {
		for (i = 0; i < arg_list->size; i++) {
			xmms_ipc_msg_put_string (msg, arg_list->args[i].name);
			xmms_ipc_msg_put_uint32 (msg, arg_list->args[i].none);
			if (arg_list->args[i].none)
				continue;
			if (!xmmsc_service_argument_write (msg, arg_list->args + i))
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
 * @param arg_list The returning argument list.
 */
xmmsc_result_t *xmmsc_service_return (xmmsc_connection_t *conn,
                                      xmmsc_result_t *res,
                                      const xmmsc_service_arg_list_t *arg_list)
{
	xmms_ipc_msg_t *msg;
	uint32_t cookie;
	int i;

	x_check_conn (conn, NULL);
	x_return_val_if_fail (res, NULL);

	if (!xmmsc_result_get_service_cookie (res, &cookie))
		return NULL;

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_RETURN);
	xmms_ipc_msg_put_uint32 (msg, cookie);
	if (arg_list->args) {
		for (i = 0; i < arg_list->size; i++) {
			xmms_ipc_msg_put_string (msg, arg_list->args[i].name);
			xmms_ipc_msg_put_uint32 (msg, arg_list->args[i].none);
			if (arg_list->args[i].none)
				continue;
			if (!xmmsc_service_argument_write (msg, arg_list->args + i))
				return 0;
		}
	}

	return xmmsc_send_msg (conn, msg);
}

/**
 * Request the service changed broadcast from the server.
 *
 * Everytime a service is registered, unregistered or modified, the broadcast
 * will be emitted. The returned #xmmsc_result_t of the broadcast is a list of
 * service ids after the service's modification.
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
 * broadcast will be emitted. The returned #xmmsc_result_t is a list of IDs,
 * where the first one is the ID of the service which all the methods belong to,
 * followed by the IDs of the all the methods this service provides.
 */
xmmsc_result_t *
xmmsc_broadcast_service_method_changed (xmmsc_connection_t *c)
{
	x_check_conn (c, NULL);

	return xmmsc_send_broadcast_msg (c, XMMS_IPC_SIGNAL_SERVICE_METHOD_CHANGED);
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
 * @param rets Pointer to return #xmmsc_service_arg_list_t.
 * @param args Pointer to argument list #xmmsc_service_arg_list_t.
 * @param func Callback function.
 * @return The newly created #xmmsc_service_method_t.
 */
xmmsc_service_method_t *
xmmsc_service_method_new (const char *name, const char *description,
                          xmmsc_service_arg_list_t *rets,
                          xmmsc_service_arg_list_t *args,
                          xmmsc_result_notifier_t func)
{
	xmmsc_service_method_t *ret = NULL;

	if (!name || !description || !func)
		return NULL;

	ret = x_new0 (xmmsc_service_method_t, 1);

	ret->name = strdup (name);
	ret->description = strdup (description);
	ret->ret_list = rets;
	ret->arg_list = args;
	ret->func = func;

	return ret;
}

/**
 * Create a new #xmmsc_service_arg_list_t.
 *
 * Caller is responsible for freeing the returned structure using
 * #xmmsc_service_args_free.
 *
 * @param size The size of the argument list.
 * @return The newly created #xmmsc_service_arg_list_t.
 */
xmmsc_service_arg_list_t *
xmmsc_service_args_new (uint32_t size, ...)
{
	xmmsc_service_arg_list_t *ret = NULL;
	va_list ap;
	char *name = NULL;
	xmmsc_service_arg_type_t type;
	uint32_t i;

	if (!size)
		return NULL;

	ret = x_new0 (xmmsc_service_arg_list_t, 1);

	ret->size = size;
	ret->args = x_new0 (xmmsc_service_argument_t, size);

	va_start (ap, size);
	for (i = 0; i < ret->size; i++) {
		ret->args[i].name = strdup (va_arg (ap, char *));
		ret->args[i].type = va_arg (ap, xmmsc_service_arg_type_t);
	}
	va_end (ap);

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
 * @param args The #xmmsc_service_arg_list_t list.
 */
void
xmmsc_service_args_free (xmmsc_service_arg_list_t *args)
{
	uint32_t i = args->size;

	while (i-- > 0)
		free (args->args[i].name);
	free (args->args);
	free (args);
}

/**
 * Get an attribute of a service.
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
 * Set an attribute of a service.
 *
 * @param service The service containing the attribute.
 * @param key The name of the attribute.
 * @param value The new value.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_attribute_set (xmmsc_service_t *service, const char *key,
                             const void *value)
{
	x_return_val_if_fail (service, 0);
	x_return_val_if_fail (key, 0);
	x_return_val_if_fail (value, 0);

	if (strcasecmp (key, "name") == 0)
		service->name = *(char **)value;
	else if (strcasecmp (key, "description") == 0)
		service->description = *(char **)value;
	else if (strcasecmp (key, "major_version") == 0)
		service->major_version = *(uint32_t *)value;
	else if (strcasecmp (key, "minor_version") == 0)
		service->minor_version = *(uint32_t *)value;
	else if (strcasecmp (key, "count") == 0)
		service->count = *(uint32_t *)value;
	else
		return 0;

	return 1;
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

	if (strcasecmp (key, "name") == 0)
		*(char **)value = method->name;
	else if (strcasecmp (key, "description") == 0)
		*(char **)value = method->description;
	else if (strcasecmp (key, "ret_list") == 0)
		*(xmmsc_service_arg_list_t **)value = method->ret_list;
	else if (strcasecmp (key, "arg_list") == 0)
		*(xmmsc_service_arg_list_t **)value = method->arg_list;
	else if (strcasecmp (key, "func") == 0)
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
	else if (strcasecmp (key, "ret_list") == 0)
		method->ret_list = *(xmmsc_service_arg_list_t **)value;
	else if (strcasecmp (key, "arg_list") == 0)
		method->arg_list = *(xmmsc_service_arg_list_t **)value;
	else if (strcasecmp (key, "func") == 0)
		method->func = *(xmmsc_result_notifier_t *)value;
	else
		return 0;

	return 1;
}

/**
 * Get the size of an argument list.
 *
 * @param arg_list The argument list containing the size.
 * @param size Pointer to the place to store the size.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_arg_list_size (xmmsc_service_arg_list_t *arg_list, uint32_t *size)
{
	x_return_val_if_fail (arg_list, 0);
	x_return_val_if_fail (size, 0);

	*size = arg_list->size;

	return 1;
}

/**
 * Get an attribute of an argument.
 *
 * @param arg_list The argument list containing the argument.
 * @param name The name of the argument.
 * @param key The name of the attribute.
 * @param value The new value.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_arg_attribute_get (xmmsc_service_arg_list_t *arg_list,
                                 const char *name, const char *key,
                                 void *value)
{
	int i;

	x_return_val_if_fail (arg_list, 0);
	x_return_val_if_fail (name, 0);
	x_return_val_if_fail (key, 0);
	x_return_val_if_fail (value, 0);

	for (i = 0; i < arg_list->size; i++) {
		if (strcasecmp (arg_list->args[i].name, name) == 0) {
			if (strcasecmp (key, "type") == 0)
				*(xmmsc_service_arg_type_t *)value = arg_list->args[i].type;
			else if (strcasecmp (key, "optional") == 0)
				*(uint32_t *)value = arg_list->args[i].optional;
			else if (strcasecmp (key, "none") == 0)
				*(uint32_t *)value = arg_list->args[i].none;
			else
				return 0;

			return 1;
		}
	}

	return 0;
}

/**
 * Set an attribute of an argument.
 *
 * @param arg_list The argument list containing the argument.
 * @param name The name of the argument.
 * @param key The name of the attribute.
 * @param value The new value.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_arg_attribute_set (xmmsc_service_arg_list_t *arg_list,
                                 const char *name, const char *key,
                                 const void *value)
{
	int i;

	x_return_val_if_fail (arg_list, 0);
	x_return_val_if_fail (name, 0);
	x_return_val_if_fail (key, 0);
	x_return_val_if_fail (value, 0);

	for (i = 0; i < arg_list->size; i++) {
		if (strcasecmp (arg_list->args[i].name, name) == 0) {
			if (strcasecmp (key, "type") == 0)
				arg_list->args[i].type = *(xmmsc_service_arg_type_t *)value;
			else if (strcasecmp (key, "optional") == 0)
				arg_list->args[i].optional = *(uint32_t *)value;
			else if (strcasecmp (key, "none") == 0)
				arg_list->args[i].none = *(uint32_t *)value;
			else
				return 0;

			return 1;
		}
	}

	return 0;
}

/**
 * Get an argument value of a method.
 *
 * @param arg_list The argument list containing the argument.
 * @param key The name of the argument.
 * @param value Pointer to the place where the value will be stored to.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_arg_value_get (xmmsc_service_arg_list_t *arg_list, const char *key,
                             void *value)
{
	int i;

	x_return_val_if_fail (arg_list, 0);
	x_return_val_if_fail (key, 0);
	x_return_val_if_fail (value, 0);

	for (i = 0; i < arg_list->size; i++) {
		if (strcasecmp (arg_list->args[i].name, key) == 0) {
			switch (arg_list->args[i].type) {
			case XMMSC_SERVICE_ARG_TYPE_UINT32:
				*(uint32_t *)value = arg_list->args[i].value.uint32;
				break;
			case XMMSC_SERVICE_ARG_TYPE_INT32:
				*(int32_t *)value = arg_list->args[i].value.int32;
				break;
			case XMMSC_SERVICE_ARG_TYPE_STRING:
				*(char **)value = arg_list->args[i].value.string;
				break;
			case XMMSC_SERVICE_ARG_TYPE_STRINGLIST:
				*(char ***)value = arg_list->args[i].value.strings;
				break;
			case XMMSC_SERVICE_ARG_TYPE_COLL:
				*(xmmsc_coll_t **)value = arg_list->args[i].value.coll;
				break;
			case XMMSC_SERVICE_ARG_TYPE_BIN:
				*(unsigned char **)value = arg_list->args[i].value.bin;
				break;
			default:
				return 0;
			}

			return 1;
		}
	}

	return 0;
}

/**
 * Set an argument value of a method.
 *
 * @param arg_list The argument list containing the argument.
 * @param key The name of the argument.
 * @param value The new value.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_arg_value_set (xmmsc_service_arg_list_t *arg_list, const char *key,
                            const void *value)
{
	int i;

	x_return_val_if_fail (arg_list, 0);
	x_return_val_if_fail (key, 0);
	x_return_val_if_fail (value, 0);

	for (i = 0; i < arg_list->size; i++) {
		if (strcasecmp (arg_list->args[i].name, key) == 0) {
			switch (arg_list->args[i].type) {
			case XMMSC_SERVICE_ARG_TYPE_UINT32:
				arg_list->args[i].value.uint32 = *(uint32_t *)value;
				break;
			case XMMSC_SERVICE_ARG_TYPE_INT32:
				arg_list->args[i].value.int32 = *(int32_t *)value;
				break;
			case XMMSC_SERVICE_ARG_TYPE_STRING:
				arg_list->args[i].value.string = (char *)value;
				break;
			case XMMSC_SERVICE_ARG_TYPE_STRINGLIST:
				arg_list->args[i].value.strings = (char **)value;
				break;
			case XMMSC_SERVICE_ARG_TYPE_COLL:
				arg_list->args[i].value.coll = (xmmsc_coll_t *)value;
				break;
			case XMMSC_SERVICE_ARG_TYPE_BIN:
				arg_list->args[i].value.bin = (unsigned char *)value;
				break;
			default:
				return 0;
			}

			return 1;
		}
	}

	return 0;
}

/**
 * Set an argument value to none.
 *
 * @param arg_list The argument list containing the argument.
 * @param key The name of the argument.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_arg_value_setnone (xmmsc_service_arg_list_t *arg_list,
                                 const char *key)
{
	int i;

	x_return_val_if_fail (arg_list, 0);
	x_return_val_if_fail (key, 0);

	for (i = 0; i < arg_list->size; i++) {
		if (strcasecmp (arg_list->args[i].name, key) == 0) {
			arg_list->args[i].none = 1;

			return 1;
		}
	}

	return 0;
}

/**
 * @internal
 */
static int
xmmsc_service_argument_write (xmms_ipc_msg_t *msg, xmmsc_service_argument_t *arg)
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

/* @} */
