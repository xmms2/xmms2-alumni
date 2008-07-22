/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2008 XMMS2 Team
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

struct xmmsc_service_St {
	char *name;
	char *desc;
	uint32_t major;
	uint32_t minor;
	x_list_t *methods;

	uint32_t ref;
};

typedef struct xmmsc_service_method_St {
	char *name;
	char *desc;
	void *udata;
	xmmsc_service_notifier_t func;
	xmmsc_user_data_free_func_t ufree;
	xmmsv_type_t rettype;
	xmmsv_t *args;
} xmmsc_service_method_t;

typedef struct xmmsc_service_method_arg_St {
	char *name;
	xmmsc_value_type_t *type;
} xmmsc_service_method_arg_t;

static int xmmsc_service_method_add_valist (xmmsc_service_t *svc,
                                            const char *name, const char *desc,
                                            xmmsv_type_t rettype,
                                            xmmsc_service_notifier_t func,
                                            void *udata,
                                            xmmsc_user_data_free_func_t ufree,
                                            va_list ap);
static void xmmsc_service_free (xmmsc_service_t *svc);
static void xmmsc_service_method_free (xmmsc_service_method_t *meth);

/* Strange, old crap. Will stick these methods above this comment line one-by-
 * one as they are checked/updated. */
/*
static xmmsc_result_t *method_return (xmmsc_connection_t *conn,
                                      xmmsc_value_t *val,
                                      xmmsc_service_method_t *method);
static int arg_attribute_get (xmmsc_service_argument_t *arg,
                              xmmsc_service_arg_type_t *type, uint32_t *optional,
                              uint32_t *none);
static int data_copy (void *dest, const void *src, uint32_t len);
static void arg_value_free (xmmsc_service_argument_t *arg);
static void arg_reset (xmmsc_service_method_t *method);
static void ret_reset (xmmsc_service_method_t *method);
static int arg_value_get (xmmsc_service_argument_t *arg, void *value);
static int argument_write (xmms_ipc_msg_t *msg, xmmsc_service_argument_t *arg);
static int arg_lookup (const void *arg, const void *name);
static void free_infos (void *data);
static void dispatch (xmmsc_value_t *val, void *data);
static xmmsc_coll_t *coll_copy (xmmsc_coll_t *coll);
static void coll_attr_copy (const char *key, const char *value, void *userdata);
*/

/**
 * Create a new service.
 *
 * @param name Service name (will be duplicated).
 * @param desc Service description (will be duplicated).
 * @param major Service major version.
 * @param minor Service minor version.
 * @return The newly created #xmmsc_service_t or NULL on failure. This must be
 * freed using #xmmsc_service_unref.
 */
xmmsc_service_t *
xmmsc_service_new (const char *name, const char *desc,
                   uint32_t major, uint32_t minor)
{
	xmmsc_service_t *svc;

	x_return_null_if_fail (name);
	x_return_null_if_fail (description);

	svc = x_new (xmmsc_service_t, 1);
	x_return_null_if_fail (svc);

	svc->name = strdup (name);
	svc->desc = strdup (desc);
	svc->major = major;
	svc->minor = minor;
	svc->methods = x_list_alloc ();
	svc->ref = 1;

	if (!(svc->name && svc->desc && svc->methods)) {
		xmmsc_service_free (svc);
		return NULL;
	}

	svc->methods->data = NULL;

	return svc;
}

/**
 * Create a new service method.
 *
 * Caller is responsible for freeing the returned structure using
 * #xmmsc_service_method_unref.
 *
 * @param svc The #xmmsc_service_t to which the method will be added.
 * @param name Method name (will be duplicated).
 * @param desc Method description (will be duplicated).
 * @param rettype Type of the return value, an #xmmsv_type_t.
 * @param func Callback function, an #xmmsc_service_notifier_t.
 * @param udata User data to pass to func.
 * @param ... NULL-terminated list of const char *, xmmsv_type_t.
 * @return 1 on success, 0 otherwise.
 */
int
xmmsc_service_method_add (xmmsc_service_t *svc, const char *name,
                          const char *desc, xmmsv_type_t rettype,
                          xmmsc_service_notifier_t func, void *udata, ...)
{
	int ret;
	va_list ap;

	va_start (ap, udata);
	ret = xmmsc_service_method_add_valist (svc, name, desc, rettype, func,
	                                       udata, NULL, ap);
	va_end (ap);

	return ret;
}

/**
 * Create a new service method.
 *
 * Udata will be freed by free_func if this call fails.
 *
 * @param svc The #xmmsc_service_t to which the method will be added.
 * @param name Method name (will be duplicated).
 * @param desc Method description (will be duplicated).
 * @param rettype Type of the return value, a #xmmsv_type_t.
 * @param func Callback function, an #xmmsc_service_notifier_t.
 * @param udata User data to pass to func.
 * @param ufree Optional function that should be called to free udata, an
 * #xmmsc_user_data_free_func_t.
 * @param ... NULL-terminated list of const char *, xmmsv_type_t.
 * @return 1 on success, 0 otherwise.
 */
int
xmmsc_service_method_add_full (xmmsc_service_t *svc, const char *name,
                               const char *desc, xmmsv_type_t rettype,
                               xmmsc_service_notifier_t func, void *udata,
                               xmmsc_user_data_free_func_t ufree, ...)
{
	int ret;
	va_list ap;

	va_start (ap, free_func);
	ret = xmmsc_service_method_add_valist (svc, name, desc, rettype, func,
	                                       udata, ufree, ap);
	va_end (ap);

	return ret;
}

static int
xmmsc_service_method_add_valist (xmmsc_service_t *svc, const char *name,
                                 const char *desc, xmmsv_type_t rettype,
                                 xmmsc_service_notifier_t func, void *udata,
                                 xmmsc_user_data_free_func_t ufree, va_list ap)
{
	xmmsc_service_method_arg_t *arg;
	xmmsv_type_t type;
	const char* arg;

	x_return_val_if_fail (xmmsc_service_method_add_noarg (svc, name, desc,
	                                                      rettype, func, udata,
	                                                      ufree), 0);

	while (42) {
		arg = va_arg (ap, const char *);
		if (!arg) {
			break;
		}
		type = va_arg (ap, xmmsv_type_t);
		x_return_val_if_fail (xmmsv_check_type (type), 0);

		xmmsc_service_method_add_arg (svc, arg, type);
	}

	return 1;
}

/**
 * Create a new service method with no arguments. Arguments can be added
 * individually by calling #xmmsc_service_add_arg immediately after calling
 * this function.
 *
 * Udata will be freed by free_func if this call fails.
 *
 * @param svc The #xmmsc_service_t to which the method will be added.
 * @param name Method name (will be duplicated).
 * @param desc Method description (will be duplicated).
 * @param rettype Type of the return value, a #xmmsv_type_t.
 * @param func Callback function, an #xmmsc_service_notifier_t.
 * @param udata User data to pass to func.
 * @param ufree Optional function that should be called to free udata, an
 * #xmmsc_user_data_free_func_t.
 * @return 1 on success, 0 otherwise.
 */
int
xmmsc_service_method_add_noarg (xmmsc_service_t *svc, const char *name,
                                const char *desc, xmmsv_type_t rettype,
                                xmmsc_service_notifier_t func, void *udata,
                                xmmsc_user_data_free_func_t ufree)
{
	xmmsc_service_method_t *meth;

	x_return_val_if_fail (svc, 0);
	x_return_val_if_fail (name, 0);
	x_return_val_if_fail (desc, 0);
	x_return_val_if_fail (xmmsv_check_type (rettype), 0);

	meth = x_new (xmmsc_service_method_t, 1);
	meth->name = strdup (name);
	meth->desc = strdup (desc);
	meth->rettype = rettype;
	meth->func = func;
	meth->udata = udata;
	meth->ufree = ufree;
	meth->args = NULL;
	if (!(meth->name && meth->desc)) {
		xmmsc_service_method_free (meth);
		return 0;
	}

	svc->methods = x_list_append (svc->methods, (void *) meth);
	if (!svc->methods) {
		return 0;
	}

	return 1;
}

/**
 * Add an argument to a method. This will add an argument to the last method
 * added to the service.
 *
 * @param svc The #xmmsc_service_t containing the method to which this argument
 * will be added.
 * @param name The name of the argument.
 * @param type The type of the argument, an #xmmsv_type_t.
 * @return 1 on success, 0 otherwise.
 */
int
xmmsc_service_method_add_arg (xmmsc_service_t *svc, const char *name,
                              xmmsv_type_t type)
{
	x_list_t *tmp;
	xmmsv_t *val;

	x_return_val_if_fail (svc, 0);
	x_return_val_if_fail (name, 0);
	x_return_val_if_fail (xmmsv_check_type (type), 0);

	tmp = x_list_last (svc->methods);
	x_return_val_if_fail (tmp, 0);

	val = xmmsv_new_int ((int32_t) type);
	x_return_val_if_fail (val, 0);

	if (!tmp->args) {
		tmp->args = xmmsv_new_dict ();
	}

	if (!(tmp->args && xmmsv_dict_insert (tmp->args, name, val))) {
		xmmsv_unref (val);
		return 0;
	}

	xmmsv_unref (val);

	return 1;
}

/**
 * Increases the refcount of the #xmmsc_service_t.
 *
 * @param svc The #xmmsc_service_t to be referenced.
 * @return #xmmsc_service_t on success, NULL otherwise.
 */
xmmsc_service_t *
xmmsc_service_ref (xmmsc_service_t *svc)
{
	x_return_val_if_fail (svc, NULL);

	svc->ref++;

	return svc;
}

/**
 * Decreases the refcount of the #xmmsc_service_t. If there are no more
 * references to the #xmmsc_service_t, it will be freed.
 *
 * @param svc The #xmmsc_service_t to be unreferenced.
 */
void
xmmsc_service_unref (xmmsc_service_t *svc)
{
	x_return_if_fail (svc);

	svc->ref--;

	if (svc->ref == 0) {
		xmmsc_service_free (svc);
	}
}

/**
 * Free a service.
 *
 * @param svc The Service.
 */
static void
xmmsc_service_free (xmmsc_service_t *svc)
{
	x_return_if_fail (svc);

	if (svc->name) {
		free (svc->name);
	}
	if (svc->desc) {
		free (svc->desc);
	}
	if (svc->methods) {
		/* Walk the list, deleting each link as we do so. */
		while (svc->methods) {
			if (tmp->data) {
				xmmsc_service_method_free ((xmmsc_service_method_t *)tmp->data);
			}
			svc->methods = x_list_delete_link (svc->methods, svc->methods);
		}
	}

	free (svc);
}

/**
 * Free the given #xmmsc_service_method_t.
 *
 * @param meth The #xmmsc_service_method_t.
 */
static void
xmmsc_service_method_free (xmmsc_service_method_t *meth)
{
	x_return_if_fail (meth);

	if (meth->name) {
		free (name);
	}
	if (meth->desc) {
		free (meth->desc);
	}
	if (meth->ufree) {
		meth->ufree (udata);
	}
	if (meth->args) {
		xmmsv_list_clear (meth->args);
		xmmsv_unref (meth->args);
	}

	free (meth);
}

/* Stuff below this line hasn't been updated yet. */
/**
 * Register a new service.
 * @param conn The connection to the server.
 * @param svc The Service.
 */
xmmsc_result_t *
xmmsc_service_register (xmmsc_connection_t *conn, xmmsc_service_t *svc)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;
	x_list_t *n;

	x_check_conn (conn, NULL);
	x_return_null_if_fail (svc);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_REGISTER);
	xmms_ipc_msg_put_string (msg, svc->name);
	xmms_ipc_msg_put_string (msg, svc->description);
	xmms_ipc_msg_put_uint32 (msg, svc->major);
	xmms_ipc_msg_put_uint32 (msg, svc->minor);

	xmms_ipc_msg_put_uint32 (msg, x_list_length (svc->methods));
	if (svc->methods) {
		for (n = svc->methods; n; x_list_next (n)) {
			service_method_register (msg, (xmms_service_method_t *)n->data);
		}
	}

	res = xmmsc_send_msg (conn, msg);

	return res;
}

static void
service_method_register (xmms_ipc_msg_t *msg, xmmsc_service_method_t *method)
{
	x_list_t *n;
	xmmsc_service_argument_t *arg = NULL;

	xmms_ipc_msg_put_string (msg, method->name);
	xmms_ipc_msg_put_string (msg, method->description);

	xmms_ipc_msg_put_uint32 (msg, x_list_length (method->args));
	for (n = method->args; n; n = x_list_next (n)) {
		arg = (xmmsc_service_argument_t *)n->data;
		xmms_ipc_msg_put_string (msg, arg->name);
		xmms_ipc_msg_put_uint32 (msg, arg->type);
		xmms_ipc_msg_put_uint32 (msg, arg->optional);
	}

	res = xmmsc_send_msg (conn, msg);

	xmmsc_result_notifier_set_full (res, dispatch, method, free_infos);
	xmmsc_service_method_ref (method);
}

/**
 * Unregister an existing method.
 *
 * If the service does not have any methods left after removal of this method,
 * the service will be unregistered, too.
 *
 * @param conn The connection to the server.
 * @param service The name of the service which the method belongs to.
 * @param method The method to be removed.
 */
xmmsc_result_t *
xmmsc_service_unregister (xmmsc_connection_t *conn,
                          const char *service,
                          xmmsc_service_method_t *method)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);
	x_return_null_if_fail (service);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_UNREGISTER);
	xmms_ipc_msg_put_string (msg, service);

	if (method) {
		xmms_ipc_msg_put_string (msg, method->name);
		xmmsc_service_method_unref (method);
	}

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
xmmsc_service_list (xmmsc_connection_t *conn)
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
xmmsc_service_describe (xmmsc_connection_t *conn, const char *service)
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
xmmsc_service_method_list (xmmsc_connection_t *conn, const char *service)
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
xmmsc_service_method_describe (xmmsc_connection_t *conn, const char *service,
                               const char *method)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_METHOD_INFO_LIST);
	xmms_ipc_msg_put_string (msg, service);
	xmms_ipc_msg_put_string (msg, method);

	return xmmsc_send_msg (conn, msg);
}

/**
 * Make service method call.
 *
 * All argument values will be reset after this call.
 *
 * @param conn The connection to the server.
 * @param service The id of the service.
 * @param method The #xmmsc_service_method_t structure.
 */
xmmsc_result_t *
xmmsc_service_request (xmmsc_connection_t *conn, const char *service,
                       xmmsc_service_method_t *method)
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
			if (!argument_write (msg, arg)) {
				xmms_ipc_msg_destroy (msg);
				return 0;
			}

			arg_value_free (arg);
		}
	}

	arg_reset (method);

	res = xmmsc_send_msg (conn, msg);

	return res;
}

/**
 * Send shutdown broadcast to a service client.
 *
 * This does not guarantee that the service client will shutdown.  If you need
 * this guarantee, please consult Service Client Manager.
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
 * Increase the reference count.
 *
 * @param method The method to increase reference count on.
 */
void
xmmsc_service_method_ref (xmmsc_service_method_t *method)
{
	x_return_if_fail (method);

	method->ref++;
}

/**
 * Decrease the reference count.
 *
 * When the count reaches 0, the method will be freed.
 *
 * @param method The method to decrease reference count on.
 */
void
xmmsc_service_method_unref (xmmsc_service_method_t *method)
{
	x_return_if_fail (method);
	x_api_error_if (method->ref < 1, "with a freed method",);

	method->ref--;
	if (method->ref == 0) {
		xmmsc_service_method_free (method);
	}
}

/**
 * Get an attribute of a method.
 *
 * If you do not need any of the attribute, simply pass a NULL.  The returned
 * value is owned by the method.
 *
 * @param method The method containing the attribute.
 * @param name The returned name of the method.
 * @param description The returned description of the method.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_attribute_get (xmmsc_service_method_t *method,
                                    char **name, char **description)
{
	x_return_val_if_fail (method, 0);

	if (name && method->name) {
		*name = method->name;
	}
	if (description && method->description) {
		*description = method->description;
	}

	return !name && !description ? 0 : 1;
}

/**
 * Push new type to a method's argument list.
 *
 * @param method The method to push to.
 * @param name The name of the new type.
 * @param type The new type.
 * @param optional If the argument is optional.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_arg_type_add (xmmsc_service_method_t *method,
                                   const char *name,
                                   xmmsc_value_t type,
                                   int32_t optional)
{
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (name, 0);
	x_api_error_if (strcmp (name, "sc_id") == 0,
	                "Sorry, you cannot use \"sc_id\" as the argument name,"
	                " it is reserved for internal use only.", 0);

	arg = x_new0 (xmmsc_service_argument_t, 1);
	if (!arg) {
		return 0;
	}

	arg->name = strdup (name);
	arg->type = type;
	arg->optional = optional;
	method->arg_list = x_list_append (method->arg_list, arg);

	return 1;
}

/**
 * Push new type to a method's return value list.
 *
 * @param method The method to push to.
 * @param name The name of the new type.
 * @param type The new type.
 * @param optional If the return value is optional.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_ret_type_add (xmmsc_service_method_t *method,
                                   const char *name,
                                   xmmsc_service_arg_type_t type,
                                   int32_t optional)
{
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (name, 0);

	arg = x_new0 (xmmsc_service_argument_t, 1);
	if (!arg) {
		return 0;
	}

	arg->name = strdup (name);
	arg->type = type;
	arg->optional = optional;
	arg->none = 1;
	method->ret_list = x_list_append (method->ret_list, arg);

	return 1;
}

/**
 * Add value to argument list.
 *
 * @param method The method which contains the argument list.
 * @param name The name of the argument.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_arg_add_uint32 (xmmsc_service_method_t *method,
                                     const char *name, uint32_t value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup))) {
		return 0;
	}
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_UINT32) {
		return 0;
	}
	arg->value.uint32 = value;
	arg->none = 0;

	return 1;
}

/**
 * Add value to argument list.
 *
 * @param method The method which contains the argument list.
 * @param name The name of the argument.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_arg_add_int32 (xmmsc_service_method_t *method,
                                    const char *name, int32_t value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup))) {
		return 0;
	}
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_INT32) {
		return 0;
	}
	arg->value.int32 = value;
	arg->none = 0;

	return 1;
}

/**
 * Add value to argument list.
 *
 * @param method The method which contains the argument list.
 * @param name The name of the argument.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_arg_add_string (xmmsc_service_method_t *method,
                                     const char *name, char *value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;
	uint32_t l;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (value, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup))) {
		return 0;
	}
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_STRING) {
		return 0;
	}

	l = strlen (value);

	arg->value.string = x_malloc (l + 1);
	if (!arg->value.string) {
		return 0;
	}

	if (!data_copy (arg->value.string, value, l)) {
		free (arg->value.string);
		return 0;
	}

	arg->value.string[l] = '\0';

	arg->none = 0;

	return 1;
}

/**
 * Add value to argument list.
 *
 * @param method The method which contains the argument list.
 * @param name The name of the argument.
 * @param value The NULL-terminated string list going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_arg_add_stringlist (xmmsc_service_method_t *method,
                                         const char *name, char **value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;
	uint32_t len, i;
	uint32_t l;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (value, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup))) {
		return 0;
	}
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_STRINGLIST) {
		return 0;
	}

	for (len = 0; value[len]; len++);
	arg->value.strings = x_new (char *, len + 1);

	for (i = 0; i < len; i++) {
		l = strlen (value[i]);

		arg->value.strings[i] = x_malloc (l + 1);
		if (!arg->value.strings[i]) {
			while (i > 0) {
				free (arg->value.strings[--i]);
			}

			free (arg->value.strings);
			return 0;
		}

		if (!data_copy (arg->value.strings[i], value[i], l)) {
			while (i >= 0) {
				free (arg->value.strings[i--]);
			}

			free (arg->value.strings);
			return 0;
		}

		arg->value.strings[i][l] = '\0';
	}
	arg->value.strings[len] = NULL;

	arg->none = 0;

	return 1;
}

/**
 * Add value to argument list.
 *
 * @param method The method which contains the argument list.
 * @param name The name of the argument.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_arg_add_coll (xmmsc_service_method_t *method,
                                   const char *name, xmmsc_coll_t *value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (value, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup))) {
		return 0;
	}
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_COLL) {
		return 0;
	}
	arg->value.coll = value;
	arg->none = 0;

	return 1;
}

/**
 * Add value to argument list.
 *
 * @param method The method which contains the argument list.
 * @param name The name of the argument.
 * @param value The value going to be added.
 * @param len The length of the value in bytes.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_arg_add_bin (xmmsc_service_method_t *method,
                                  const char *name, unsigned char *value,
                                  uint32_t len)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (value, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup))) {
		return 0;
	}
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_BIN || len == 0) {
		return 0;
	}

	arg->value.bin = x_malloc (len);
	if (!arg->value.bin) {
		return 0;
	}

	if (!data_copy (arg->value.bin, value, len)) {
		free (arg->value.bin);
		return 0;
	}

	arg->len = len;
	arg->none = 0;

	return 1;
}

/**
 * Add value to return value list.
 *
 * @param method The method which contains the return value list.
 * @param name The name of the return value.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_ret_add_uint32 (xmmsc_service_method_t *method,
                                     const char *name, uint32_t value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup))) {
		return 0;
	}
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_UINT32) {
		return 0;
	}
	arg->value.uint32 = value;
	arg->none = 0;

	return 1;
}

/**
 * Add value to return value list.
 *
 * @param method The method which contains the return value list.
 * @param name The name of the return value.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_ret_add_int32 (xmmsc_service_method_t *method,
                                    const char *name, int32_t value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup))) {
		return 0;
	}
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_INT32) {
		return 0;
	}
	arg->value.int32 = value;
	arg->none = 0;

	return 1;
}

/**
 * Add value to return value list.
 *
 * @param method The method which contains the return value list.
 * @param name The name of the return value.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_ret_add_string (xmmsc_service_method_t *method,
                                     const char *name, char *value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;
	uint32_t l;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (value, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup))) {
		return 0;
	}
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_STRING) {
		return 0;
	}

	l = strlen (value);

	arg->value.string = x_malloc (l + 1);
	if (!arg->value.string) {
		return 0;
	}

	if (!data_copy (arg->value.string, value, l)) {
		free (arg->value.string);
		return 0;
	}

	arg->value.string[l] = '\0';

	arg->none = 0;

	return 1;
}

/**
 * Add value to return value list.
 *
 * @param method The method which contains the return value list.
 * @param name The name of the return value.
 * @param value The NULL-terminated string list going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_ret_add_stringlist (xmmsc_service_method_t *method,
                                         const char *name, char **value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;
	uint32_t len, i;
	uint32_t l;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (value, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup))) {
		return 0;
	}
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_STRINGLIST || len == 0) {
		return 0;
	}

	for (len = 0; value[len]; len++) { }
	arg->value.strings = x_new (char *, len + 1);

	for (i = 0; i < len; i++) {
		l = strlen (value[i]);

		arg->value.strings[i] = x_malloc (l + 1);
		if (!arg->value.strings[i]) {
			while (i > 0) {
				free (arg->value.strings[--i]);
			}

			free (arg->value.strings);
			return 0;
		}

		if (!data_copy (arg->value.strings[i], value[i], l)) {
			while (i >= 0) {
				free (arg->value.strings[i--]);
			}

			free (arg->value.strings);
			return 0;
		}

		arg->value.strings[i][l] = '\0';
	}
	arg->value.strings[len] = NULL;

	arg->none = 0;

	return 1;
}

/**
 * Add value to return value list.
 *
 * @param method The method which contains the return value list.
 * @param name The name of the return value.
 * @param value The value going to be added.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_ret_add_coll (xmmsc_service_method_t *method,
                                   const char *name, xmmsc_coll_t *value)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (value, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup))) {
		return 0;
	}
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_COLL) {
		return 0;
	}

	arg->value.coll = coll_copy (value);
	if (!arg->value.coll) {
		return 0;
	}

	arg->none = 0;

	return 1;
}

/**
 * Add value to return value list.
 *
 * @param method The method which contains the return value list.
 * @param name The name of the return value.
 * @param value The value going to be added.
 * @param len The length of the value in bytes.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_ret_add_bin (xmmsc_service_method_t *method,
                                  const char *name, unsigned char *value,
                                  uint32_t len)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (value, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup))) {
		return 0;
	}
	arg = (xmmsc_service_argument_t *)item->data;

	if (arg->type != XMMSC_SERVICE_ARG_TYPE_BIN || len == 0) {
		return 0;
	}

	arg->value.bin = x_malloc (len);
	if (!arg->value.bin) {
		return 0;
	}

	if (!data_copy (arg->value.bin, value, len)) {
		free (arg->value.bin);
		return 0;
	}

	arg->len = len;
	arg->none = 0;

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
 * If you do not need any of the attribute, simply pass a NULL.
 *
 * @param method The method which contains the argument list.
 * @param name The name of the argument.
 * @param type The returned type of the argument.
 * @param optional The returned optionality of the argument.
 * @param none The returned value of the none field of the argument.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_arg_attribute_get (xmmsc_service_method_t *method,
                                        const char *name,
                                        xmmsc_service_arg_type_t *type,
                                        uint32_t *optional, uint32_t *none)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (name, 0);

	if (!(item = x_list_find_custom (method->arg_list, name, arg_lookup))) {
		return 0;
	}
	arg = (xmmsc_service_argument_t *)item->data;

	return arg_attribute_get (arg, type, optional, none);
}

/**
 * Get an attribute of an argument.
 *
 * If you do not need any of the attribute, simply pass a NULL.
 *
 * @param method The method which contains the return value list.
 * @param name The name of the argument.
 * @param type The returned type of the argument.
 * @param optional The returned optionality of the argument.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_service_method_ret_attribute_get (xmmsc_service_method_t *method,
                                        const char *name,
                                        xmmsc_service_arg_type_t *type,
                                        uint32_t *optional)
{
	x_list_t *item;
	xmmsc_service_argument_t *arg;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (name, 0);

	if (!(item = x_list_find_custom (method->ret_list, name, arg_lookup))) {
		return 0;
	}
	arg = (xmmsc_service_argument_t *)item->data;

	return arg_attribute_get (arg, type, optional, none);
}

/**
 * @internal
 */
static xmmsc_result_t *
method_return (xmmsc_connection_t *conn, xmmsc_value_t *val,
               xmmsc_service_method_t *method)
{
	xmms_ipc_msg_t *msg;
	uint32_t cookie;
	x_list_t *n;
	xmmsc_service_argument_t *arg;

	x_check_conn (conn, NULL);
	x_return_null_if_fail (val);
	x_return_null_if_fail (method);
	if (!method->error && !method->ret_list) {
		return NULL;
	}

	if (!xmmsc_value_get_service_cookie (val, &cookie)) {
		return NULL;
	}

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
			if (arg->none) {
				continue;
			}
			if (!argument_write (msg, arg)) {
				xmms_ipc_msg_destroy (msg);
				return 0;
			}

			arg_value_free (arg);
		}
	}

	ret_reset (method);

	return xmmsc_send_msg (conn, msg);
}

static int
arg_attribute_get (xmmsc_service_argument_t *arg, xmmsc_service_arg_type_t *type,
                   uint32_t *optional, uint32_t *none)
{
	if (type) {
		*type = arg->type;
	}
	if (optional) {
		*optional = arg->optional;
	}
	if (none) {
		*none = arg->none;
	}

	return !type && !optional && !none ? 0 : 1;
}

/*
 * Destination has to have enough memory to hold len bytes of data.
 */
static int
data_copy (void *dest, const void *src, uint32_t len)
{
	if (!dest || !src) {
		return 0;
	}

	memcpy (dest, src, len);

	return 1;
}

static void
arg_value_free (xmmsc_service_argument_t *arg)
{
	uint32_t i;

	if (!arg) {
		return;
	}

	if (arg->type == XMMSC_SERVICE_ARG_TYPE_STRING) {
		if (arg->value.string) {
			free (arg->value.string);
		}
	} else if (arg->type == XMMSC_SERVICE_ARG_TYPE_STRINGLIST) {
		for (i = 0; arg->value.strings && arg->value.strings[i]; i++) {
			free (arg->value.strings[i]);
		}
		free (arg->value.strings);
	} else if (arg->type == XMMSC_SERVICE_ARG_TYPE_BIN) {
		if (arg->value.bin) {
			free (arg->value.bin);
		}
		arg->len = 0;
	} else if (arg->type == XMMSC_SERVICE_ARG_TYPE_COLL) {
		if (arg->value.coll) {
			xmmsc_coll_unref (arg->value.coll);
		}
	}
}

static void
arg_reset (xmmsc_service_method_t *method)
{
	x_list_t *n;

	if (!method) {
		return;
	}

	xmmsc_service_method_error_reset (method);

	for (n = method->arg_list; n; n = x_list_next (n)) {
		((xmmsc_service_argument_t *)n->data)->none = 1;
	}
}

static void
ret_reset (xmmsc_service_method_t *method)
{
	x_list_t *n;

	if (!method) {
		return;
	}

	xmmsc_service_method_error_reset (method);

	for (n = method->ret_list; n; n = x_list_next (n)) {
		((xmmsc_service_argument_t *)n->data)->none = 1;
	}
}

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
	                (const char *)name) == 0) {
		return 0;
	}

	return 1;
}

static void
free_infos (void *data)
{
	xmmsc_service_method_t *method = (xmmsc_service_method_t *)data;

	xmmsc_service_method_unref (method);
}

static void
dummy_handler (xmmsc_value_t *val, void *data)
{

}

static void
dispatch (xmmsc_value_t *val, void *data)
{
	xmmsc_service_method_t *method = (xmmsc_service_method_t *)data;
	xmmsc_result_t *result;

	x_return_if_fail (method);

	method->func (method->conn, val, method, method->udata);

	result = method_return (method->conn, val, method);
	if (result) {
		xmmsc_result_notifier_set (result, dummy_handler, NULL);
		xmmsc_result_unref (result);
	}
}

static xmmsc_coll_t *
coll_copy (xmmsc_coll_t *coll)
{
	xmmsc_coll_t *op, *new_op;
	xmmsc_coll_t *new_coll;

	x_return_null_if_fail (coll);

	new_coll = xmmsc_coll_new (xmmsc_coll_get_type (coll));

	xmmsc_coll_attribute_foreach (coll, coll_attr_copy, new_coll);

	xmmsc_coll_set_idlist (new_coll, xmmsc_coll_get_idlist (coll));

	xmmsc_coll_operand_list_save (coll);

	if (xmmsc_coll_get_type (coll) != XMMS_COLLECTION_TYPE_REFERENCE) {
		xmmsc_coll_operand_list_first (coll);
		while (xmmsc_coll_operand_list_entry (coll, &op)) {
			new_op = coll_copy (op);
			if (!new_op) {
				xmmsc_coll_unref (new_coll);
				return NULL;
			}

			xmmsc_coll_add_operand (new_coll, new_op);
			xmmsc_coll_unref (new_op);
			xmmsc_coll_operand_list_next (coll);
		}
	}

	xmmsc_coll_operand_list_restore (coll);

	return new_coll;
}

static void
coll_attr_copy (const char *key, const char *value, void *userdata)
{
	xmmsc_coll_t *coll = (xmmsc_coll_t *)userdata;

	xmmsc_coll_attribute_set (coll, key, value);
}

/* @} */
