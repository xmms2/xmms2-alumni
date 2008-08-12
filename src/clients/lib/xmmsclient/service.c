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

typedef struct xmmsc_service_dispatch_St {
	xmmsc_connection_t *conn;
	xmmsc_service_t *svc;
} xmmsc_service_dispatch_t;

/* Macro Magic */
#define DICT_ADD_INT(dict, key, n)                     \
	val = xmmsv_new_int ((int32_t) n);                 \
	if (!val || !xmmsv_dict_insert (dict, key, val)) { \
		goto err;                                      \
	}                                                  \
	xmmsv_unref (val);                                 \
	val = NULL;

#define DICT_ADD_UINT(dict, key, n)                    \
	val = xmmsv_new_uint ((uint32_t) n);               \
	if (!val || !xmmsv_dict_insert (dict, key, val)) { \
		goto err;                                      \
	}                                                  \
	xmmsv_unref (val);                                 \
	val = NULL;

#define DICT_ADD_STRING(dict, key, str)                \
	val = xmmsv_new_string (str);       \
	if (!val || !xmmsv_dict_insert (dict, key, val)) { \
		goto err;                                      \
	}                                                  \
	xmmsv_unref (val);                                 \
	val = NULL;

static int xmmsc_service_method_add_valist (xmmsc_service_t *svc,
                                            const char *name, const char *desc,
                                            xmmsv_type_t rettype,
                                            xmmsc_service_notifier_t func,
                                            void *udata,
                                            xmmsc_user_data_free_func_t ufree,
                                            va_list ap);
static void xmmsc_service_free (xmmsc_service_t *svc);
static void xmmsc_service_method_free (xmmsc_service_method_t *meth);
static xmmsv_t *xmmsc_service_methods_to_value (x_list_t *methods);
static xmmsc_service_dispatch_t *
xmmsc_service_dispatch_new (xmmsc_connection_t *conn, xmmsc_service_t *svc);
static void xmmsc_service_dispatch_free (void *data);
static int xmmsc_service_dispatch (xmmsv_t *val, void *data);
static xmmsc_result_t *
xmmsc_service_method_handle (xmmsc_connection_t *conn, xmmsc_service_t *svc,
                             xmmsc_service_method_t *method, xmmsv_t *args);
static int xmmsc_service_method_check_args (xmmsv_t *argtypes, xmmsv_t *args);
static int xmmsc_service_dummy_handler (xmmsv_t *val, void *data);

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
	x_return_null_if_fail (desc);

	svc = x_new (xmmsc_service_t, 1);
	x_return_null_if_fail (svc);

	svc->name = strdup (name);
	svc->desc = strdup (desc);
	svc->major = major;
	svc->minor = minor;
	svc->methods = NULL;
	svc->ref = 1;

	if (!(svc->name && svc->desc)) {
		xmmsc_service_free (svc);
		return NULL;
	}

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
 * @param ... NULL-terminated list of const char *, xmmsv_type_t, int32_t.
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
 * @param ... NULL-terminated list of const char *, xmmsv_type_t, int32_t.
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

	va_start (ap, ufree);
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
	int32_t optional;
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

		optional = va_arg (ap, int32_t);

		xmmsc_service_method_add_arg (svc, arg, type, optional);
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
	x_return_val_if_fail (rettype >= XMMSV_TYPE_NONE &&
	                      rettype < XMMSV_TYPE_END, 0);

	meth = x_new (xmmsc_service_method_t, 1);
	meth->name = strdup (name);
	meth->desc = strdup (desc);
	meth->rettype = rettype;
	meth->func = func;
	meth->udata = udata;
	meth->ufree = ufree;
	meth->args = xmmsv_new_list ();
	if (!(meth->name && meth->desc && meth->args)) {
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
 * @param optional The argument is mandatory for 0, optional for all other
 * values.
 * @return 1 on success, 0 otherwise.
 */
int
xmmsc_service_method_add_arg (xmmsc_service_t *svc, const char *name,
                              xmmsv_type_t type, int32_t optional)
{
	x_list_t *tmp;
	xmmsc_service_method_t *meth;
	xmmsv_t *arg = NULL;
	xmmsv_t *val = NULL;

	x_return_val_if_fail (svc, 0);
	x_return_val_if_fail (name, 0);
	x_return_val_if_fail (xmmsv_check_type (type), 0);

	tmp = x_list_last (svc->methods);
	x_return_val_if_fail (tmp, 0);

	meth = (xmmsc_service_method_t *) tmp->data;
	x_return_val_if_fail (meth, 0);

	arg = xmmsv_new_dict ();
	if (!arg) {
		goto err;
	}

	/* arg = { "name" => name } */
	DICT_ADD_STRING (arg, XMMSC_SERVICE_METHOD_ARG_PROP_NAME, name);

	/* arg = { "name" => name, "type" => type } */
	DICT_ADD_INT (arg, XMMSC_SERVICE_METHOD_ARG_PROP_TYPE, type);

	/* arg = { "name" => name, "type" => type, "optional" => optional } */
	DICT_ADD_INT (arg, XMMSC_SERVICE_METHOD_ARG_PROP_OPTIONAL, optional);

	if (!xmmsv_list_append (meth->args, arg)) {
		goto err;
	}

	return 1;

err:
	if (arg) {
		xmmsv_dict_clear (arg);
		xmmsv_unref (arg);
	}
	if (val) {
		xmmsv_unref (val);
	}

	return 0;
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
	x_list_t *tmp;

	x_return_if_fail (svc);

	if (svc->name) {
		free (svc->name);
	}
	if (svc->desc) {
		free (svc->desc);
	}
	if (svc->methods) {
		/* Walk the list, deleting each link as we do so. */
		for (tmp = svc->methods; tmp; tmp = x_list_next (tmp)) {
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
		free (meth->name);
	}
	if (meth->desc) {
		free (meth->desc);
	}
	if (meth->ufree && meth->udata) {
		meth->ufree (meth->udata);
	}
	if (meth->args) {
		xmmsv_list_clear (meth->args);
		xmmsv_unref (meth->args);
	}

	free (meth);
}

/**
 * Register a new service.
 * @param conn The connection to the server.
 * @param svc The service.
 */
xmmsc_result_t *
xmmsc_service_register (xmmsc_connection_t *conn, xmmsc_service_t *svc)
{
	xmms_ipc_msg_t *msg;
	xmmsv_t *service;
	xmmsv_t *val = NULL;
	xmmsc_result_t *res;
	xmmsc_service_dispatch_t *sd;

	x_check_conn (conn, NULL);
	x_return_null_if_fail (svc);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_REGISTER);
	x_return_null_if_fail (msg);

	service = xmmsv_new_dict ();
	x_return_null_if_fail (service);

	/* service = { "name" => name } */
	DICT_ADD_STRING (service, XMMSC_SERVICE_PROP_NAME, svc->name);

	/* service = { "name" => name , "description" => desc } */
	DICT_ADD_STRING (service, XMMSC_SERVICE_PROP_DESCRIPTION, svc->desc);

	/* service = { "name" => name , "description" => desc, "major" => major } */
	DICT_ADD_UINT (service, XMMSC_SERVICE_PROP_MAJOR, svc->major);

	/* service = { "name" => name , "description" => desc, "major" => major,
	               "minor" => minor } */
	DICT_ADD_UINT (service, XMMSC_SERVICE_PROP_MINOR, svc->minor);

	/* service = { "name" => name , "description" => desc, "major" => major,
	               "minor" => minor, "methods" => { ... } } */
	val = xmmsc_service_methods_to_value (svc->methods);
	if (!val || !xmmsv_dict_insert (service, XMMSC_SERVICE_PROP_METHODS, val)) {
		goto err;
	}
	xmmsv_unref (val);

	xmms_ipc_msg_put_value_dict (msg, service);
	xmmsv_unref (service);

	res = xmmsc_send_msg (conn, msg);

	sd = xmmsc_service_dispatch_new (conn, svc);
	if (sd) {
		xmmsc_result_notifier_set_full (res, xmmsc_service_dispatch,
		                                (void *) sd,
		                                xmmsc_service_dispatch_free);
	}

	return res;

err:
	if (val) {
		xmmsv_unref (val);
	}
	xmmsv_unref (service);

	return NULL;
}

static xmmsv_t *
xmmsc_service_methods_to_value (x_list_t *meths)
{
	xmmsc_service_method_t *meth;
	x_list_t *tmp;
	xmmsv_t *methods;
	xmmsv_t *method = NULL;
	xmmsv_t *val = NULL;

	methods = xmmsv_new_dict ();
	x_return_null_if_fail (methods);

	if (!meths || !meths->data) {
		return methods;
	}

	for (tmp = meths; tmp; tmp = x_list_next (tmp)) {
		meth = (xmmsc_service_method_t *) tmp->data;
		method = xmmsv_new_dict ();
		if (!method) {
			goto err;
		}
		/* method = { "description" => desc } */
		DICT_ADD_STRING (method, XMMSC_SERVICE_METHOD_PROP_DESCRIPTION,
		                 meth->desc);

		/* method = { "description" => desc, "args" => [ ... ] } */
		if (!xmmsv_dict_insert (method, XMMSC_SERVICE_METHOD_PROP_ARGUMENTS,
		                        meth->args)) {
			goto err;
		}

		/* method = { "description" => desc, "args" => [ ... ],
		              "rettype" => rettype } */
		DICT_ADD_INT (method, XMMSC_SERVICE_METHOD_PROP_RETURN_TYPE,
		              meth->rettype);
		val = xmmsv_new_int ((int32_t) meth->rettype);
		if (!xmmsv_dict_insert (method, XMMSC_SERVICE_METHOD_PROP_RETURN_TYPE,
		                        val)) {
			goto err;
		}
		xmmsv_unref (val);

		if (!xmmsv_dict_insert (methods, meth->name, method)) {
			goto err;
		}
	}

	return methods;

err:
	if (val) {
		xmmsv_unref (val);
	}
	if (method) {
		xmmsv_dict_clear (method);
		xmmsv_unref (method);
	}
	xmmsv_dict_clear (methods);
	xmmsv_unref (methods);
	return NULL;
}

static xmmsc_service_dispatch_t *
xmmsc_service_dispatch_new (xmmsc_connection_t *conn, xmmsc_service_t *svc)
{
	xmmsc_service_dispatch_t *sd;

	x_return_null_if_fail (conn);
	x_return_null_if_fail (svc);

	/* Effectively, all members are initialized to NULL. */
	sd = x_new0 (xmmsc_service_dispatch_t, 1);
	x_return_null_if_fail (sd);

	sd->conn = xmmsc_ref (conn);
	sd->svc = xmmsc_service_ref (svc);
	if (!sd->conn || !sd->svc) {
		goto err;
	}

	return sd;

err:
	if (sd->conn) {
		xmmsc_unref (sd->conn);
	}
	if (sd->svc) {
		xmmsc_service_unref (sd->svc);
	}

	return NULL;
}

static void
xmmsc_service_dispatch_free (void *data)
{
	xmmsc_service_dispatch_t *sd = (xmmsc_service_dispatch_t *) data;
	x_return_if_fail (sd);

	xmmsc_unref (sd->conn);
	xmmsc_service_unref (sd->svc);
}

/**
 * Unregister a registered service.
 *
 * @param conn The connection to the server.
 * @param service The service.
 */
xmmsc_result_t *
xmmsc_service_unregister (xmmsc_connection_t *conn, xmmsc_service_t *svc)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);
	x_return_null_if_fail (svc);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_UNREGISTER);
	xmms_ipc_msg_put_string (msg, svc->name);

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
	                        XMMS_IPC_CMD_SERVICE_DESCRIBE);
	xmms_ipc_msg_put_string (msg, service);

	return xmmsc_send_msg (conn, msg);
}

/**
 * Make service method call.
 *
 * All argument values will be reset after this call.
 *
 * @param conn The connection to the server.
 * @param svc The name of the service containing the method to be called.
 * @param meth The name of the method to be called.
 * @param args An #xmmsv_t containing a list of dictionaries each containing
 * XMMSC_SERVICE_METHOD_ARG_PROP_NAME => string,
 * XMMSC_SERVICE_METHOD_ARG_PROP_VALUE => xmmsv_t.
 * describing the arguments to the method.
 * @return An #xmmsc_result_t containing a #value_t with either a return value
 * or an error.
 */
xmmsc_result_t *
xmmsc_service_query (xmmsc_connection_t *conn, const char *svc,
                     const char *meth, xmmsv_t *args)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);
	x_return_null_if_fail (svc);
	x_return_null_if_fail (meth);

	if (!args) {
		args = xmmsv_new_list ();
	}
	x_return_null_if_fail (args);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_QUERY);
	x_return_null_if_fail (msg);

	xmms_ipc_msg_put_string (msg, svc);
	xmms_ipc_msg_put_string (msg, meth);
	xmms_ipc_msg_put_value_list (msg, args);

	return xmmsc_send_msg (conn, msg);
}

/**
 * Send shutdown broadcast to a service client.
 *
 * This does not guarantee that the service client will shutdown.  If you need
 * this guarantee, please consult Service Client Manager.
 *
 * @param conn The connection to the server.
 * @param service The name of the service to be shutdown.
 */
xmmsc_result_t *
xmmsc_service_shutdown (xmmsc_connection_t *conn, const char *svc)
{
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);
	x_return_val_if_fail (svc, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_SHUTDOWN);
	xmms_ipc_msg_put_string (msg, svc);

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
 * @internal
 */
static int
xmmsc_service_dispatch (xmmsv_t *val, void *data)
{
	xmmsc_service_dispatch_t *sd = (xmmsc_service_dispatch_t *)data;
	xmmsc_service_method_t *meth = NULL;
	x_list_t *tmp;
	xmmsv_t *method;
	xmmsc_result_t *res;
	const char *name;

	x_return_val_if_fail (sd, 0);
	if (xmmsv_get_type (val) == XMMSV_TYPE_NONE) {
		/* This notifier was called after service_register, ignore it. */
		return 1;
	}

	x_return_val_if_fail (xmmsv_dict_get (val, "method", &method), 1);
	x_return_val_if_fail (xmmsv_get_string (method, &name), 1);
	for (tmp = sd->svc->methods; tmp; tmp = x_list_next (tmp)) {
		meth = (xmmsc_service_method_t *) tmp->data;
		x_return_val_if_fail (meth, 1);

		if (strcmp (meth->name, name) == 0) {
			break;
		}
	}
	if (!tmp) {
		return 1;
	}

	res = xmmsc_service_method_handle (sd->conn, sd->svc, meth, val);
	if (res) {
		xmmsc_result_notifier_set (res, xmmsc_service_dummy_handler, NULL);
		xmmsc_result_unref (res);
	}

	return 1;
}

static xmmsc_result_t *
xmmsc_service_method_handle (xmmsc_connection_t *conn, xmmsc_service_t *svc,
                             xmmsc_service_method_t *method, xmmsv_t *val)
{
	xmms_ipc_msg_t *msg;
	xmmsv_t *ret;
	xmmsv_t *tmp;
	xmmsv_t *args;
	uint32_t cookie;

	if (xmmsv_get_type (val) != XMMSV_TYPE_DICT) {
		return NULL;
	}
	x_return_null_if_fail (xmmsv_dict_get (val, "args", &args));
	x_return_null_if_fail (xmmsv_dict_get (val, "cookie", &tmp));
	x_return_null_if_fail (xmmsv_get_uint (tmp, &cookie));

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_CMD_SERVICE_RETURN);
	x_return_null_if_fail (msg);

	xmms_ipc_msg_put_uint32 (msg, cookie);

	if (xmmsc_service_method_check_args (method->args, args)) {
		tmp = method->func (conn, svc, method->name,
		                    args, method->udata);
		if (!val) {
			tmp = xmmsv_new_error ("No return value from service method.");
			x_return_null_if_fail (tmp);
		}
	} else {
		tmp = xmmsv_new_error ("Invalid arguments.");
		x_return_null_if_fail (tmp);
	}

	/* FIXME: HACK! The service method can return any type, but the server needs
	   to know what to expect, so we pack the return type into a list. */
	ret = xmmsv_new_list ();
	if (!ret) {
		xmmsv_unref (tmp);
		return NULL;
	}
	if (!xmmsv_list_append (ret, tmp)) {
		xmmsv_unref (tmp);
		xmmsv_unref (ret);
		return NULL;
	}

	xmms_ipc_msg_put_value_data (msg, ret);
	xmmsv_unref (ret);

	return xmmsc_send_msg (conn, msg);
}

static int
xmmsc_service_method_check_args (xmmsv_t *argtypes, xmmsv_t *args)
{
	xmmsv_list_iter_t *argit;
	xmmsv_list_iter_t *typeit;
	xmmsv_t *arg;
	xmmsv_t *typearg;
	xmmsv_t *val;
	xmmsv_t *type;
	const char *argname;
	const char *typename;
	int match;
	int32_t optional;
	xmmsv_type_t argtype;

	x_return_val_if_fail (argtypes, 0);
	x_return_val_if_fail (args, 0);

	if (xmmsv_list_get_size (args) == 0 &&
	    xmmsv_list_get_size (argtypes) == 0) {
		return 1;
	}

	x_return_val_if_fail (xmmsv_get_list_iter (args, &argit), 0);
	x_return_val_if_fail (xmmsv_get_list_iter (argtypes, &typeit), 0);

	for (xmmsv_list_iter_first (typeit); xmmsv_list_iter_valid (typeit);
	     xmmsv_list_iter_next (typeit))
	{
		match = 0;

		for (xmmsv_list_iter_first (argit); xmmsv_list_iter_valid (argit);
		     xmmsv_list_iter_next (argit))
		{
			x_return_val_if_fail (xmmsv_list_iter_entry (argit, &arg), 0);
			x_return_val_if_fail (xmmsv_list_iter_entry (typeit, &typearg), 0);
			x_return_val_if_fail (xmmsv_dict_get (arg,
			                                      XMMSC_SERVICE_METHOD_ARG_PROP_NAME,
			                                      &val), 0);
			x_return_val_if_fail (xmmsv_dict_get (typearg,
			                                      XMMSC_SERVICE_METHOD_ARG_PROP_NAME,
			                                      &type), 0);
			x_return_val_if_fail (xmmsv_get_string (val, &argname), 0);
			x_return_val_if_fail (xmmsv_get_string (type, &typename), 0);

			if (strcmp (argname, typename) != 0) {
				continue;
			} else {
				match++;
			}

			x_return_val_if_fail (xmmsv_dict_get (arg,
			                                      XMMSC_SERVICE_METHOD_ARG_PROP_VALUE,
			                                      &val), 0);
			x_return_val_if_fail (xmmsv_dict_get (typearg,
			                                      XMMSC_SERVICE_METHOD_ARG_PROP_TYPE,
			                                      &type), 0);
			x_return_val_if_fail (xmmsv_get_int (type, (int32_t *) &argtype),
			                                     0);

			if (xmmsv_get_type (val) == argtype) {
				/* Don't search anymore. */
				break;
			} else {
				/* Houston, we have a problem. */
				return 0;
			}
		}

		if (match == 0) {
			x_return_val_if_fail (xmmsv_list_iter_entry (typeit, &typearg), 0);
			x_return_val_if_fail (xmmsv_dict_get (typearg,
			                                      XMMSC_SERVICE_METHOD_ARG_PROP_OPTIONAL,
			                                      &val), 0);
			x_return_val_if_fail (xmmsv_get_int (val, &optional), 0);
			if (optional == 0) {
				/* There was no method by this name and it's not optional. */
				return 0;
			}
			/* else { We're in the clear, this is an optional method. } */
		}
	}

	return 1;
}

static int
xmmsc_service_dummy_handler (xmmsv_t *val, void *data)
{
	/* Is this /really/ necessary? */
	return 0;
}

/* @} */
