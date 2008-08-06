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

#include <stdlib.h>
#include <string.h>
#include "xmms/xmms_log.h"
#include "xmms/xmms_service.h"
#include "xmmspriv/xmms_ipc.h"
#include "xmmspriv/xmms_service.h"

/**
 * @defgroup Service Service
 * @ingroup XMMSServer
 * @brief Service client functions for XMMS2 Daemon
 * @{
 */

#define XMMS_SERVICE_ERROR(err, code, message) \
{ \
	xmms_log_error ("%s", message); \
	xmms_error_set (err, code, message); \
}

/**
 * @internal
 *
 * Service registry structure
 *
 * This is the structure that will emit all the service signals.
 * It includes the list of registered services and client-service pool.
 */
struct xmms_service_registry_St {
	xmms_object_t object;

	GMutex *mutex;

	/* The service client registry. */
	GTree *services;

	/* The client-service pool. */
	GTree *clients;
};

/**
 * @internal
 *
 * A single service representation
 */
struct xmms_service_entry_St {
	/* The description of the service and all its methods. */
	xmmsv_t *description;

	/* A client should have exclusive access when querying a service client. */
	GMutex *mutex;

	/* Service client fd */
	xmms_socket_t sc;
};

/**
 * @internal
 *
 * Client representation
 */
typedef struct xmms_service_client_St {
	xmms_socket_t fd;
	guint cookie;
	gchar *method;
} xmms_service_client_t;

/**
 * Functions
 */
static void xmms_service_register (xmms_service_registry_t *registry,
                                   xmmsv_t *description, int32_t fd,
                                   uint32_t cookie, xmms_error_t *err);
static void xmms_service_destroy (xmms_object_t *object);

static xmms_service_entry_t *xmms_service_entry_new (xmms_socket_t client,
                                                     xmmsv_t *svc);
static gboolean xmms_service_querying_client_unregister (gpointer k,
                                                         gpointer v,
                                                         gpointer d);
static void xmms_service_entry_free (gpointer data);

static void xmms_service_list (xmms_service_registry_t *registry,
                               xmms_object_cmd_arg_t *arg);
static void xmms_service_key_insert (gpointer key, gpointer value,
                                     gpointer data);

static xmmsv_t *
xmms_service_changed_msg_new (xmmsv_t *svc,
                              xmms_service_changed_actions_t type);

static void xmms_service_querying_client_free (gpointer value);
static gint uint_compare (gconstpointer a, gconstpointer b, gpointer data);

/*
static void xmms_service_unregister (xmms_service_t *xmms_service,
                                     xmms_ipc_msg_t *msg,
                                     xmms_socket_t client,
                                     xmms_error_t *err);
static gboolean xmms_service_request (xmms_service_t *xmms_service,
                                      xmms_ipc_msg_t *msg,
                                      xmms_socket_t client,
                                      xmms_error_t *err);
static void xmms_service_return (xmms_service_t *xmms_service,
                                 xmms_ipc_msg_t *msg,
                                 xmms_error_t *err);
static void xmms_service_shutdown (xmms_service_t *xmms_service,
                                   xmms_ipc_msg_t *msg, xmms_error_t *err);

static xmms_service_entry_t *
xmms_service_is_registered (xmms_service_t *xmms_service, const gchar *name);
static xmms_service_method_t *
xmms_service_method_is_registered (xmms_service_entry_t *entry,
                                   const gchar *name);
static gboolean xmms_service_matchsc (gpointer key, gpointer value,
                                      gpointer data);
static gboolean xmms_service_method_request_matchfd (gpointer key,
                                                     gpointer value,
                                                     gpointer data);
static gboolean xmms_service_method_request_matchmethod (gpointer key,
                                                         gpointer value,
                                                         gpointer data);
static inline guint xmms_service_next_id (void);
static xmms_service_entry_t *xmms_service_get (xmms_service_t *xmms_service,
                                               xmms_ipc_msg_t *msg, gchar **name,
                                               xmms_error_t *err);
static GTree *xmms_service_arg_types_parse (xmms_ipc_msg_t *msg,
                                            xmms_error_t *err);
static GTree *xmms_service_args_parse (xmms_ipc_msg_t *msg,
                                       GTree *args, xmms_error_t *err);
static gboolean xmms_service_args_is_error (xmms_ipc_msg_t *msg);
static gboolean xmms_service_args_error_parse (xmms_ipc_msg_t *msg,
                                               gchar **error, xmms_error_t *err);
*/

XMMS_SVC_CMD_DEFINE (svc_register, xmms_service_register,
                     xmms_service_registry_t *, NONE,
                     DICT, NONE, NONE, NONE);

/**
 * Initialize service client handling
 */
gboolean
xmms_service_init (void)
{
	xmms_service_registry_t *ret = NULL;

	ret = xmms_object_new (xmms_service_registry_t, xmms_service_destroy);
	if (!ret) {
		xmms_log_error ("Failed to create service object.");
		goto err;
	}

	ret->mutex = g_mutex_new ();
	ret->services = g_tree_new_full ((GCompareDataFunc) strcmp, NULL, NULL,
	                                 xmms_service_entry_free);
	ret->clients = g_tree_new_full (uint_compare, NULL, NULL,
	                                xmms_service_querying_client_free);

	if (!(ret->mutex && ret->services && ret->clients)) {
		xmms_log_error ("Failed to populate service object.");
		goto err;
	}

	/* Yay, now the registry is an object that is passed back to us for all
	   calls. */
	xmms_ipc_object_register (XMMS_IPC_OBJECT_SERVICE, XMMS_OBJECT (ret));

	/* Register methods. */
	xmms_object_cmd_add (XMMS_OBJECT (ret), XMMS_IPC_CMD_SERVICE_REGISTER,
	                     XMMS_CMD_FUNC (svc_register));

	/* Register broadcasts. */
	xmms_ipc_broadcast_register (XMMS_OBJECT (ret), XMMS_IPC_SIGNAL_SERVICE);
	xmms_ipc_broadcast_register (XMMS_OBJECT (ret),
	                             XMMS_IPC_SIGNAL_SERVICE_CHANGED);
	xmms_ipc_broadcast_register (XMMS_OBJECT (ret),
	                             XMMS_IPC_SIGNAL_SERVICE_SHUTDOWN);

	XMMS_DBG ("Service object initialized.");

	return true;

err:
	if (ret) {
		xmms_service_destroy (XMMS_OBJECT (ret));
	}

	return false;
}

/**
 * Handle service related commands
 *
 * Based on the type parameter passed in, specific functions will be called to
 * handle the command.
 *
 * @param msg Message from (service) client.
 * @param cmdid Command ID.
 * @param client Service client's transport fd.
 * @param arg Command arguments.
 * @returns True if the cookie needs to be added to broadcast list, false otherwise
 */
/*
gboolean
xmms_service_handle (xmms_object_t *obj, xmms_ipc_msg_t *msg,
                     xmms_ipc_cmds_t cmdid, xmms_socket_t client,
                     xmms_object_cmd_arg_t *arg)
{
	xmms_service_registry_t *registry = (xmms_service_registry_t *)obj;

	g_return_val_if_fail (registry, FALSE);

	switch (cmdid) {
	case XMMS_IPC_CMD_SERVICE_REGISTER:
		arg->retval = xmms_object_cmd_value_none_new ();
		xmms_service_register (registry, msg, client, &arg->error);
		return FALSE;
	case XMMS_IPC_CMD_SERVICE_UNREGISTER:
		if (arg) {
			arg->retval = xmms_object_cmd_value_none_new ();
			xmms_service_unregister (registry, msg, client, &arg->error);
		} else {
			xmms_service_unregister (registry, msg, client, NULL);
		}
		return FALSE;
	case XMMS_IPC_CMD_SERVICE_LIST:
		xmms_service_list (registry, arg);
		return FALSE;
	case XMMS_IPC_CMD_SERVICE_DESCRIBE:
		return FALSE;
	case XMMS_IPC_CMD_SERVICE_REQUEST:
		arg->retval = xmms_object_cmd_value_none_new ();
		return xmms_service_request (registry, msg, client, &arg->error);
	case XMMS_IPC_CMD_SERVICE_RETURN:
		arg->retval = xmms_object_cmd_value_none_new ();
		xmms_service_return (registry, msg, &arg->error);
		return FALSE;
	case XMMS_IPC_CMD_SERVICE_SHUTDOWN:
		arg->retval = xmms_object_cmd_value_none_new ();
		xmms_service_shutdown (registry, msg, &arg->error);
		return FALSE;
	default:
		XMMS_SERVICE_ERROR (&arg->error, XMMS_ERROR_INVAL,
		                    "Invalid service command");
		return FALSE;
	}
}
*/

/**
 * Register a new service
 */
static void
xmms_service_register (xmms_service_registry_t *registry, xmmsv_t *description,
                       int32_t fd, uint32_t cookie, xmms_error_t *err)
{
	const gchar *name = NULL;
	gchar *key = NULL;;
	xmms_service_entry_t *entry = NULL;
	xmmsv_t *ret;
	xmms_ipc_msg_t *msg;

	entry = xmms_service_entry_new ((xmms_socket_t) fd, description);
	xmmsv_unref (description);

	if (!xmmsv_get_string (description, &name) || !name) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NO_SAUSAGE,
		                    "Failed to extract service name.");
		goto err;
	}

	key = g_strdup (name);
	if (!key) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NO_SAUSAGE,
		                    "Failed to copy service name.");
		goto err;
	}

	if (!entry) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NO_SAUSAGE,
		                    "Failed to create new service entry.");
		goto err;
	}

	g_mutex_lock (registry->mutex);
	g_tree_insert (registry->services, (gpointer) key, (gpointer) entry);
	g_mutex_unlock (registry->mutex);

	ret = xmms_service_changed_msg_new (entry->description,
	                                    XMMS_SERVICE_CHANGED_REGISTER);
	if (!ret) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT,
		                    "Failed to set broadcast message.");
		goto err;
	}

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE,
	                        XMMS_IPC_SIGNAL_SERVICE_CHANGED);
	xmms_ipc_msg_put_value (msg, ret);
	xmmsv_unref (ret);

	XMMS_DBG ("New service registered");

	return;

err:
	if (entry) {
		xmms_service_entry_free ((gpointer) entry);
	}

	return;
}

static xmms_service_entry_t *
xmms_service_entry_new (xmms_socket_t client, xmmsv_t *svc)
{
	xmms_service_entry_t *entry;

	entry = g_new0 (xmms_service_entry_t, 1);
	g_return_val_if_fail (entry, NULL);

	entry->description = xmmsv_ref (svc);
	if (!entry->description) {
		goto err;
	}
	entry->mutex = g_mutex_new ();
	if (!entry->mutex) {
		goto err;
	}

	entry->sc = client;

	return entry;

err:
	xmms_service_entry_free ((gpointer) entry);

	xmms_log_error ("Service entry initialization failed!");

	return NULL;
}

/**
 * Free all the memory used by xmms_service_registry_t
 */
static void
xmms_service_destroy (xmms_object_t *object)
{
	xmms_service_registry_t *registry = (xmms_service_registry_t *)object;

	g_return_if_fail (registry);

	if (registry->mutex) {
		g_mutex_free (registry->mutex);
	}

	if (registry->services) {
		g_tree_destroy (registry->services);
	}
	if (registry->clients) {
		g_tree_foreach (registry->clients,
		                xmms_service_querying_client_unregister, registry);
		g_tree_destroy (registry->clients);
	}

	xmms_ipc_signal_unregister (XMMS_IPC_SIGNAL_SERVICE);
	xmms_ipc_object_unregister (XMMS_IPC_OBJECT_SERVICE);

	XMMS_DBG ("Service object shutdown.");
}

static void
xmms_service_entry_free (gpointer data)
{
	xmms_service_entry_t *entry = (xmms_service_entry_t *) data;

	g_return_if_fail (entry);

	if (entry->description) {
		xmmsv_unref (entry->description);
	}
	if (entry->mutex) {
		g_mutex_free (entry->mutex);
	}

	g_free (entry);
}

/**
 * Send all querying clients an error message indicating that the service
 * method has been destroyed.
 */
static gboolean
xmms_service_querying_client_unregister (gpointer k, gpointer v, gpointer d)
{
	xmms_service_client_t *client = (xmms_service_client_t *) v;
	xmms_service_registry_t *registry = (xmms_service_registry_t *) d;
	xmmsv_t *fd;
	xmmsv_t *cookie;

	fd = xmmsv_new_uint ((uint32_t) client->fd);
	cookie = xmmsv_new_uint ((uint32_t) client->cookie);

	xmms_object_emit_f (XMMS_OBJECT (registry),
	                    XMMS_IPC_SIGNAL_SERVICE,
	                    XMMSV_TYPE_UINT32, fd,
	                    XMMSV_TYPE_UINT32, cookie);

	return FALSE;
}

/**
 * Free all resources in an xmms_service_client_t.
 */
static void
xmms_service_querying_client_free (gpointer value)
{
	xmms_service_client_t *client = (xmms_service_client_t *) value;

	g_return_if_fail (client);

	if (client->method) {
		g_free (client->method);
	}

	g_free (client);
}

/**
 * Unregister an existing service
 */
static void
xmms_service_unregister (xmms_service_registry_t *registry, xmms_ipc_msg_t *msg,
                         xmms_socket_t client, xmms_error_t *err)
{
	gchar *name = NULL;
	xmms_service_entry_t *entry;

	if (!msg) {
		guint ret;
		g_mutex_lock (registry->mutex);
		ret = g_hash_table_foreach_remove (registry->services,
		                                   xmms_service_matchsc,
		                                   GUINT_TO_POINTER (client));
		g_mutex_unlock (registry->mutex);
		if (ret > 0) {
			XMMS_DBG ("Service client (%d) just vaporized!"
			          " Removed from registry.", client);
		}

		g_mutex_lock (registry->mutex);
		ret = g_hash_table_foreach_remove (registry->clients,
		                                   xmms_service_method_request_matchfd,
		                                   GUINT_TO_POINTER (client));
		g_mutex_unlock (registry->mutex);
		if (ret > 0) {
			XMMS_DBG ("Requests from client (%d) removed.", client);
		}
		return;
	}

	entry = xmms_service_get (registry, msg, &name, err);
	g_return_if_fail (entry);

	if (client != entry->sc) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_PERMISSION,
		                    "Permission to unregister denied.");
		free (name);
		return;
	}

	if (!xmms_service_method_unregister (registry, msg, entry, err)) {
		g_mutex_lock (registry->mutex);
		if (!g_hash_table_remove (registry->services, name)) {
			XMMS_SERVICE_ERROR (err, XMMS_ERROR_GENERIC,
			                    "Failed to remove service");
		}
		g_mutex_unlock (registry->mutex);
	}

	XMMS_DBG ("Service unregistered.");

	free (name);
}

static void
object_cmd_value_unref (gpointer v)
{
	xmms_object_cmd_value_t *val = v;

	g_return_if_fail (val);

	xmms_object_cmd_value_unref (val);
}

/**
 * List all available service ids.
 */
static void
xmms_service_list (xmms_service_registry_t *registry,
                   xmms_object_cmd_arg_t *arg)
{
	xmmsv_t *list;

	list = xmmsv_new_list ();
	g_return_if_fail (list);

	g_mutex_lock (registry->mutex);
	g_hash_table_foreach (registry->services,
						  xmms_service_key_insert, list);
	g_mutex_unlock (registry->mutex);

	arg->retval = list;
}

/**
 * Pass service method call to service client.
 *
 * Argument name "sc_id" is reserved, don't use it.
 */
static gboolean
xmms_service_request (xmms_service_registry_t *registry, xmms_ipc_msg_t *msg,
                      xmms_socket_t client, xmms_error_t *err)
{
	gchar *name = NULL;
	xmms_service_entry_t *entry;
	xmms_service_method_t *method;
	GTree *table = NULL;
	guint next;
	xmms_service_client_t *cli = NULL;
	xmms_object_cmd_arg_t arg;

	g_return_val_if_fail (msg, FALSE);

	if (!(entry = xmms_service_get (registry, msg, &name, err))) {
		return FALSE;
	}

	free (name);

	if (!(method = xmms_service_method_get (msg, entry, &name, err))) {
		return FALSE;
	}

	XMMS_DBG ("Requesting method (%s) from client (%d)", name, client);

	free (name);

	g_mutex_lock (method->mutex);
	if (!(table = xmms_service_args_parse (msg, method->args, err))) {
		g_mutex_unlock (method->mutex);
		return FALSE;
	}
	g_mutex_unlock (method->mutex);
	next = xmms_service_next_id ();
	g_hash_table_insert (table, strdup ("sc_id"),
	                     xmms_object_cmd_value_uint_new (next));

	cli = g_new0 (xmms_service_client_t, 1);
	cli->fd = client;
	cli->cookie = xmms_ipc_msg_get_cookie (msg);
	cli->method = method;
	g_mutex_lock (registry->mutex);
	g_hash_table_insert (registry->clients, GUINT_TO_POINTER (next), cli);
	g_mutex_unlock (registry->mutex);

	xmms_object_cmd_arg_init (&arg);
	arg.retval = xmms_object_cmd_value_dict_new (table);
	arg.values[0].type = XMMS_OBJECT_CMD_ARG_UINT32;
	arg.values[0].value.uint32 = entry->sc;
	arg.values[1].type = XMMS_OBJECT_CMD_ARG_UINT32;
	arg.values[1].value.uint32 = method->cookie;
	xmms_object_emit (XMMS_OBJECT (registry),
	                  XMMS_IPC_SIGNAL_SERVICE,
	                  &arg);

	object_cmd_value_unref (arg.retval);

	return TRUE;
}

/**
 * Pass service method return to client.
 */
static void
xmms_service_return (xmms_service_registry_t *registry, xmms_ipc_msg_t *msg,
                     xmms_error_t *err)
{
	guint id;
	xmms_service_client_t *cli = NULL;
	GTree *table = NULL;
	xmms_object_cmd_arg_t arg;
	gchar *error = NULL;

	g_return_if_fail (msg);

	if (!xmms_ipc_msg_get_uint32 (msg, &id)) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "No service cookie given");
		return;
	}

	g_mutex_lock (registry->mutex);
	if (!(cli = g_hash_table_lookup (registry->clients,
	                                 GUINT_TO_POINTER (id)))) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_INVAL, "Invalid client id");
		g_mutex_unlock (registry->mutex);
		return;
	}
	g_mutex_unlock (registry->mutex);

	XMMS_DBG ("Returning method call to client (%d)", cli->fd);

	xmms_object_cmd_arg_init (&arg);
	arg.values[0].type = XMMS_OBJECT_CMD_ARG_UINT32;
	arg.values[0].value.uint32 = cli->fd;
	arg.values[1].type = XMMS_OBJECT_CMD_ARG_UINT32;
	arg.values[1].value.uint32 = cli->cookie;

	if (xmms_service_args_is_error (msg) &&
	    xmms_service_args_error_parse (msg, &error, err)) {
		xmms_error_set (&arg.error, XMMS_ERROR_GENERIC, error);
		free (error);
	} else {
		if (!(table = xmms_service_args_parse (msg, cli->method->rets, err))) {
			return;
		}
		arg.retval = xmms_object_cmd_value_dict_new (table);
	}

	xmms_object_emit (XMMS_OBJECT (registry),
	                  XMMS_IPC_SIGNAL_SERVICE,
	                  &arg);

	if (arg.retval) {
		object_cmd_value_unref (arg.retval);
	}
}

/**
 * Send shutdown broadcast to service client.
 */
static void
xmms_service_shutdown (xmms_service_registry_t *registry, xmms_ipc_msg_t *msg,
                       xmms_error_t *err)
{
	gchar *name = NULL;
	xmms_service_entry_t *entry;
	xmms_object_cmd_arg_t arg;

	g_return_if_fail (msg);

	if (!(entry = xmms_service_get (registry, msg, &name, err))) {
		return;
	}

	free (name);

	XMMS_DBG ("Shutdown request to (%d)", entry->sc);

	xmms_object_cmd_arg_init (&arg);
	arg.retval = xmms_object_cmd_value_none_new ();
	arg.values[0].type = XMMS_OBJECT_CMD_ARG_UINT32;
	arg.values[0].value.uint32 = entry->sc;
	arg.values[1].type = XMMS_OBJECT_CMD_ARG_UINT32;
	arg.values[1].value.uint32 = 0;
	xmms_object_emit (XMMS_OBJECT (registry),
	                  XMMS_IPC_SIGNAL_SERVICE_SHUTDOWN,
	                  &arg);
}

static xmms_service_entry_t *
xmms_service_is_registered (xmms_service_registry_t *registry,
                            const gchar *name)
{
	xmms_service_entry_t *ret;

	if (!name) {
		return NULL;
	}

	g_mutex_lock (registry->mutex);
	ret = g_hash_table_lookup (registry->services, name);
	g_mutex_unlock (registry->mutex);

	return ret;
}

static xmms_service_method_t *
xmms_service_method_is_registered (xmms_service_entry_t *entry,
                                   const gchar *name)
{
	xmms_service_method_t *ret;

	g_return_val_if_fail (entry, NULL);
	if (!name) {
		return NULL;
	}

	g_mutex_lock (entry->mutex);
	ret = g_hash_table_lookup (entry->methods, name);
	g_mutex_unlock (entry->mutex);

	return ret;
}

/**
 * Return true if the service is provided by the given service client data.
 */
static gboolean
xmms_service_matchsc (gpointer key, gpointer value, gpointer data)
{
	xmms_service_entry_t *v = value;
	guint sc = GPOINTER_TO_UINT (data);

	return (v->sc == sc);
}

/**
 * Remove reqeust from a client.
 */
static gboolean
xmms_service_method_request_matchfd (gpointer key, gpointer value, gpointer data)
{
	xmms_service_client_t *cli = value;
	xmms_socket_t fd = GPOINTER_TO_UINT (data);

	if (cli->fd == fd) {
		return TRUE;
	}

	return FALSE;
}

/**
 * Remove request to a method.
 */
static gboolean
xmms_service_method_request_matchmethod (gpointer key, gpointer value,
                                         gpointer data)
{
	xmms_service_client_t *cli = value;
	xmms_service_method_t *method = data;

	if (cli->method == method) {
		return TRUE;
	}

	return FALSE;
}

/**
 * Insert keys into a list
 */
static void
xmms_service_key_insert (gpointer key, gpointer value, gpointer data)
{
	xmmsv_t *name;
	xmmsv_t *list = (xmmsv_t *) data;

	name = xmmsv_new_string ((const char *) key);
	if (!name || !xmmsv_list_append (list, name)) {
		xmms_log_error ("Failed to add \"%s\" to service list.",
		                (const char *) key);
	}
	if (name) {
		xmmsv_unref (name);
	}
}

/**
 * Increase the universal service call counter
 */
static inline guint
xmms_service_next_id (void)
{
	static guint counter = 0;

	return counter++;
}

/**
 * Get the service with the given name
 */
static xmms_service_entry_t *
xmms_service_get (xmms_service_registry_t *registry, xmms_ipc_msg_t *msg,
                  gchar **name, xmms_error_t *err)
{
	guint len;
	xmms_service_entry_t *entry = NULL;

	g_return_val_if_fail (msg, NULL);
	g_return_val_if_fail (name, NULL);

	*name = NULL;

	if (!xmms_ipc_msg_get_string_alloc (msg, name, &len)) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "No service id given");
		free (*name);
	}

	if (!(entry = xmms_service_is_registered (registry, *name))) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_INVAL, "Invalid service id");
		free (*name);
	}

	return entry;
}

/**
 * Parse method signature
 */
static GTree *
xmms_service_arg_types_parse (xmms_ipc_msg_t *msg, xmms_error_t *err)
{
	xmms_service_argument_t *arg = NULL;
	guint len;
	guint num;
	GTree *table = NULL;

	g_return_val_if_fail (msg, NULL);

	table = g_hash_table_new_full (g_str_hash, g_str_equal, free, g_free);

	if (!xmms_ipc_msg_get_uint32 (msg, &num)) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT,
		                    "No number of arguments given");
		return NULL;
	}

	for (; num > 0; num--) {
		arg = g_new0 (xmms_service_argument_t, 1);

		if (!xmms_ipc_msg_get_string_alloc (msg, &arg->name, &len)) {
			XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "No argument name given");
			free (arg->name);
			g_free (arg);
			g_hash_table_destroy (table);
			return NULL;
		}
		if (!xmms_ipc_msg_get_uint32 (msg, &arg->type)) {
			XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "No argument type given");
			free (arg->name);
			g_free (arg);
			g_hash_table_destroy (table);
			return NULL;
		}
		if (!xmms_ipc_msg_get_int32 (msg, &arg->optional)) {
			XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT,
			                    "Optional field missing for argument");
			free (arg->name);
			g_free (arg);
			g_hash_table_destroy (table);
			return NULL;
		}

		g_hash_table_insert (table, arg->name, arg);
	}

	return table;
}

/**
 * Parse arguments
 */
static GTree *
xmms_service_args_parse (xmms_ipc_msg_t *msg, GTree *args, xmms_error_t *err)
{
	gchar *name = NULL;
	guint len, none;
	xmms_service_argument_t *arg = NULL;
	guint i;
	xmms_object_cmd_value_t *val = NULL;
	GTree *table = NULL;

	g_return_val_if_fail (msg, NULL);

	table = g_hash_table_new_full (g_str_hash, g_str_equal, free,
	                               object_cmd_value_unref);

	for (i = g_hash_table_size (args); i > 0; i--) {
		if (!xmms_ipc_msg_get_string_alloc (msg, &name, &len)) {
			XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "No argument name given");
			free (name);
			g_hash_table_destroy (table);
			return NULL;
		}
		if (!(arg = g_hash_table_lookup (args, name))) {
			XMMS_SERVICE_ERROR (err, XMMS_ERROR_INVAL, "Invalid argument");
			free (name);
			g_hash_table_destroy (table);
			return NULL;
		}
		if (xmms_ipc_msg_get_uint32 (msg, &none) && none) {
			if (!arg->optional) {
				XMMS_SERVICE_ERROR (err, XMMS_ERROR_INVAL,
				                    "Required argument not given");
				free (name);
				g_hash_table_destroy (table);
				return NULL;
			}

			val = xmms_object_cmd_value_none_new ();
			g_hash_table_insert (table, name, val);
			continue;
		}
		switch (arg->type) {
		case XMMS_OBJECT_CMD_ARG_UINT32:
		{
			guint tmp;
			if (!xmms_ipc_msg_get_uint32 (msg, &tmp)) {
				goto inval;
			}
			val = xmms_object_cmd_value_uint_new (tmp);
		}
		break;
		case XMMS_OBJECT_CMD_ARG_INT32:
		{
			gint tmp;
			if (!xmms_ipc_msg_get_int32 (msg, &tmp)) {
				goto inval;
			}
			val = xmms_object_cmd_value_int_new (tmp);
		}
		break;
		case XMMS_OBJECT_CMD_ARG_STRING:
		{
			gchar *tmp = NULL;
			if (!xmms_ipc_msg_get_string_alloc (msg, &tmp, &len)) {
				free (tmp);
				goto inval;
			}
			val = xmms_object_cmd_value_str_new (tmp);
			free (tmp);
		}
		break;
		case XMMS_OBJECT_CMD_ARG_STRINGLIST:
		{
			GList *tmp = NULL;
			guint size, k;
			if (!xmms_ipc_msg_get_uint32 (msg, &size)) {
				goto inval;
			}
			for (k = 0; k < size; k++) {
				gchar *buf;
				if (!xmms_ipc_msg_get_string_alloc (msg, &buf, &len) ||
				    !(val = xmms_object_cmd_value_str_new (buf)) ||
				    !(tmp = g_list_prepend (tmp, val))) {
					object_cmd_value_unref (val);
					while (tmp) {
						g_free (tmp->data);
						tmp = g_list_delete_link (tmp, tmp);
					}
					goto inval;
				}
				free (buf);
			}
			tmp = g_list_reverse (tmp);
			val = xmms_object_cmd_value_list_new (tmp);
		}
		break;
		case XMMS_OBJECT_CMD_ARG_COLL:
		{
			xmmsc_coll_t *tmp = NULL;
			if (!xmms_ipc_msg_get_collection_alloc (msg, &tmp)) {
				xmmsc_coll_unref (tmp);
				goto inval;
			}
			val = xmms_object_cmd_value_coll_new (tmp);
		}
		break;
		case XMMS_OBJECT_CMD_ARG_BIN:
		{
			GString *tmp = g_string_new (NULL);
			if (!xmms_ipc_msg_get_bin_alloc (msg, (unsigned char **)&tmp->str,
			                                 (uint32_t *)&tmp->len)) {
				g_string_free (tmp, TRUE);
				goto inval;
			}
			val = xmms_object_cmd_value_bin_new (tmp);
		}
		break;
		default:
		inval:
			XMMS_SERVICE_ERROR (err, XMMS_ERROR_INVAL, "Invalid argument");
			free (name);
			g_hash_table_destroy (table);
			return NULL;
		}

		g_hash_table_insert (table, name, val);
	}

	return table;
}

/**
 * Test if a return value message is an error message.
 */
static gboolean
xmms_service_args_is_error (xmms_ipc_msg_t *msg)
{
	guint err;

	g_return_val_if_fail (msg, FALSE);

	if (xmms_ipc_msg_get_uint32 (msg, &err) && err == XMMS_IPC_CMD_ERROR) {
		return TRUE;
	}

	return FALSE;
}

/**
 * Create a new return message for XMMS_IPC_SIGNAL_SERVICE_#_CHANGED broadcasts.
 */
static xmmsv_t *
xmms_service_changed_msg_new (xmmsv_t *svc, xmms_service_changed_actions_t type)
{
	xmmsv_t *ret;
	xmmsv_t *val;

	ret = xmmsv_new_dict ();
	x_return_null_if_fail (ret);

	x_return_null_if_fail (xmmsv_dict_get (svc, XMMSC_SERVICE_PROP_NAME, &val));
	x_return_null_if_fail (xmmsv_dict_insert (ret, XMMSC_SERVICE_PROP_NAME,
	                                          val));
	xmmsv_unref (val);

	val = xmmsv_new_int ((int32_t) type);
	x_return_null_if_fail (val);
	x_return_null_if_fail (xmmsv_dict_insert (ret, XMMSC_SERVICE_CHANGE_TYPE,
	                                          val));
	xmmsv_unref (val);

	return ret;
}

static gint
uint_compare (gconstpointer a, gconstpointer b, gpointer data)
{
	guint x = GPOINTER_TO_UINT (a);
	guint y = GPOINTER_TO_UINT (b);

	return x == y ? 0 : (x > y ? 1 : -1);
}

/** @} */
