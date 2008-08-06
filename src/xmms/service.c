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

	/* A cookie to match request to response. */
	guint32 cookie;

	/* Service client fd to respond to the right client. */
	xmms_socket_t sc;
};

/**
 * @internal
 *
 * Querying client representation
 */
typedef struct xmms_service_client_St {
	/* Querying client fd to respond to the right client. */
	xmms_socket_t fd;

	/* A cookie to match request to response. */
	guint32 cookie;

	/* The name of the method being queried. */
	gchar *method;
} xmms_service_client_t;

/**
 * Functions
 */
static void xmms_service_register (xmms_service_registry_t *registry,
                                   xmmsv_t *description, int32_t fd,
                                   uint32_t cookie, xmms_error_t *err);
static void xmms_service_unregister (xmms_service_registry_t *registry,
                                     const gchar *name, int32_t fd,
                                     uint32_t cookie, xmms_error_t *err);
static void xmms_service_destroy (xmms_object_t *object);

static xmms_service_entry_t *xmms_service_entry_new (xmms_socket_t client,
                                                     uint32_t cookie,
                                                     xmmsv_t *svc);
static gboolean xmms_service_querying_client_unregister (gpointer k,
                                                         gpointer v,
                                                         gpointer d);
static void xmms_service_entry_free (gpointer data);

static GList *xmms_service_list (xmms_service_registry_t *registry, int32_t fd,
                                 uint32_t cookie, xmms_error_t *err);
static gboolean xmms_service_key_insert (gpointer key, gpointer value,
                                         gpointer data);

static gint xmms_service_query (xmms_service_registry_t *registry,
                                const gchar *svc, const gchar *meth,
                                xmmsv_t *args, int32_t fd, uint32_t cookie,
                                xmms_error_t *err);
static void xmms_service_return (xmms_service_registry_t *registry, uint32_t id,
                                 xmmsv_t *list, int32_t fd, uint32_t cookie,
                                 xmms_error_t *err);

static xmmsv_t *
xmms_service_changed_msg_new (xmmsv_t *svc,
                              xmms_service_changed_actions_t type);

static void xmms_service_querying_client_free (gpointer value);
static gint uint_compare (gconstpointer a, gconstpointer b, gpointer data);
static inline guint xmms_service_next_id (void);

/*
static void xmms_service_shutdown (xmms_service_t *xmms_service,
                                   xmms_ipc_msg_t *msg, xmms_error_t *err);
*/

XMMS_SVC_CMD_DEFINE (svc_register, xmms_service_register,
                     xmms_service_registry_t *, NONE,
                     DICT, NONE, NONE, NONE);
XMMS_SVC_CMD_DEFINE (svc_unregister, xmms_service_unregister,
                     xmms_service_registry_t *, NONE,
                     STRING, NONE, NONE, NONE);
XMMS_SVC_CMD_DEFINE (svc_list, xmms_service_list,
                     xmms_service_registry_t *, LIST,
                     NONE, NONE, NONE, NONE);
XMMS_SVC_CMD_DEFINE (svc_query, xmms_service_query,
                     xmms_service_registry_t *, INT32,
                     STRING, STRING, DICT, NONE);
XMMS_SVC_CMD_DEFINE (svc_return, xmms_service_return,
                     xmms_service_registry_t *, NONE,
                     INT32, LIST, NONE, NONE);

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
	xmms_object_cmd_add (XMMS_OBJECT (ret), XMMS_IPC_CMD_SERVICE_UNREGISTER,
	                     XMMS_CMD_FUNC (svc_unregister));
	xmms_object_cmd_add (XMMS_OBJECT (ret), XMMS_IPC_CMD_SERVICE_LIST,
	                     XMMS_CMD_FUNC (svc_list));
	xmms_object_cmd_add (XMMS_OBJECT (ret), XMMS_IPC_CMD_SERVICE_QUERY,
	                     XMMS_CMD_FUNC (svc_query));
	xmms_object_cmd_add (XMMS_OBJECT (ret), XMMS_IPC_CMD_SERVICE_RETURN,
	                     XMMS_CMD_FUNC (svc_return));

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
 * Register a new service
 */
static void
xmms_service_register (xmms_service_registry_t *registry, xmmsv_t *description,
                       int32_t fd, uint32_t cookie, xmms_error_t *err)
{
	const gchar *name = NULL;
	gchar *key = NULL;;
	xmms_service_entry_t *entry = NULL;
	xmmsv_t *val;
	xmmsv_t *ret;
	xmms_object_cmd_arg_t arg;

	entry = xmms_service_entry_new ((xmms_socket_t) fd, cookie, description);

	if (!entry) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NO_SAUSAGE,
		                    "Failed to create new service entry.");
		goto err;
	}

	if (!xmmsv_dict_get (description, XMMSC_SERVICE_PROP_NAME, &val)) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NO_SAUSAGE,
		                    "Failed to extract service name.");
		goto err;
	}
	if (!xmmsv_get_string (val, &name) || !name) {
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

	g_mutex_lock (registry->mutex);
	if (g_tree_lookup (registry->services, (gconstpointer) key)) {
		g_mutex_unlock (registry->mutex);
		xmms_error_set (err, XMMS_ERROR_INVAL, "Service already registered!");
		XMMS_DBG ("Service (%s) already registered!", key);
		g_free (key);
		goto err;
	}
	g_tree_insert (registry->services, (gpointer) key, (gpointer) entry);
	g_mutex_unlock (registry->mutex);

	ret = xmms_service_changed_msg_new (entry->description,
	                                    XMMS_SERVICE_CHANGED_REGISTER);
	if (!ret) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT,
		                    "Failed to set broadcast message.");
		goto err;
	}

	xmms_object_cmd_arg_init (&arg);
	arg.retval = ret;
	xmms_object_emit (XMMS_OBJECT (registry), XMMS_IPC_SIGNAL_SERVICE_CHANGED,
	                  &arg);
	xmmsv_unref (ret);

	XMMS_DBG ("New service registered: %s", key);

	return;

err:
	if (entry) {
		xmms_service_entry_free ((gpointer) entry);
	}

	return;
}

static xmms_service_entry_t *
xmms_service_entry_new (xmms_socket_t client, uint32_t cookie, xmmsv_t *svc)
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
/* FIXME: Unregister dead service clients. Probably need to hack ipc.c. :( */
static void
xmms_service_unregister (xmms_service_registry_t *registry, const gchar *name,
                         int32_t fd, uint32_t cookie, xmms_error_t *err)
{
	xmms_service_entry_t *entry;

	g_mutex_lock (registry->mutex);
	entry = (xmms_service_entry_t *) g_tree_lookup (registry->services,
	                                                (gconstpointer) name);
	g_mutex_unlock (registry->mutex);
	if (!entry) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "Failed to unregister "
		                    "service: could not look up service.");
		return;
	}

	g_mutex_lock (entry->mutex);
	if ((xmms_socket_t) fd != entry->sc) {
		g_mutex_unlock (entry->mutex);
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_PERMISSION, "Failed to unregister "
		                    "service: permission denied.");
		return;
	}
	g_mutex_unlock (entry->mutex);

	g_mutex_lock (registry->mutex);
	if (!g_tree_remove (registry->services, (gconstpointer) name)) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "Failed to unregister "
		                    "service: could not look up service.");
		return;
	}
	g_mutex_unlock (registry->mutex);

	XMMS_DBG ("Service unregistered.");
}

/**
 * List all available service ids.
 */
static GList *
xmms_service_list (xmms_service_registry_t *registry, int32_t fd,
                   uint32_t cookie, xmms_error_t *err)
{
	GList *list = NULL;

	g_mutex_lock (registry->mutex);
	g_tree_foreach (registry->services, xmms_service_key_insert,
	                (gpointer) &list);
	g_mutex_unlock (registry->mutex);

	return list;
}

/**
 * Pass service method call to service client.
 */
static gint
xmms_service_query (xmms_service_registry_t *registry, const gchar *svc,
                    const gchar *meth, xmmsv_t *args, int32_t fd,
                    uint32_t cookie, xmms_error_t *err)
{
	xmms_service_entry_t *entry;
	xmms_service_client_t *cli = NULL;
	gchar *name;

	XMMS_DBG ("Client (%d) requesting method (%s) from service (%s).",
	          fd, meth, svc);

	cli = g_new0 (xmms_service_client_t, 1);
	if (!cli) {
		goto err;
	}
	cli->fd = (xmms_socket_t) fd;
	cli->cookie = cookie;
	cli->method = strdup (meth);
	if (!cli->method) {
		goto err;
	}

	g_mutex_lock (registry->mutex);
	g_tree_insert (registry->clients,
	               GUINT_TO_POINTER (xmms_service_next_id ()), (gpointer) cli);
	g_mutex_unlock (registry->mutex);

	name = strdup (meth);
	if (!name) {
		goto err;
	}
	xmms_object_emit_f (XMMS_OBJECT (registry),
	                    XMMS_IPC_SIGNAL_SERVICE,
	                    XMMSV_TYPE_INT32, (int32_t) entry->sc,
	                    XMMSV_TYPE_UINT32, entry->cookie,
	                    XMMSV_TYPE_STRING, name);
	g_free (name);

	return 1;

err:
	if (cli) {
		xmms_service_querying_client_free ((gpointer) cli);
	}

	return 0;
}

/**
 * Pass service method return to client.
 */
static void
xmms_service_return (xmms_service_registry_t *registry, uint32_t id,
                     xmmsv_t *list, int32_t fd, uint32_t cookie,
                     xmms_error_t *err)
{
	xmms_service_client_t *cli = NULL;
	xmmsv_t *ret;

	g_mutex_lock (registry->mutex);
	cli = g_tree_lookup (registry->clients, GUINT_TO_POINTER (id));
	if (!cli) {
		g_mutex_unlock (registry->mutex);
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_INVAL, "Invalid client id");
		return;
	}
	g_mutex_unlock (registry->mutex);

	XMMS_DBG ("Returning method (%s) call to client (%d)",
	          cli->method, cli->fd);

	/* HACK: We use the list as a container for an xmmsv_t of any type. */
	if (!xmmsv_list_get (list, 0, &ret)) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT,
		                    "Could not get return value");
		return;
	}

	xmms_object_emit_f (XMMS_OBJECT (registry),
	                    XMMS_IPC_SIGNAL_SERVICE,
	                    XMMSV_TYPE_INT32, (int32_t) cli->fd,
	                    XMMSV_TYPE_UINT32, cli->cookie);
}

/**
 * Insert keys into a list
 */
static gboolean
xmms_service_key_insert (gpointer key, gpointer value, gpointer data)
{
	GList **list = (GList **) data;
	xmmsv_t *name = NULL;

	name = xmmsv_new_string ((const char *) key);
	if (!name) {
		xmms_log_error ("Failed to copy \"%s\" to service list.",
		                (const char *) key);
		/* Keep traversing anyway. Fill the list as much as possible. */
		return FALSE;
	}

	*list = g_list_append (*list, (gpointer) name);
	if (!*list) {
		xmms_log_error ("Failed to add \"%s\" to service list.",
		                (const char *) key);
		/* Keep traversing anyway. Fill the list as much as possible. */
		return FALSE;
	}

	return FALSE;
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
