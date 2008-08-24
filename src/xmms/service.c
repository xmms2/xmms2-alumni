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
#include "xmmspriv/xmms_ipc_pending.h"
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

#define XMMSC_SERVICE_PENDING_PROP_SERVICE     "service"
#define XMMSC_SERVICE_PENDING_PROP_METHOD      "method"

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
};

/**
 * @internal
 *
 * A single service representation
 */
struct xmms_service_entry_St {
	/* The description of the service and all its methods. */
	xmmsv_t *description;

	/* Service client connection used for authentication. */
	xmms_ipc_client_t *sc;
};

/**
 * Functions
 */
static void xmms_service_register (xmms_service_registry_t *registry, xmmsv_t *description, xmms_ipc_client_t *client, xmms_error_t *err);
static void xmms_service_unregister (xmms_service_registry_t *registry, const gchar *name, xmms_ipc_client_t *client, xmms_error_t *err);
static GList *xmms_service_list (xmms_service_registry_t *registry, xmms_error_t *err);
static xmmsv_t *xmms_service_describe (xmms_service_registry_t *registry, const char *svc, xmms_error_t *err);
static xmms_ipc_pending_id_t xmms_service_query (xmms_service_registry_t *registry, const gchar *svc, const gchar *meth, xmmsv_t *args, xmms_error_t *err);
static void xmms_service_return (xmms_service_registry_t *registry, xmmsv_t *dict, xmms_ipc_client_t *client, xmms_error_t *err);

static void xmms_service_destroy (xmms_object_t *object);

static xmms_service_entry_t *xmms_service_entry_new (xmms_ipc_client_t *client, xmmsv_t *svc);
static void xmms_service_entry_free (gpointer data);

static gboolean xmms_service_key_insert (gpointer key, gpointer value, gpointer data);

static xmmsv_t *xmms_service_changed_msg_new (xmmsv_t *svc, xmms_service_changed_actions_t type);


static void dict_insert_string (xmmsv_t *dict, const gchar *key, const gchar *value);
static gboolean dict_get_string (xmmsv_t *dict, const gchar *key, const gchar **value);
static xmms_ipc_pending_id_t returndict_extract_pid (xmmsv_t *dict);
static xmmsv_t *returndict_extract_retval (xmmsv_t *dict);


/*
static void xmms_service_shutdown (xmms_service_t *xmms_service,
                                   xmms_ipc_msg_t *msg, xmms_error_t *err);
*/

XMMS_CMD_DEFINE (svc_register, xmms_service_register,
                 xmms_service_registry_t *, NONE,
                 DICT, CLIENT);
XMMS_CMD_DEFINE (svc_unregister, xmms_service_unregister,
                 xmms_service_registry_t *, NONE,
                 STRING, CLIENT);
XMMS_CMD_DEFINE (svc_list, xmms_service_list,
                 xmms_service_registry_t *, LIST,
                 NONE, NONE);
XMMS_CMD_DEFINE (svc_describe, xmms_service_describe,
                 xmms_service_registry_t *, DICTVALUE,
                 STRING, NONE);
XMMS_CMD_DEFINE3(svc_query, xmms_service_query,
                 xmms_service_registry_t *, PENDING,
                 STRING, STRING, LIST);
XMMS_CMD_DEFINE (svc_return, xmms_service_return,
                 xmms_service_registry_t *, NONE,
                 DICT, CLIENT);

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

	/* Initialize registry fields */
	ret->mutex = g_mutex_new ();
	ret->services = g_tree_new_full ((GCompareDataFunc) strcmp, NULL,
	                                 g_free, xmms_service_entry_free);

	if (!ret->mutex) {
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
	xmms_object_cmd_add (XMMS_OBJECT (ret), XMMS_IPC_CMD_SERVICE_DESCRIBE,
	                     XMMS_CMD_FUNC (svc_describe));
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
                       xmms_ipc_client_t *client, xmms_error_t *err)
{
	const gchar *name = NULL;
	xmms_service_entry_t *entry = NULL;
	xmmsv_t *val;
	xmmsv_t *ret;
	xmms_object_cmd_arg_t arg;

	/* FIXME: check description */

	entry = xmms_service_entry_new (client, description);
	if (!entry) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NO_SAUSAGE,
		                    "Failed to create new service entry.");
		goto err;
	}

	if (!xmmsv_dict_get (description, XMMSC_SERVICE_PROP_NAME, &val) ||
	    !xmmsv_get_string (val, &name)) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NO_SAUSAGE,
		                    "Failed to extract service name.");
		goto err;
	}

	g_mutex_lock (registry->mutex);
	if (g_tree_lookup (registry->services, name)) {
		g_mutex_unlock (registry->mutex);
		xmms_error_set (err, XMMS_ERROR_INVAL, "Service already registered!");
		XMMS_DBG ("Service (%s) already registered!", name);
		goto err;
	}
	g_tree_insert (registry->services, g_strdup (name), entry);
	g_mutex_unlock (registry->mutex);
/* FIXME: commented out broadcasts for now
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
*/
	XMMS_DBG ("New service registered: %s", name);

	return;

err:
	if (entry) {
		xmms_service_entry_free (entry);
	}

	return;
}

static xmms_service_entry_t *
xmms_service_entry_new (xmms_ipc_client_t *client, xmmsv_t *svc)
{
	xmms_service_entry_t *entry;

	entry = g_new0 (xmms_service_entry_t, 1);
	g_return_val_if_fail (entry, NULL);

	entry->description = xmmsv_ref (svc);
	entry->sc = client;

	return entry;
}

/**
 * Free all the memory used by xmms_service_registry_t
 */
static void
xmms_service_destroy (xmms_object_t *object)
{
	xmms_service_registry_t *registry = (xmms_service_registry_t *)object;

	g_return_if_fail (registry);

	g_mutex_free (registry->mutex);
	g_tree_destroy (registry->services);

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

	g_free (entry);
}

/**
 * Unregister all services
 */
void
xmms_service_unregister_all (xmms_object_t *obj)
{
	xmms_service_registry_t *registry = (xmms_service_registry_t *) obj;

	g_return_if_fail (registry);

	g_tree_destroy (registry->services);
	registry->services = g_tree_new_full ((GCompareDataFunc) strcmp, NULL,
	                                      g_free, xmms_service_entry_free);
	/* FIXME: nicer way to empty? */
}

/**
 * Unregister an existing service
 */
static void
xmms_service_unregister (xmms_service_registry_t *registry, const gchar *name,
                         xmms_ipc_client_t *client, xmms_error_t *err)
{
	xmms_service_entry_t *entry;

	g_return_if_fail (name);

	g_mutex_lock (registry->mutex);
	entry = (xmms_service_entry_t *) g_tree_lookup (registry->services,
	                                                (gconstpointer) name);
	if (!entry) {
		g_mutex_unlock (registry->mutex);
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "Failed to unregister "
		                    "service: could not look up service.");
		return;
	}

	if (client != entry->sc) {
		g_mutex_unlock (registry->mutex);
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_PERMISSION, "Failed to unregister "
		                    "service: permission denied.");
		return;
	}

	if (!g_tree_remove (registry->services, (gconstpointer) name)) {
		g_mutex_unlock (registry->mutex);
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "Failed to unregister "
		                    "service: could not remove service.");
		return;
	}
	g_mutex_unlock (registry->mutex);

	XMMS_DBG ("Service '%s' unregistered.", name);
}

/**
 * List all available service ids.
 */
static GList *
xmms_service_list (xmms_service_registry_t *registry, xmms_error_t *err)
{
	GList *list = NULL;

	g_mutex_lock (registry->mutex);
	g_tree_foreach (registry->services, xmms_service_key_insert,
	                (gpointer) &list);
	g_mutex_unlock (registry->mutex);

	list = g_list_reverse (list);

	return list;
}

/**
 * Return the description and signature of a service method.
 */
static xmmsv_t *
xmms_service_describe (xmms_service_registry_t *registry, const char *svc,
                       xmms_error_t *err)
{
	xmms_service_entry_t *entry;
	xmmsv_t *val;
	g_mutex_lock (registry->mutex);

	entry = (xmms_service_entry_t *) g_tree_lookup (registry->services, svc);
	if (!entry) {
		g_mutex_unlock (registry->mutex);
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "Could not find service.");
		return xmmsv_new_none ();
	}

	val = xmmsv_ref (entry->description); /* ref it so it's not freed by ipc */
	g_mutex_unlock (registry->mutex);

	return val;
}


static GTree *
build_service_request_dict (guint reqid, const gchar *method, xmmsv_t *args)
{
	GTree *reqdict;
	xmmsv_t *tmp;

	reqdict = g_tree_new_full ((GCompareDataFunc) strcmp, NULL,
	                           NULL, (GDestroyNotify) xmmsv_unref);

	xmmsv_ref (args);
	g_tree_insert (reqdict, (gpointer) XMMSC_SERVICE_QUERY_PROP_ARGUMENTS,
	               args);

	tmp = xmmsv_new_string (method);
	g_tree_insert (reqdict, (gpointer) XMMSC_SERVICE_QUERY_PROP_METHOD, tmp);

	tmp = xmmsv_new_uint (reqid);
	g_tree_insert (reqdict, (gpointer) XMMSC_SERVICE_QUERY_PROP_ID, tmp);

	return reqdict;
}

/**
 * Pass service method call to service client.
 */
static xmms_ipc_pending_id_t
xmms_service_query (xmms_service_registry_t *registry, const gchar *svc,
                    const gchar *meth, xmmsv_t *args, xmms_error_t *err)
{
	xmmsv_t *pending_data;
	xmms_ipc_pending_id_t pid;
	GTree *request;
	xmms_service_entry_t *entry;

	XMMS_DBG ("Client requesting method (%s) from service (%s).", meth, svc);

	// Check service exists
	entry = (xmms_service_entry_t *) g_tree_lookup (registry->services, svc);
	if (!entry) {
		g_mutex_unlock (registry->mutex);
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "Could not find service.");
		return 0;
	}

	// FIXME: Check signature

	pending_data = xmmsv_new_dict ();
	dict_insert_string (pending_data, XMMSC_SERVICE_PENDING_PROP_SERVICE, svc);
	dict_insert_string (pending_data, XMMSC_SERVICE_PENDING_PROP_METHOD, meth);

	// Get a fresh id from the pending pool
	pid = xmms_ipc_pending_register (pending_data);
	xmmsv_unref (pending_data);

	// Prepare request structure to send to SC
	request = build_service_request_dict (pid, meth, args);

	// Forward query to SC
	// FIXME: This is BROKEN, it forwards requests to ALL service clients!
	xmms_object_emit_f (XMMS_OBJECT (registry),
	                    XMMS_IPC_SIGNAL_SERVICE,
	                    XMMSV_TYPE_DICT, request);

	// Command return type is PENDING
	return pid;
}


/**
 * Pass service method return to client.
 */
static void
xmms_service_return (xmms_service_registry_t *registry, xmmsv_t *dict,
                     xmms_ipc_client_t *client, xmms_error_t *err)
{
	xmms_ipc_pending_id_t pid;
	xmmsv_t *retval;
	xmmsv_t *pending_data;
	xmms_service_entry_t *entry;
	const gchar *queried_service, *queried_method;

	// Extract infos from the SC message
	pid = returndict_extract_pid (dict);
	if (!pid) { /* pid == 0 => error */
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "Could not get query id");
		goto err;
	}
	retval = returndict_extract_retval (dict);
	if (!retval) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "Could not get return value");
		goto err;
	}

	// Find back infos about the pending query
	pending_data = xmms_ipc_pending_get_data (pid);
	if (!dict_get_string (pending_data, XMMSC_SERVICE_PENDING_PROP_SERVICE, &queried_service) ||
	    !dict_get_string (pending_data, XMMSC_SERVICE_PENDING_PROP_METHOD, &queried_method)) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "Invalid recorded pending data");
		goto err;
	}

	// Verify origin of the reply
	g_mutex_lock (registry->mutex);
	entry = (xmms_service_entry_t *) g_tree_lookup (registry->services,
	                                                queried_service);
	if (!entry) {
		g_mutex_unlock (registry->mutex);
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT,
		                    "Failed to find queried service.");
		goto err;
	}
	g_mutex_unlock (registry->mutex);

	if (entry->sc != client) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_PERMISSION,
		                    "Reply received from a foreign service client!");
		goto err;
	}

	// FIXME: Check retval type

	XMMS_DBG ("Returning value from method (%s) of service (%s)",
	          queried_method, queried_service);

	// Finally send pending message to the original caller
	xmms_ipc_pending_send (pid, retval);

	return; // Return success (no error) to service client

err:
	// FIXME: we need to send some error back to the client!
	return;
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

	*list = g_list_prepend (*list, name);
	if (!*list) {
		xmmsv_unref (name);
		xmms_log_error ("Failed to add \"%s\" to service list.",
		                (const char *) key);
		/* Keep traversing anyway. Fill the list as much as possible. */
		return FALSE;
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

	x_return_null_if_fail (xmmsv_dict_get (svc, XMMSC_SERVICE_PROP_NAME, &val));
	x_return_null_if_fail (xmmsv_dict_insert (ret, XMMSC_SERVICE_PROP_NAME,
	                                          val));

	val = xmmsv_new_int ((int32_t) type);
	x_return_null_if_fail (val);
	x_return_null_if_fail (xmmsv_dict_insert (ret, XMMSC_SERVICE_CHANGE_TYPE,
	                                          val));
	xmmsv_unref (val);

	return ret;
}

static xmms_ipc_pending_id_t
returndict_extract_pid (xmmsv_t *dict)
{
	xmmsv_t *tmp;
	xmms_ipc_pending_id_t pid;

	if (!xmmsv_dict_get (dict, XMMSC_SERVICE_QUERY_PROP_ID, &tmp) ||
	    !xmmsv_get_uint (tmp, &pid)) {
		return 0;
	}

	return pid;
}

static xmmsv_t *
returndict_extract_retval (xmmsv_t *dict)
{
	xmmsv_t *tmp;

	if (!xmmsv_dict_get (dict, XMMSC_SERVICE_QUERY_PROP_RETURN, &tmp)) {
		return NULL;
	}

	return tmp;
}

static void
dict_insert_string (xmmsv_t *dict, const gchar *key, const gchar *value)
{
	xmmsv_t *v;
	v = xmmsv_new_string (value);
	xmmsv_dict_insert (dict, key, v);
	xmmsv_unref (v);
}

static gboolean
dict_get_string (xmmsv_t *dict, const gchar *key, const gchar **value)
{
	xmmsv_t *v;
	if (!xmmsv_dict_get (dict, key, &v) || !xmmsv_get_string (v, value)) {
		return false;
	}

	return true;
}

/** @} */
