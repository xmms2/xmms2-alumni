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
#include "xmmsc/xmmsc_service.h"
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

	/* Service client msg cookie used to send back queries. */
	xmms_ipc_cookie_t cookie;
};

/**
 * Functions
 */
static void xmms_service_register (xmms_service_registry_t *registry, xmmsv_t *description, xmms_ipc_client_t *client, xmms_ipc_cookie_t cookie, xmms_error_t *err);
static void xmms_service_unregister (xmms_service_registry_t *registry, const gchar *name, xmms_ipc_client_t *client, xmms_error_t *err);
static GList *xmms_service_list (xmms_service_registry_t *registry, xmms_error_t *err);
static xmmsv_t *xmms_service_describe (xmms_service_registry_t *registry, const char *svc, xmms_error_t *err);
static void xmms_service_query (xmms_service_registry_t *registry, const gchar *svc, const gchar *meth, xmmsv_t *args, xmms_ipc_client_t *client, xmms_ipc_cookie_t cookie, xmms_error_t *err);
static void xmms_service_return (xmms_service_registry_t *registry, xmmsv_t *dict, xmms_ipc_client_t *client, xmms_error_t *err);

static void xmms_service_destroy (xmms_object_t *object);

static xmms_service_entry_t *xmms_service_entry_new (xmms_ipc_client_t *client, xmms_ipc_cookie_t cookie, xmmsv_t *svc);
static void xmms_service_entry_free (gpointer data);

static gboolean xmms_service_key_insert (gpointer key, gpointer value, gpointer data);

static xmmsv_t *xmms_service_changed_msg_new (xmmsv_t *svc, xmms_service_changed_actions_t type);

static xmms_service_entry_t *registry_lookup_service (xmms_service_registry_t *registry, const gchar *name);
static gboolean check_args_against_signature (xmmsv_t *args, xmmsv_t *meth_desc, xmms_error_t *err);

static int xmms_service_validate (xmmsv_t *desc, gchar **errmsg);

static void dict_insert_string (xmmsv_t *dict, const gchar *key, const gchar *value);
static gboolean dict_get_string (xmmsv_t *dict, const gchar *key, const gchar **value);
static GTree *build_service_request_dict (guint reqid, const gchar *method, xmmsv_t *args);
static xmmsv_t *build_pending_data_dict (const gchar *svc, const gchar *meth);
static gboolean extract_return_dict (xmmsv_t *dict, xmms_ipc_pending_id_t *pid, xmmsv_t **retval);
static gboolean extract_pending_data_dict (xmmsv_t *dict, const gchar **svc, const gchar **meth);


XMMS_CMD_DEFINE3(svc_register, xmms_service_register,
                 xmms_service_registry_t *, NONE,
                 DICT, CLIENT, COOKIE);
XMMS_CMD_DEFINE (svc_unregister, xmms_service_unregister,
                 xmms_service_registry_t *, NONE,
                 STRING, CLIENT);
XMMS_CMD_DEFINE (svc_list, xmms_service_list,
                 xmms_service_registry_t *, LIST,
                 NONE, NONE);
XMMS_CMD_DEFINE (svc_describe, xmms_service_describe,
                 xmms_service_registry_t *, DICTVALUE,
                 STRING, NONE);
XMMS_CMD_DEFINE5(svc_query, xmms_service_query,
                 xmms_service_registry_t *, NOREPLY,
                 STRING, STRING, LIST, CLIENT, COOKIE);
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
 * Free all the memory used by xmms_service_registry_t
 */
static void
xmms_service_destroy (xmms_object_t *object)
{
	xmms_service_registry_t *registry = (xmms_service_registry_t *)object;

	g_return_if_fail (registry);

	g_mutex_free (registry->mutex);
	g_tree_destroy (registry->services);

	xmms_ipc_broadcast_unregister (XMMS_IPC_SIGNAL_SERVICE_CHANGED);
	xmms_ipc_broadcast_unregister (XMMS_IPC_SIGNAL_SERVICE_SHUTDOWN);
	xmms_ipc_object_unregister (XMMS_IPC_OBJECT_SERVICE);

	XMMS_DBG ("Service object shutdown.");
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

static xmms_service_entry_t *
xmms_service_entry_new (xmms_ipc_client_t *client, xmms_ipc_cookie_t cookie,
                        xmmsv_t *svc)
{
	xmms_service_entry_t *entry;

	entry = g_new0 (xmms_service_entry_t, 1);
	g_return_val_if_fail (entry, NULL);

	entry->description = xmmsv_ref (svc);
	entry->sc = client;
	entry->cookie = cookie;

	return entry;
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
 * Register a new service
 */
static void
xmms_service_register (xmms_service_registry_t *registry, xmmsv_t *description,
                       xmms_ipc_client_t *client, xmms_ipc_cookie_t cookie,
                       xmms_error_t *err)
{
	const gchar *name = NULL;
	gchar *validerr = NULL;
	xmms_service_entry_t *entry = NULL;
	xmmsv_t *val;
	xmmsv_t *ret;
	xmms_object_cmd_arg_t arg;

	XMMS_DBG ("Registering new service.");

	/* Check service description structure */
	if (!xmms_service_validate (description, &validerr)) {
		gchar *errmsg;
		errmsg = g_strconcat ("Invalid service description: ", validerr, NULL);
		xmms_error_set (err, XMMS_ERROR_INVAL, errmsg);
		XMMS_DBG ("%s", errmsg);
		g_free (errmsg);
		goto err;
	}

	entry = xmms_service_entry_new (client, cookie, description);
	if (!entry) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NO_SAUSAGE,
		                    "Failed to create new service entry.");
		goto err;
	}

	xmmsv_service_get_name (description, &name);

	g_mutex_lock (registry->mutex);
	if (registry_lookup_service (registry, name)) {
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

	g_free (validerr);

	return;
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
	g_return_if_fail (client);

	XMMS_DBG ("Unregistering service '%s'.", name);

	g_mutex_lock (registry->mutex);
	if (!(entry = registry_lookup_service (registry, name))) {
		g_mutex_unlock (registry->mutex);
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "Failed to unregister "
		                    "service: could not look up service.");
		return;
	}

	/* Only service client can unregister itself */
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

	/* FIXME: Say goodbye to all clients who were pending on a query */

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
	g_tree_foreach (registry->services, xmms_service_key_insert, &list);
	g_mutex_unlock (registry->mutex);

	return g_list_reverse (list);
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

	g_return_val_if_fail (svc, NULL);

	g_mutex_lock (registry->mutex);
	if (!(entry = registry_lookup_service (registry, svc))) {
		g_mutex_unlock (registry->mutex);
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "Could not find service.");
		return xmmsv_new_none ();
	}

	val = xmmsv_ref (entry->description); /* ref it so it's not freed by ipc */
	g_mutex_unlock (registry->mutex);

	return val;
}

/**
 * Pass service method call to service client.
 */
static void
xmms_service_query (xmms_service_registry_t *registry, const gchar *svc,
                    const gchar *meth, xmmsv_t *args, xmms_ipc_client_t *client,
                    xmms_ipc_cookie_t cookie, xmms_error_t *err)
{
	xmmsv_t *pending_data;
	xmmsv_t *meth_desc;
	xmms_ipc_pending_id_t pid;
	GTree *request;
	xmms_service_entry_t *entry;
	xmms_ipc_msg_t *retmsg;
	xmmsv_t *reqval;

	g_return_if_fail (svc);
	g_return_if_fail (meth);
	g_return_if_fail (args);

	XMMS_DBG ("Client requesting method (%s) from service (%s).", meth, svc);

	/* Check service exists */
	g_mutex_lock (registry->mutex);
	if (!(entry = registry_lookup_service (registry, svc))) {
		g_mutex_unlock (registry->mutex);
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT,
		                    "Could not find requested service.");
		return;
	}
	g_mutex_unlock (registry->mutex);

	/* Check args against signature */
	if (!xmmsv_service_get_method (entry->description, meth, &meth_desc)) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT,
		                    "Could not find requested method.");
		return;
	}
	if (!check_args_against_signature (args, meth_desc, err)) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_INVAL,
		                    "Arguments did not match method signature.");
		return;
	}

	pending_data = build_pending_data_dict (svc, meth);

	/* Get a fresh id from the pending pool */
	pid = xmms_ipc_pending_register (pending_data);
	xmmsv_unref (pending_data);

	/* Prepare request structure to send to SC */
	request = build_service_request_dict (pid, meth, args);

	/* Save pending IPC */
	xmms_ipc_pending_save_ipc (pid, XMMS_IPC_OBJECT_SERVICE, client, cookie);

	/* Forward query to SC */
	/* FIXME: ipc/client lock?! */
	reqval = xmms_create_xmmsv_dict (request);
	retmsg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SERVICE, XMMS_IPC_CMD_BROADCAST);
	xmms_ipc_msg_put_value (retmsg, reqval);
	xmms_ipc_msg_set_cookie (retmsg, entry->cookie);
	xmms_ipc_client_msg_write (entry->sc, retmsg);
	xmmsv_unref (reqval);

	XMMS_DBG ("Request (%s) passed to service client (%s).", meth, svc);
}

/**
 * Pass service method return to client.
 */
static void
xmms_service_return (xmms_service_registry_t *registry, xmmsv_t *dict,
                     xmms_ipc_client_t *client, xmms_error_t *err)
{
	xmms_ipc_pending_id_t pid;
	xmmsv_t *retval, *method;
	xmmsv_t *pending_data;
	xmms_service_entry_t *entry;
	const gchar *queried_service, *queried_method;
	xmmsv_type_t rettype;

	g_return_if_fail (dict);
	g_return_if_fail (client);

	/* Extract infos from the SC message */
	if (!extract_return_dict (dict, &pid, &retval)) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "Could not parse return dict.");
		goto err;
	}

	/* Find back infos about the pending query */
	pending_data = xmms_ipc_pending_get_data (pid);
	if (!pending_data ||
	    !extract_pending_data_dict (pending_data, &queried_service, &queried_method)) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "Invalid recorded pending data");
		goto err;
	}

	/* Verify origin of the reply */
	g_mutex_lock (registry->mutex);
	if (!(entry = registry_lookup_service (registry, queried_service))) {
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

	/* Check retval type against method signature */
	if (!xmmsv_service_get_method (entry->description, queried_method, &method)) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT,
		                    "Failed to find queried method.");
		goto err;
	}
	/* no check: description is ensured to be valid */
	xmmsv_service_method_get_return_type (method, &rettype);
	if (xmmsv_get_type (retval) != rettype) {
		XMMS_SERVICE_ERROR (err, XMMS_ERROR_NOENT, "Type of return value "
	                        "does not match method signature.");
		goto err;
	}

	XMMS_DBG ("Returning value from method (%s) of service (%s)",
	          queried_method, queried_service);

	/* Finally send pending message to the original caller */
	xmmsv_ref (retval);
	xmms_ipc_pending_send (pid, retval);

	return; /* Return success (no error) to service client */

err:
	/* FIXME: only send that if we've really failed */
	/* We need to send some error back to the client! */
	if (pid) {
		const gchar *errmsg = "The service client returned an invalid value.";
		retval = xmmsv_new_error (errmsg);
		xmms_ipc_pending_send (pid, retval);
	}

	return;
}


static int
xmms_service_validate (xmmsv_t *desc, gchar **errmsg)
{
	/* FIXME: todo */
	return 1;
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

/** Find service entry by the given name, else return NULL. */
static xmms_service_entry_t *
registry_lookup_service (xmms_service_registry_t *registry, const gchar *name)
{
	return (xmms_service_entry_t *) g_tree_lookup (registry->services, name);
}

static xmmsv_t *
find_arg_value (xmmsv_t *args, const gchar *name)
{
	xmmsv_t *arg, *n;
	xmmsv_list_iter_t *it;
	const gchar *str;

	for (xmmsv_get_list_iter (args, &it);
	     xmmsv_list_iter_valid (it);
	     xmmsv_list_iter_next (it)) {
		xmmsv_list_iter_entry (it, &arg);
		if (xmmsv_dict_get (arg, XMMSC_SERVICE_METHOD_ARG_PROP_NAME, &n) &&
		    xmmsv_get_string (n, &str) && strcmp (str, name) == 0) {
			xmmsv_t *val = NULL;
			xmmsv_dict_get (arg, XMMSC_SERVICE_METHOD_ARG_PROP_VALUE, &val);
			return val;
		}
	}

	return NULL;
}

static gboolean
check_args_against_signature (xmmsv_t *args, xmmsv_t *meth_desc,
                              xmms_error_t *err)
{
	xmmsv_t *arg_list, *arg_desc, *argval;
	xmmsv_list_iter_t *argit;
	const gchar *arg_name;
	xmmsv_type_t arg_type;
	int arg_optional, found_args = 0;

	/* Loops over all arguments in signature */
	xmmsv_service_method_get_argument_list (meth_desc, &arg_list);
	for (xmmsv_get_list_iter (arg_list, &argit);
	     xmmsv_list_iter_valid (argit);
	     xmmsv_list_iter_next (argit)) {

		xmmsv_list_iter_entry (argit, &arg_desc);
		xmmsv_service_method_argument_get_name (arg_desc, &arg_name);
		xmmsv_service_method_argument_get_type (arg_desc, &arg_type);
		xmmsv_service_method_argument_get_optional (arg_desc, &arg_optional);

		/* Check argument is present (or optional) */
		if (!(argval = find_arg_value (args, arg_name))) {
			if (!arg_optional) {
				xmms_error_set (err, XMMS_ERROR_INVAL, "Missing argument.");
				/* FIXME: sprintf dance
				xmms_error_set (err, XMMS_ERROR_INVAL,
			                    "Missing argument '%s'.", arg_name);
				*/
				return false;
			}
		} else if (xmmsv_get_type (argval) != arg_type) {
			xmms_error_set (err, XMMS_ERROR_INVAL, "Argument has invalid type.");
			/* FIXME: sprintf dance
			xmms_error_set (err, XMMS_ERROR_INVAL,
		                    "Argument '%s' has invalid type.", arg_name);
			*/
			return false;
		} else {
			found_args++;
		}
	}

	/* Check that all arg sent are in the signature */
	if (xmmsv_list_get_size (args) > found_args) {
		xmms_error_set (err, XMMS_ERROR_INVAL, "Unknown arguments in the query.");
		return false;
	}

	return true;
}

static xmmsv_t *
build_pending_data_dict (const gchar *svc, const gchar *meth)
{
	xmmsv_t *dict;
	dict = xmmsv_new_dict ();
	dict_insert_string (dict, XMMSC_SERVICE_PENDING_PROP_SERVICE, svc);
	dict_insert_string (dict, XMMSC_SERVICE_PENDING_PROP_METHOD, meth);
	return dict;
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

static gboolean
extract_return_dict (xmmsv_t *dict, xmms_ipc_pending_id_t *pid, xmmsv_t **retval)
{
	xmmsv_t *tmp;
	if (!xmmsv_dict_get (dict, XMMSC_SERVICE_QUERY_PROP_ID, &tmp) ||
	    !xmmsv_get_uint (tmp, pid)) {
		return false;
	}

	if (!xmmsv_dict_get (dict, XMMSC_SERVICE_QUERY_PROP_RETURN, retval)) {
		return false;
	}

	return true;
}

static gboolean
extract_pending_data_dict (xmmsv_t *dict, const gchar **svc, const gchar **meth)
{
	if (!dict_get_string (dict, XMMSC_SERVICE_PENDING_PROP_SERVICE, svc) ||
	    !dict_get_string (dict, XMMSC_SERVICE_PENDING_PROP_METHOD, meth)) {
		return false;
	}

	return true;
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
