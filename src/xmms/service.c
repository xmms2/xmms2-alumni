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

#include <stdlib.h>
#include <string.h>
#include "xmms/xmms_log.h"
#include "xmmspriv/xmms_ipc.h"
#include "xmmspriv/xmms_service.h"

/**
 * @defgroup Service Service
 * @ingroup XMMSServer
 * @brief Service client functions for XMMS2 Daemon
 * @{
 */

/**
 * A single service representation
 */
typedef struct xmms_service_entry_St {
	gchar *name;
	gchar *description;

	guint major_version;
	guint minor_version;

	/* Service client fd */
	xmms_socket_t sc;

	GMutex *mutex;
	guint count;
	GHashTable *methods;
} xmms_service_entry_t;

/**
 * A single method representation
 */
typedef struct xmms_service_method_St {
	gchar *name;
	gchar *description;

	GMutex *mutex;
	GHashTable *clients;

	guint cookie;

	guint num_rets;
	GHashTable *rets;
	/* Number of arguments */
	guint num_args;
	GHashTable *args;
} xmms_service_method_t;

static xmms_service_t *xmms_service;

/**
 * @internal
 */
typedef struct xmms_service_client_St {
	xmms_socket_t fd;
	guint cookie;
} xmms_service_client_t;

typedef struct xmms_service_argument_St {
	gchar *name;
	xmms_object_cmd_arg_type_t type;
	gboolean optional;
} xmms_service_argument_t;

/**
 * Functions
 */
static xmms_service_entry_t *xmms_service_entry_new (gchar *name,
                                                     gchar *description,
                                                     guint major, guint minor,
                                                     xmms_socket_t client);
static xmms_service_method_t *xmms_service_method_new (gchar *name,
                                                       gchar *description,
                                                       guint cookie,
                                                       guint num_rets,
                                                       GHashTable *rets,
                                                       guint num_args,
                                                       GHashTable *args);
static void xmms_service_destroy (xmms_object_t *object);
static void xmms_service_registry_destroy (gpointer value);
static void xmms_service_method_destroy (gpointer value);

static gboolean xmms_service_register (xmms_ipc_msg_t *msg, xmms_socket_t client,
                                       xmms_error_t *err);
static gboolean xmms_service_method_register (xmms_ipc_msg_t *msg,
                                              xmms_service_entry_t *entry,
                                              xmms_error_t *err);
static void xmms_service_unregister (xmms_ipc_msg_t *msg, xmms_socket_t client,
                                     xmms_error_t *err);
static gboolean xmms_service_method_unregister (xmms_ipc_msg_t *msg,
                                                xmms_service_entry_t *entry,
                                                xmms_error_t *err);
static void xmms_service_list (xmms_ipc_msg_t *msg, xmms_object_cmd_arg_t *arg);
static void xmms_service_method_list (xmms_ipc_msg_t *msg,
                                      xmms_object_cmd_arg_t *arg);
static gboolean xmms_service_request (xmms_ipc_msg_t *msg, xmms_socket_t client,
                                      xmms_error_t *err);
static void xmms_service_return (xmms_ipc_msg_t *msg, xmms_error_t *err);

static xmms_service_entry_t *xmms_service_is_registered (const gchar *name);
static xmms_service_method_t *
xmms_service_method_is_registered (xmms_service_entry_t *entry,
                                   const gchar *name);
static gboolean xmms_service_matchsc (gpointer key, gpointer value,
                                      gpointer data);
static void xmms_service_key_insert (gpointer key, gpointer value,
                                     gpointer data);
static void xmms_service_arg_insert (gpointer key, gpointer value,
                                     gpointer data);
static inline guint xmms_service_next_id (void);
static xmms_service_entry_t *xmms_service_get (xmms_ipc_msg_t *msg, gchar **name,
                                               xmms_error_t *err);
static xmms_service_method_t *
xmms_service_method_get (xmms_ipc_msg_t *msg, xmms_service_entry_t *entry,
                         gchar **name, xmms_error_t *err);
static GHashTable *xmms_service_arg_types_parse (xmms_ipc_msg_t *msg,
                                                 xmms_error_t *err);
static GHashTable *xmms_service_args_parse (xmms_ipc_msg_t *msg,
                                            GHashTable *args, xmms_error_t *err);

/**
 * Initialize service client handling
 */
xmms_service_t *
xmms_service_init(void)
{
	xmms_service_t *ret;

	ret = xmms_object_new (xmms_service_t, xmms_service_destroy);
	ret->mutex = g_mutex_new ();
	ret->registry = g_hash_table_new_full (g_str_hash, g_str_equal, free,
										   xmms_service_registry_destroy);

	xmms_ipc_broadcast_register (XMMS_OBJECT (ret), XMMS_IPC_SIGNAL_SERVICE);

	xmms_service = ret;

	XMMS_DBG ("Service object initialized.");

	return ret;
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
gboolean
xmms_service_handle (xmms_ipc_msg_t *msg, uint32_t cmdid, xmms_socket_t client,
                     xmms_object_cmd_arg_t *arg)
{
	switch (cmdid) {
	case XMMS_IPC_CMD_SERVICE_REGISTER:
		arg->retval = xmms_object_cmd_value_none_new ();
		return xmms_service_register (msg, client, &arg->error);
	case XMMS_IPC_CMD_SERVICE_UNREGISTER:
		if (arg) {
			arg->retval = xmms_object_cmd_value_none_new ();
			xmms_service_unregister (msg, client, &arg->error);
		} else
			xmms_service_unregister (msg, client, NULL);
		return FALSE;
	case XMMS_IPC_CMD_SERVICE_LIST:
		xmms_service_list (msg, arg);
		return FALSE;
	case XMMS_IPC_CMD_SERVICE_METHOD_LIST:
		xmms_service_method_list (msg, arg);
		return FALSE;
	case XMMS_IPC_CMD_SERVICE_REQUEST:
		arg->retval = xmms_object_cmd_value_none_new ();
		return xmms_service_request (msg, client, &arg->error);
	case XMMS_IPC_CMD_SERVICE_RETURN:
		arg->retval = xmms_object_cmd_value_none_new ();
		xmms_service_return (msg, &arg->error);
		return FALSE;
	case XMMS_IPC_CMD_SERVICE_SHUTDOWN:
		return FALSE;
	default:
		xmms_error_set (&arg->error, XMMS_ERROR_INVAL,
		                "Invalid service command");
		return FALSE;
	}
}

static xmms_service_entry_t *
xmms_service_entry_new (gchar *name, gchar *description, guint major,
                        guint minor, xmms_socket_t client)
{
	xmms_service_entry_t *entry;

	entry = g_new0 (xmms_service_entry_t, 1);

	if (!entry) {
		xmms_log_error ("Service entry initialization failed!");
		return NULL;
	}

	entry->name = name;
	entry->description = description;
	entry->major_version = major;
	entry->minor_version = minor;
	entry->count = 0;
	entry->sc = client;
	entry->mutex = g_mutex_new ();
	entry->methods = g_hash_table_new_full (g_str_hash, g_str_equal, free,
	                                        xmms_service_method_destroy);

	return entry;
}

static xmms_service_method_t *
xmms_service_method_new (gchar *name, gchar *description, guint cookie,
                         guint num_rets, GHashTable *rets,
                         guint num_args, GHashTable *args)
{
	xmms_service_method_t *method;

	method = g_new0 (xmms_service_method_t, 1);

	if (!method) {
		xmms_log_error ("Service method initialization failed!");
		return NULL;
	}

	method->name = name;
	method->description = description;
	method->mutex = g_mutex_new ();
	method->clients = g_hash_table_new_full (g_direct_hash, g_direct_equal,
	                                         NULL, g_free);
	method->cookie = cookie;
	method->num_rets = num_rets;
	method->rets = rets;
	method->num_args = num_args;
	method->args = args;

	return method;
}

/**
 * Free all the memory used by xmms_service_t
 */
static void
xmms_service_destroy (xmms_object_t *object)
{
	xmms_service_t *service = (xmms_service_t *)object;

	g_return_if_fail (service);

	g_mutex_free (service->mutex);

	g_hash_table_destroy (service->registry);

	xmms_ipc_signal_unregister (XMMS_IPC_SIGNAL_SERVICE);

	XMMS_DBG ("Service object shutdown.");
}

/**
 * Free all the memory used by registry entry
 */
static void
xmms_service_registry_destroy (gpointer value)
{
	xmms_service_entry_t *val = (xmms_service_entry_t *)value;

	g_return_if_fail (val);

	free (val->description);

	g_mutex_free (val->mutex);

	g_hash_table_destroy (val->methods);
}

/**
 * Free all memory used by method
 */
static void
xmms_service_method_destroy (gpointer value)
{
	xmms_service_method_t *val = (xmms_service_method_t *)value;

	g_return_if_fail (val);

	free (val->description);

	g_mutex_free (val->mutex);
	g_hash_table_destroy (val->clients);

	g_hash_table_destroy (val->rets);
	g_hash_table_destroy (val->args);
}

/**
 * Register a new service
 */
static gboolean
xmms_service_register (xmms_ipc_msg_t *msg, xmms_socket_t client, xmms_error_t *err)
{
	gchar *name = NULL;
	guint len;
	gchar *desc = NULL;
	guint major, minor;
	xmms_service_entry_t *entry;

	g_return_val_if_fail (msg, FALSE);

	if (!xmms_ipc_msg_get_string_alloc (msg, &name, &len)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No service id given");
		free (name);
		free (desc);
		return FALSE;
	}
	if (!xmms_ipc_msg_get_string_alloc (msg, &desc, &len)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No service description given");
		free (name);
		free (desc);
		return FALSE;
	}
	if (!xmms_ipc_msg_get_uint32 (msg, &major)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No major version given");
		free (name);
		free (desc);
		return FALSE;
	}
	if (!xmms_ipc_msg_get_uint32 (msg, &minor)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No minor version given");
		free (name);
		free (desc);
		return FALSE;
	}

	if (entry = xmms_service_is_registered (name)) {
		free (name);
		free (desc);
	} else {
		entry = xmms_service_entry_new (name, desc, major, minor, client);
		g_mutex_lock (xmms_service->mutex);
		g_hash_table_insert (xmms_service->registry, name, entry);
		g_mutex_unlock (xmms_service->mutex);
		XMMS_DBG ("New service registered");
	}

	return xmms_service_method_register (msg, entry, err);
}

/**
 * Register a new method
 */
static gboolean
xmms_service_method_register (xmms_ipc_msg_t *msg, xmms_service_entry_t *entry,
                              xmms_error_t *err)
{
	gchar *name = NULL;
	guint len;
	gchar *desc = NULL;
	GHashTable *rets = NULL;
	GHashTable *args = NULL;
	xmms_service_method_t *method;

	/**
	 * It might be the case that service client is only registering a service,
	 * not a method.  So we don't report an error.
	 */
	if (!xmms_ipc_msg_get_string_alloc (msg, &name, &len)) {
		free (name);
		free (desc);
		return FALSE;
	}

	if (!xmms_ipc_msg_get_string_alloc (msg, &desc, &len)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No method description given");
		free (name);
		free (desc);
		return FALSE;
	}
	if (!(rets = xmms_service_arg_types_parse (msg, err))) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No method return types given");
		free (name);
		free (desc);
		return FALSE;
	}
	if (!(args = xmms_service_arg_types_parse (msg, err))) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No method argument types given");
		free (name);
		free (desc);
		return FALSE;
	}

	if (method = xmms_service_method_is_registered (entry, name)) {
		xmms_error_set (err, XMMS_ERROR_INVAL, "Method already registered");
		free (name);
		free (desc);
		return FALSE;
	}

	method = xmms_service_method_new (name, desc, xmms_ipc_msg_get_cookie (msg),
	                                  g_hash_table_size (rets), rets,
	                                  g_hash_table_size (args), args);
	g_mutex_lock (entry->mutex);
	entry->count++;
	g_hash_table_insert (entry->methods, name, method);
	g_mutex_unlock (entry->mutex);

	XMMS_DBG ("New method registered");

	return TRUE;
}

/**
 * Unregister an existing service
 */
static void
xmms_service_unregister (xmms_ipc_msg_t *msg, xmms_socket_t client, xmms_error_t *err)
{
	gchar *name = NULL;
	xmms_service_entry_t *entry;

	if (!msg) {
		guint ret;
		g_mutex_lock (xmms_service->mutex);
		ret = g_hash_table_foreach_remove (xmms_service->registry,
		                                   xmms_service_matchsc,
		                                   GUINT_TO_POINTER (client));
		g_mutex_unlock (xmms_service->mutex);
		if (ret > 1)
			xmms_log_error ("Removed more than 1 registry entry, "
			                "registry corrupted?");
		else if (ret == 1)
			XMMS_DBG ("Service client (%d) just vaporized! "
			          "Removed from registry.", client);
		return;
	}

	if (!(entry = xmms_service_get (msg, &name, err)))
		return;

	if (client != entry->sc) {
		xmms_error_set (err, XMMS_ERROR_PERMISSION,
		                "Permission denied to unregister");
		free (name);
		return;
	}

	if (!xmms_service_method_unregister (msg, entry, err)) {
		g_mutex_lock (xmms_service->mutex);
		if (!g_hash_table_remove (xmms_service->registry, name))
			xmms_error_set (err, XMMS_ERROR_GENERIC, "Failed to remove service");
		g_mutex_unlock (xmms_service->mutex);
	}

	XMMS_DBG ("Service unregistered.");

	free (name);
	return;
}

/**
 * Unregister an existing method
 */
static gboolean
xmms_service_method_unregister (xmms_ipc_msg_t *msg, xmms_service_entry_t *entry,
                                xmms_error_t *err)
{
	gchar *name = NULL;
	gboolean ret = FALSE;

	if (xmms_service_method_get (msg, entry, &name, err)) {
		g_mutex_lock (entry->mutex);
		if (!g_hash_table_remove (entry->methods, name)) {
			xmms_error_set (err, XMMS_ERROR_INVAL, "Invalid method name!");
			ret = TRUE;
		} else
			ret = --(entry->count) == 0 ? FALSE : TRUE;
		g_mutex_unlock (entry->mutex);

		XMMS_DBG ("Method unregistered.");
	}

	free (name);
	return ret;
}

/**
 * List all available service ids or details of a single service.
 */
static void
xmms_service_list (xmms_ipc_msg_t *msg, xmms_object_cmd_arg_t *arg)
{
	gchar *name = NULL;
	xmms_service_entry_t *entry;

	g_return_if_fail (msg);

	if (entry = xmms_service_get (msg, &name, &arg->error)) {
		GHashTable *dict = g_hash_table_new_full (g_str_hash,
		                                          g_str_equal,
		                                          NULL,
		                                          xmms_object_cmd_value_free);
		xmms_object_cmd_value_t *val;

		g_mutex_lock (entry->mutex);
		val = xmms_object_cmd_value_str_new (entry->name);
		g_hash_table_insert (dict, "name", val);
		val = xmms_object_cmd_value_str_new (entry->description);
		g_hash_table_insert (dict, "description", val);
		val = xmms_object_cmd_value_uint_new (entry->major_version);
		g_hash_table_insert (dict, "major_version", val);
		val = xmms_object_cmd_value_uint_new (entry->minor_version);
		g_hash_table_insert (dict, "minor_version", val);
		val = xmms_object_cmd_value_uint_new (entry->count);
		g_hash_table_insert (dict, "count", val);
		g_mutex_unlock (entry->mutex);

		arg->retval = xmms_object_cmd_value_dict_new (dict);
		free (name);
	} else {
		xmms_error_reset (&arg->error);

		GList *list = NULL;

		g_mutex_lock (xmms_service->mutex);
		g_hash_table_foreach (xmms_service->registry,
		                      xmms_service_key_insert, &list);
		g_mutex_unlock (xmms_service->mutex);

		arg->retval = xmms_object_cmd_value_list_new (list);
	}

	return;
}

/**
 * List all available method ids of a service or details of a single method.
 */
static void
xmms_service_method_list (xmms_ipc_msg_t *msg, xmms_object_cmd_arg_t *arg)
{
	gchar *name = NULL;
	GList *list = NULL;
	xmms_service_entry_t *entry;
	xmms_service_method_t *method;
	guint i;

	g_return_if_fail (msg);

	if (!(entry = xmms_service_get (msg, &name, &arg->error)))
		return;

	free (name);

	if (method = xmms_service_method_get (msg, entry, &name, &arg->error)) {
		if (xmms_ipc_msg_get_uint32 (msg, &i) && i == TRUE) {
			g_mutex_lock (method->mutex);
			g_hash_table_foreach (method->args, xmms_service_arg_insert, &list);
			g_mutex_unlock (method->mutex);
		} else {
			GHashTable *dict = g_hash_table_new_full (g_str_hash,
													  g_str_equal,
													  NULL,
													  xmms_object_cmd_value_free);
			xmms_object_cmd_value_t *val;

			g_mutex_lock (method->mutex);
			val = xmms_object_cmd_value_str_new (method->name);
			g_hash_table_insert (dict, "name", val);
			val = xmms_object_cmd_value_str_new (method->description);
			g_hash_table_insert (dict, "description", val);
			val = xmms_object_cmd_value_uint_new (method->num_rets);
			g_hash_table_insert (dict, "num_rets", val);
			val = xmms_object_cmd_value_uint_new (method->num_args);
			g_hash_table_insert (dict, "num_args", val);
			g_mutex_unlock (method->mutex);

			arg->retval = xmms_object_cmd_value_dict_new (dict);
			free (name);
			return;
		}

		free (name);
	} else {
		xmms_error_reset (&arg->error);

		g_mutex_lock (entry->mutex);
		g_hash_table_foreach (entry->methods, xmms_service_key_insert, &list);
		g_mutex_unlock (entry->mutex);
	}

	arg->retval = xmms_object_cmd_value_list_new (list);

	return;
}

/**
 * Pass service method call to service client.
 *
 * Argument name "sc_id" is reserved, don't use it.
 */
static gboolean
xmms_service_request (xmms_ipc_msg_t *msg, xmms_socket_t client,
                      xmms_error_t *err)
{
	gchar *name = NULL;
	guint len;
	xmms_service_entry_t *entry;
	xmms_service_method_t *method;
	GHashTable *table = NULL;
	guint next;
	xmms_service_client_t *cli = NULL;
	xmms_object_cmd_arg_t arg;

	g_return_val_if_fail (msg, FALSE);

	if (!(entry = xmms_service_get (msg, &name, err)))
		return FALSE;

	free (name);

	if (!(method = xmms_service_method_get (msg, entry, &name, err)))
		return FALSE;

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
	g_mutex_lock (method->mutex);
	g_hash_table_insert (method->clients, GUINT_TO_POINTER (next), cli);
	g_mutex_unlock (method->mutex);

	xmms_object_cmd_arg_init (&arg);
	arg.retval = xmms_object_cmd_value_dict_new (table);
	arg.values[0].type = XMMS_OBJECT_CMD_ARG_UINT32;
	arg.values[0].value.uint32 = entry->sc;
	arg.values[1].type = XMMS_OBJECT_CMD_ARG_UINT32;
	arg.values[1].value.uint32 = method->cookie;
	xmms_object_emit (XMMS_OBJECT (xmms_service),
	                  XMMS_IPC_SIGNAL_SERVICE,
	                  &arg);

	xmms_object_cmd_value_free (arg.retval);

	return TRUE;
}

/**
 * Pass service method return to client.
 */
static void
xmms_service_return (xmms_ipc_msg_t *msg, xmms_error_t *err)
{

}

static xmms_service_entry_t *
xmms_service_is_registered (const gchar *name)
{
	xmms_service_entry_t *ret;

	if (!name)
		return NULL;

	g_mutex_lock (xmms_service->mutex);
	ret = g_hash_table_lookup (xmms_service->registry, name);
	g_mutex_unlock (xmms_service->mutex);

	return ret;
}

static xmms_service_method_t *
xmms_service_method_is_registered (xmms_service_entry_t *entry,
                                   const gchar *name)
{
	xmms_service_method_t *ret;

	g_return_val_if_fail (entry, NULL);
	if (!name)
		return NULL;

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
	gchar *k = key;
	xmms_service_entry_t *v = value;
	guint sc = GPOINTER_TO_UINT (data);

	return (v->sc == sc);
}

/**
 * Insert keys into a list
 */
static void
xmms_service_key_insert (gpointer key, gpointer value, gpointer data)
{
	GList **l = data;

	*l = g_list_prepend (*l, xmms_object_cmd_value_str_new (key));
}

/**
 * Insert xmms_service_argument_t elements into a dict and then put it into a
 * list
 */
static void
xmms_service_arg_insert (gpointer key, gpointer value, gpointer data)
{
	GList **l = data;
	xmms_service_argument_t *val = (xmms_service_argument_t *)value;
	GHashTable *t = g_hash_table_new_full (g_str_hash, g_str_equal,
	                                       NULL, xmms_object_cmd_value_free);

	g_hash_table_insert (t, "name", xmms_object_cmd_value_str_new (val->name));
	g_hash_table_insert (t, "type", xmms_object_cmd_value_uint_new (val->type));
	g_hash_table_insert (t, "optional",
	                     xmms_object_cmd_value_uint_new (val->optional));

	*l = g_list_prepend (*l, xmms_object_cmd_value_dict_new (t));
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
xmms_service_get (xmms_ipc_msg_t *msg, gchar **name, xmms_error_t *err)
{
	guint len;
	xmms_service_entry_t *entry = NULL;

	g_return_val_if_fail (msg, NULL);

	*name = NULL;

	if (!xmms_ipc_msg_get_string_alloc (msg, name, &len)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No service id given");
		free (*name);
	}

	if (!(entry = xmms_service_is_registered (*name))) {
		xmms_error_set (err, XMMS_ERROR_INVAL, "Invalid service id");
		free (*name);
	}

	return entry;
}

/**
 * Get the method with the given name
 */
static xmms_service_method_t *
xmms_service_method_get (xmms_ipc_msg_t *msg, xmms_service_entry_t *entry,
                         gchar **name, xmms_error_t *err)
{
	guint len;
	xmms_service_method_t *method = NULL;

	g_return_val_if_fail (msg, NULL);

	*name = NULL;

	if (!xmms_ipc_msg_get_string_alloc (msg, name, &len)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No method id given");
		free (*name);
	}

	if (!(method = xmms_service_method_is_registered (entry, *name))) {
		xmms_error_set (err, XMMS_ERROR_INVAL, "Invalid method id");
		free (*name);
	}

	return method;
}

/**
 * Parse method signature
 */
static GHashTable *
xmms_service_arg_types_parse (xmms_ipc_msg_t *msg, xmms_error_t *err)
{
	xmms_service_argument_t *arg = NULL;
	guint len;
	guint num;
	GHashTable *table = NULL;

	g_return_val_if_fail (msg, NULL);

	table = g_hash_table_new_full (g_str_hash, g_str_equal, free, g_free);

	if (!xmms_ipc_msg_get_uint32 (msg, &num)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No number of arguments given");
		return NULL;
	}

	for (; num > 0; num--) {
		arg = g_new0 (xmms_service_argument_t, 1);

		if (!xmms_ipc_msg_get_string_alloc (msg, &arg->name, &len)) {
			xmms_error_set (err, XMMS_ERROR_NOENT, "No argument name given");
			free (arg->name);
			g_free (arg);
			g_hash_table_destroy (table);
			return NULL;
		}
		if (!xmms_ipc_msg_get_uint32 (msg, &arg->type)) {
			xmms_error_set (err, XMMS_ERROR_NOENT, "No argument type given");
			free (arg->name);
			g_free (arg);
			g_hash_table_destroy (table);
			return NULL;
		}
		if (!xmms_ipc_msg_get_uint32 (msg, &arg->optional)) {
			xmms_error_set (err, XMMS_ERROR_NOENT,
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
static GHashTable *
xmms_service_args_parse (xmms_ipc_msg_t *msg, GHashTable *args,
                         xmms_error_t *err)
{
	gchar *name = NULL;
	guint len, none;
	xmms_service_argument_t *arg = NULL;
	guint i;
	xmms_object_cmd_value_t *val = NULL;
	GHashTable *table = NULL;

	g_return_val_if_fail (msg, NULL);

	table = g_hash_table_new_full (g_str_hash, g_str_equal, free,
	                               xmms_object_cmd_value_free);

	for (i = g_hash_table_size (args); i > 0; i--) {
		if (!xmms_ipc_msg_get_string_alloc (msg, &name, &len)) {
			xmms_error_set (err, XMMS_ERROR_NOENT, "No argument name given");
			free (name);
			g_hash_table_destroy (table);
			return NULL;
		}
		if (!(arg = g_hash_table_lookup (args, name))) {
			xmms_error_set (err, XMMS_ERROR_INVAL, "Invalid argument");
			free (name);
			g_hash_table_destroy (table);
			return NULL;
		}
		if (xmms_ipc_msg_get_uint32 (msg, &none) && none) {
			if (!arg->optional) {
				xmms_error_set (err, XMMS_ERROR_INVAL,
				                "Required argument not given");
				free (name);
				g_hash_table_destroy (table);
				return NULL;
			}

			continue;
		}
		switch (arg->type) {
		case XMMS_OBJECT_CMD_ARG_UINT32:
		{
			guint tmp;
			if (!xmms_ipc_msg_get_uint32 (msg, &tmp))
				goto inval;
			val = xmms_object_cmd_value_uint_new (tmp);
		}
		break;
		case XMMS_OBJECT_CMD_ARG_INT32:
		{
			gint tmp;
			if (!xmms_ipc_msg_get_int32 (msg, &tmp))
				goto inval;
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
		}
		break;
		case XMMS_OBJECT_CMD_ARG_STRINGLIST:
		{
			GList *tmp = NULL;
			guint size, k;
			if (!xmms_ipc_msg_get_uint32 (msg, &size))
				goto inval;
			for (k = 0; k < size; k++) {
				gchar *buf;
				if (!xmms_ipc_msg_get_string_alloc (msg, &buf, &len) ||
					!(tmp = g_list_prepend (tmp, buf))) {
					free (buf);
					while (tmp) {
						g_free (tmp->data);
						tmp = g_list_delete_link (tmp, tmp);
					}
					goto inval;
				}
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
			xmms_error_set (err, XMMS_ERROR_INVAL, "Invalid argument");
			free (name);
			g_hash_table_destroy (table);
			return NULL;
		}

		g_hash_table_insert (table, name, val);
	}

	return table;
}

/** @} */
