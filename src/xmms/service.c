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
	GList *clients;

	guint cookie;

	gchar *ret_type;
	/* Number of arguments */
	guint num_args;
	/* A string of types of arguments */
	gchar *args;
} xmms_service_method_t;

static xmms_service_t *xmms_service;

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
                                                       gchar *ret_type,
                                                       guint num_args,
                                                       gchar *args);
static void xmms_service_destroy (xmms_object_t *object);
static void xmms_service_registry_destroy (gpointer value);
static void xmms_service_method_destroy (gpointer value);

static xmms_service_entry_t *xmms_service_is_registered (gchar *name);
static xmms_service_method_t *
xmms_service_is_method_registered (xmms_service_entry_t *entry, gchar *name);
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
static void xmms_service_list_method (xmms_ipc_msg_t *msg,
                                      xmms_object_cmd_arg_t *arg);
static gboolean xmms_service_matchsc (gpointer key, gpointer value,
                                      gpointer data);
static void xmms_service_foreach (gpointer key, gpointer value, gpointer data);

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

static xmms_service_entry_t *
xmms_service_entry_new (gchar *name, gchar *description, guint major,
                        guint minor, xmms_socket_t client)
{
	xmms_service_entry_t *e;

	e = g_new0 (xmms_service_entry_t, 1);

	if (!e) {
		xmms_log_error ("Service entry initialization failed!");
		return NULL;
	}

	e->name = name;
	e->description = description;
	e->major_version = major;
	e->minor_version = minor;
	e->count = 0;
	e->sc = client;
	e->mutex = g_mutex_new ();
	e->methods = g_hash_table_new_full (g_str_hash, g_str_equal, free,
	                                    xmms_service_method_destroy);

	return e;
}

static xmms_service_method_t *
xmms_service_method_new (gchar *name, gchar *description, guint cookie,
                         gchar *ret_type, guint num_args, gchar *args)
{
	xmms_service_method_t *m;

	m = g_new0 (xmms_service_method_t, 1);

	if (!m) {
		xmms_log_error ("Service method initialization failed!");
		return NULL;
	}

	m->name = name;
	m->description = description;
	m->mutex = g_mutex_new ();
	m->cookie = cookie;
	m->ret_type = ret_type;
	m->num_args = num_args;
	m->args = args;

	return m;
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
	xmms_service_entry_t *v = (xmms_service_entry_t *)value;

	g_return_if_fail (v);

	free (v->description);

	g_mutex_free (v->mutex);

	g_hash_table_destroy (v->methods);
}

/**
 * Free all memory used by method
 */
static void
xmms_service_method_destroy (gpointer value)
{
	xmms_service_method_t *v = (xmms_service_method_t *)value;

	g_return_if_fail (v);

	free (v->description);

	g_mutex_free (v->mutex);
	g_list_free (v->clients);

	free (v->ret_type);
	free (v->args);
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
	XMMS_DBG ("Handling service command.");

	if (cmdid <= XMMS_IPC_CMD_SERVICE_BEGIN ||
		cmdid >= XMMS_IPC_CMD_SERVICE_END) {
		xmms_error_set (&arg->error, XMMS_ERROR_INVAL,
		                "Invalid service command");
		return FALSE;
	}

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
	case XMMS_IPC_CMD_SERVICE_LIST_METHOD:
		xmms_service_list_method (msg, arg);
		return FALSE;
	case XMMS_IPC_CMD_SERVICE_REQUEST:
		return TRUE;
	case XMMS_IPC_CMD_SERVICE_RETURN:
		return FALSE;
	case XMMS_IPC_CMD_SERVICE_SHUTDOWN:
		return FALSE;
	}
}

/**
 * Register a new service
 */
static gboolean
xmms_service_register (xmms_ipc_msg_t *msg, xmms_socket_t client, xmms_error_t *err)
{
	gchar *n;
	guint l;
	gchar *desc;
	guint major, minor;
	xmms_service_entry_t *e;

	g_return_val_if_fail (msg, FALSE);

	if (!xmms_ipc_msg_get_string_alloc (msg, &n, &l)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No service name given");
		return FALSE;
	}
	if (!xmms_ipc_msg_get_string_alloc (msg, &desc, &l)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No service description given");
		return FALSE;
	}
	if (!xmms_ipc_msg_get_uint32 (msg, &major)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No major version given");
		return FALSE;
	}
	if (!xmms_ipc_msg_get_uint32 (msg, &minor)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No minor version given");
		return FALSE;
	}

	if (e = xmms_service_is_registered (n))
		goto method;

	e = xmms_service_entry_new (n, desc, major, minor, client);
	g_mutex_lock (xmms_service->mutex);
	g_hash_table_insert (xmms_service->registry, n, e);
	g_mutex_unlock (xmms_service->mutex);
	XMMS_DBG ("New service registered");

method:
	return xmms_service_method_register (msg, e, err);
}

/**
 * Register a new method
 */
static gboolean
xmms_service_method_register (xmms_ipc_msg_t *msg, xmms_service_entry_t *entry,
                              xmms_error_t *err)
{
	gchar *n;
	guint l;
	gchar *desc;
	gchar *rt = NULL;
	gchar *at = NULL;
	xmms_service_method_t *m;

	/**
	 * It might be the case that service client is only registering a service,
	 * not a method.  So we don't report an error.
	 */
	if (!xmms_ipc_msg_get_string_alloc (msg, &n, &l))
		return FALSE;

	if (!xmms_ipc_msg_get_string_alloc (msg, &desc, &l)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No method description given");
		return FALSE;
	}
	/**
	 * Methods can have no arguments nor return values.
	 */
	xmms_ipc_msg_get_string_alloc (msg, &rt, &l);
	xmms_ipc_msg_get_string_alloc (msg, &at, &l);

	if (m = xmms_service_is_method_registered (entry, n)) {
		xmms_error_set (err, XMMS_ERROR_INVAL, "Method already registered");
		return FALSE;
	}

	gchar *c;
	for (c = rt; rt && *c; c++) {
		switch (*c) {
		case 'u':
		case 'i':
		case 'f':
		case 's':
		case 'l':
		case 'c':
		case 'b':
			break;
		default:
			xmms_error_set (err, XMMS_ERROR_INVAL, "Invalid return type");
			return FALSE;
		}
	}
	l = 0;
	for (c = at; at && *c; c++, l++) {
		switch (*c) {
		case 'u':
		case 'i':
		case 'f':
		case 's':
		case 'l':
		case 'c':
		case 'b':
			break;
		default:
			xmms_error_set (err, XMMS_ERROR_INVAL, "Invalid argument type");
			return FALSE;
		}
	}

	m = xmms_service_method_new (n, desc, xmms_ipc_msg_get_cookie (msg),
	                             rt, l, at);
	g_mutex_lock (entry->mutex);
	entry->count++;
	g_hash_table_insert (entry->methods, n, m);
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
	gchar *n;
	guint l;
	xmms_service_entry_t *e;

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

	if (!xmms_ipc_msg_get_string_alloc (msg, &n, &l)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No service name given");
		return;
	}

	if (!(e = xmms_service_is_registered (n))) {
		xmms_error_set (err, XMMS_ERROR_INVAL, "Invalid service name");
		return;
	}

	if (client != e->sc) {
		xmms_error_set (err, XMMS_ERROR_PERMISSION,
		                "Permission denied to unregister");
		return;
	}

	if (!xmms_service_method_unregister (msg, e, err)) {
		g_mutex_lock (xmms_service->mutex);
		if (!g_hash_table_remove (xmms_service->registry, n))
			xmms_error_set (err, XMMS_ERROR_INVAL, "Invalid service name");
		g_mutex_unlock (xmms_service->mutex);
	}

	XMMS_DBG ("Service unregistered.");

	return;
}

/**
 * Unregister an existing method
 */
static gboolean
xmms_service_method_unregister (xmms_ipc_msg_t *msg,
                                xmms_service_entry_t *entry,
                                xmms_error_t *err)
{
	gchar *n;
	guint l;
	gboolean ret = FALSE;

	if (xmms_ipc_msg_get_string_alloc (msg, &n, &l)) {
		g_mutex_lock (entry->mutex);
		if (!g_hash_table_remove (entry->methods, n)) {
			xmms_error_set (err, XMMS_ERROR_INVAL, "Invalid method name!");
			ret = TRUE;
		} else
			ret = --(entry->count) == 0 ? FALSE : TRUE;
		g_mutex_unlock (entry->mutex);

		XMMS_DBG ("Method unregistered.");
	}

	return ret;
}

static xmms_service_entry_t *
xmms_service_is_registered (gchar *name)
{
	xmms_service_entry_t *ret;

	g_return_val_if_fail (name, NULL);

	g_mutex_lock (xmms_service->mutex);
	ret = g_hash_table_lookup (xmms_service->registry, name);
	g_mutex_unlock (xmms_service->mutex);

	return ret;
}

static xmms_service_method_t *
xmms_service_is_method_registered (xmms_service_entry_t *entry, gchar *name)
{
	xmms_service_method_t *ret;

	g_return_val_if_fail (entry, NULL);
	g_return_val_if_fail (name, NULL);

	g_mutex_lock (entry->mutex);
	ret = g_hash_table_lookup (entry->methods, name);
	g_mutex_unlock (entry->mutex);

	return ret;
}

/**
 * List all available service ids or details of a single service.
 */
static void
xmms_service_list (xmms_ipc_msg_t *msg, xmms_object_cmd_arg_t *arg)
{
	gchar *n;
	guint l;
	xmms_service_entry_t *e;

	g_return_if_fail (msg);

	if (xmms_ipc_msg_get_string_alloc (msg, &n, &l)) {
		if (!(e = xmms_service_is_registered (n))) {
			xmms_error_set (&arg->error, XMMS_ERROR_INVAL, "Invalid service id");
			return;
		}

		GHashTable *dict = g_hash_table_new_full (g_str_hash,
		                                          g_str_equal,
		                                          NULL,
		                                          xmms_object_cmd_value_free);
		xmms_object_cmd_value_t *val;

		g_mutex_lock (xmms_service->mutex);
		val = xmms_object_cmd_value_str_new (e->name);
		g_hash_table_insert (dict, "name", val);
		val = xmms_object_cmd_value_str_new (e->description);
		g_hash_table_insert (dict, "description", val);
		val = xmms_object_cmd_value_uint_new (e->major_version);
		g_hash_table_insert (dict, "major version", val);
		val = xmms_object_cmd_value_uint_new (e->minor_version);
		g_hash_table_insert (dict, "minor version", val);
		val = xmms_object_cmd_value_uint_new (e->count);
		g_hash_table_insert (dict, "count", val);
		g_mutex_unlock (xmms_service->mutex);

		arg->retval = xmms_object_cmd_value_dict_new (dict);
	} else {
		GList *list = NULL;

		g_mutex_lock (xmms_service->mutex);
		g_hash_table_foreach (xmms_service->registry,
		                      xmms_service_foreach, &list);
		g_mutex_unlock (xmms_service->mutex);

		arg->retval = xmms_object_cmd_value_list_new (list);
	}

	return;
}

/**
 * List all available method ids of a service or details of a single method.
 */
static void
xmms_service_list_method (xmms_ipc_msg_t *msg, xmms_object_cmd_arg_t *arg)
{

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
xmms_service_foreach (gpointer key, gpointer value, gpointer data)
{
	GList **l = data;

	*l = g_list_prepend (*l, xmms_object_cmd_value_str_new (key));
}

/** @} */
