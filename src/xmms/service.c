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
	guint sc;

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
static void xmms_service_destroy (xmms_object_t *object);
static void xmms_service_registry_destroy (gpointer value);
static void xmms_service_method_destroy (gpointer value);

static xmms_service_entry_t *xmms_service_is_registered (gchar *name);
static xmms_service_method_t *
xmms_service_is_method_registered (xmms_service_entry_t *entry, gchar *name);
static gboolean xmms_service_register (xmms_ipc_msg_t *msg, guint client,
                                       xmms_error_t *err);
static void xmms_service_method_register (xmms_ipc_msg_t *msg,
                                          xmms_service_entry_t *entry,
                                          xmms_error_t *err);
static void xmms_service_unregister (xmms_ipc_msg_t *msg, guint client,
                                     xmms_error_t *err);
static gboolean xmms_service_method_unregister (xmms_ipc_msg_t *msg,
                                                xmms_service_entry_t *entry,
                                                xmms_error_t *err);
static xmms_service_entry_t *xmms_service_entry_new (gchar *name,
                                                     gchar *description,
                                                     guint major, guint minor,
                                                     guint client);
static xmms_service_method_t *xmms_service_method_new (gchar *name,
                                                       gchar *description,
                                                       gchar *ret_type,
                                                       guint num_args,
                                                       gchar *args);

/**
 * Initialize service client handling
 */
xmms_service_t *
xmms_service_init(void)
{
	xmms_service_t *ret;

	XMMS_DBG ("Initializing service object.");

	ret = xmms_object_new (xmms_service_t, xmms_service_destroy);
	ret->mutex = g_mutex_new ();
	ret->registry = g_hash_table_new_full (g_str_hash, g_str_equal, g_free,
										   xmms_service_registry_destroy);

	xmms_ipc_broadcast_register (XMMS_OBJECT (ret), XMMS_IPC_SIGNAL_SERVICE);

	xmms_service = ret;

	return ret;
}

/**
 * Free all the memory used by xmms_service_t
 */
static void
xmms_service_destroy (xmms_object_t *object)
{
	xmms_service_t *service = (xmms_service_t *)object;

	XMMS_DBG ("Shutting service object down.");

	g_return_if_fail (service);

	g_mutex_free (service->mutex);

	g_hash_table_destroy (service->registry);

	xmms_ipc_signal_unregister (XMMS_IPC_SIGNAL_SERVICE);
}

/**
 * Free all the memory used by registry entry
 */
static void
xmms_service_registry_destroy (gpointer value)
{
	xmms_service_entry_t *v = (xmms_service_entry_t *)value;

	g_return_if_fail (v);

	g_free (v->name);
	g_free (v->description);

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

	g_free (v->name);
	g_free (v->description);

	g_mutex_free (v->mutex);
	g_list_free (v->clients);

	g_free (v->args);
}

/**
 * Handle service related commands
 *
 * Based on the type parameter passed in, specific functions will be called to
 * handle the command.
 *
 * @param msg Message from (service) client.
 * @param cmdid Command ID.
 * @param data Service client's transport path if registering, otherwise it's message cookie.
 * @param arg Command arguments.
 * @returns True if the cookie needs to be added to broadcast list, false otherwise
 */
gboolean
xmms_service_handle (xmms_ipc_msg_t *msg, uint32_t cmdid, gpointer data,
					 xmms_object_cmd_arg_t *arg)
{
	guint id;

	XMMS_DBG ("Handling service command.");

	g_return_val_if_fail (msg, FALSE);

	if (cmdid <= XMMS_IPC_CMD_SERVICE_BEGIN ||
		cmdid >= XMMS_IPC_CMD_SERVICE_END) {
		xmms_error_set (&arg->error, XMMS_ERROR_INVAL,
		                "Invalid service command");
		return FALSE;
	}

	arg->retval = xmms_object_cmd_value_none_new ();
	id = GPOINTER_TO_UINT (data);

	switch (cmdid) {
	case XMMS_IPC_CMD_SERVICE_REGISTER:
		return xmms_service_register (msg, id, &arg->error);
	case XMMS_IPC_CMD_SERVICE_UNREGISTER:
		xmms_service_unregister (msg, id, &arg->error);
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
xmms_service_register (xmms_ipc_msg_t *msg, guint client, xmms_error_t *err)
{
	gchar *n;
	guint l;
	gchar *desc;
	guint major, minor;
	xmms_service_entry_t *e;
	gboolean ret = FALSE;

	g_return_val_if_fail (msg, ret);

	if (!xmms_ipc_msg_get_string_alloc (msg, &n, &l)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No service name given");
		return ret;
	}
	if (!xmms_ipc_msg_get_string_alloc (msg, &desc, &l)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No service description given");
		return ret;
	}
	if (!xmms_ipc_msg_get_uint32 (msg, &major)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No major version given");
		return ret;
	}
	if (!xmms_ipc_msg_get_uint32 (msg, &minor)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No minor version given");
		return ret;
	}

	if (e = xmms_service_is_registered (n))
		goto method;

	XMMS_DBG ("Registering new service");
	e = xmms_service_entry_new (n, desc, major, minor, client);
	g_mutex_lock (xmms_service->mutex);
	g_hash_table_insert (xmms_service->registry, n, e);
	g_mutex_unlock (xmms_service->mutex);
	ret = TRUE;
	XMMS_DBG ("New service registered");

method:
	xmms_service_method_register (msg, e, err);
	return ret;
}

/**
 * Register a new method
 */
static void
xmms_service_method_register (xmms_ipc_msg_t *msg, xmms_service_entry_t *entry,
                              xmms_error_t *err)
{
	gchar *n;
	guint l;
	gchar *desc;
	gchar *rt, *at;
	xmms_service_method_t *m;

	XMMS_DBG ("Registering new method");

	/**
	 * It might be the case that service client is only registering a service,
	 * not a method.  So we don't report an error.
	 */
	if (!xmms_ipc_msg_get_string_alloc (msg, &n, &l))
		return;

	if (!xmms_ipc_msg_get_string_alloc (msg, &desc, &l)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No method description given");
		return;
	}
	if (!xmms_ipc_msg_get_string_alloc (msg, &rt, &l)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No return types given");
		return;
	}
	if (!xmms_ipc_msg_get_string_alloc (msg, &at, &l)) {
		xmms_error_set (err, XMMS_ERROR_NOENT, "No argument types given");
		return;
	}

	if (m = xmms_service_is_method_registered (entry, n)) {
		xmms_error_set (err, XMMS_ERROR_INVAL, "Method already registered");
		return;
	}

	gchar *c;
	for (c = rt; *c; c++) {
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
			return;
		}
	}
	l = 0;
	for (c = at; *c; c++, l++) {
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
			return;
		}
	}

	m = xmms_service_method_new (n, desc, rt, l, at);
	g_mutex_lock (entry->mutex);
	entry->count++;
	g_hash_table_insert (entry->methods, n, m);
	g_mutex_unlock (entry->mutex);

	XMMS_DBG ("New method registered");
}

/**
 * Unregister an existing service
 */
static void
xmms_service_unregister (xmms_ipc_msg_t *msg, guint client,
                         xmms_error_t *err)
{
	gchar *n;
	guint l;
	xmms_service_entry_t *e;

	XMMS_DBG ("Unregistering service");

	g_return_if_fail (msg);

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
		g_mutex_lock (xmms_service->mutex);
	}

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

	XMMS_DBG ("Unregistering method");

	if (xmms_ipc_msg_get_string_alloc (msg, &n, &l)) {
		g_mutex_lock (entry->mutex);
		if (!g_hash_table_remove (entry->methods, n)) {
			xmms_error_set (err, XMMS_ERROR_INVAL, "Invalid method name!");
			ret = TRUE;
		} else
			ret = --(entry->count) == 0 ? FALSE : TRUE;
		g_mutex_unlock (entry->mutex);
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

static xmms_service_entry_t *
xmms_service_entry_new (gchar *name, gchar *description, guint major,
                        guint minor, guint client)
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
	e->methods = g_hash_table_new_full (g_str_hash, g_str_equal, g_free,
	                                    xmms_service_method_destroy);

	return e;
}

static xmms_service_method_t *
xmms_service_method_new (gchar *name, gchar *description, gchar *ret_type,
                         guint num_args, gchar *args)
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
	m->ret_type = ret_type;
	m->num_args = num_args;
	m->args = args;

	return m;
}

/** @} */
