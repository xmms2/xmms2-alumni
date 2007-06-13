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

	/* Service client cookie */
	guint sc;

	GMutex *mutex;
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

void xmms_service_handle (xmms_ipc_msg_t *msg, uint32_t cmdid, uint32_t cookie);
static gboolean xmms_service_is_registered (gchar *name);
static gboolean xmms_service_is_method_registered (xmms_service_entry_t *entry,
												   gchar *name);
static void xmms_service_register (xmms_ipc_msg_t *msg, uint32_t cookie);
static void xmms_service_method_register (xmms_ipc_msg_t *msg,
										  xmms_service_entry_t *entry);
static xmms_service_entry_t *xmms_service_entry_new (gchar *name,
													 gchar *description,
													 guint major, guint minor,
													 guint cookie);
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

	ret = xmms_object_new (xmms_service_t, xmms_service_destroy);
	ret->mutex = g_mutex_new ();
	ret->registry = g_hash_table_new_full (g_str_hash, g_str_equal, g_free,
										   xmms_service_registry_destroy);

	xmms_ipc_signal_register (XMMS_OBJECT (ret), XMMS_IPC_SIGNAL_SERVICE);

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
 */
void
xmms_service_handle (xmms_ipc_msg_t *msg, uint32_t cmdid, uint32_t cookie)
{
	g_return_if_fail (msg);

	if (cmdid <= XMMS_IPC_CMD_SERVICE_BEGIN ||
		cmdid >= XMMS_IPC_CMD_SERVICE_END) {
		xmms_log_error ("Invalid service command (%d)", cmdid);
		return;
	}

	switch (cmdid) {
	case XMMS_IPC_CMD_SERVICE_REGISTER:
		xmms_service_register (msg, cookie);
		break;
	case XMMS_IPC_CMD_SERVICE_REQUEST:
		break;
	case XMMS_IPC_CMD_SERVICE_RETURN:
		break;
	case XMMS_IPC_CMD_SERVICE_SHUTDOWN:
		break;
	}
}

/**
 * Register a new service
 */
static void
xmms_service_register (xmms_ipc_msg_t *msg, uint32_t cookie)
{
	gchar *n;
	guint l;
	gchar *desc;
	guint major, minor;

	g_return_if_fail (msg);

	XMMS_DBG ("Registering new service");

	if (!xmms_ipc_msg_get_string_alloc (msg, &n, &l)) {
		xmms_log_error ("No service name in message!");
		return;
	}
	if (!xmms_ipc_msg_get_string_alloc (msg, &desc, &l)) {
		xmms_log_error ("No service description in message!");
		return;
	}
	if (!xmms_ipc_msg_get_uint32 (msg, &major)) {
		xmms_log_error ("No major version in message!");
		return;
	}
	if (!xmms_ipc_msg_get_uint32 (msg, &minor)) {
		xmms_log_error ("No minor version in message!");
		return;
	}

	if (!xmms_service_is_registered (n)) {
		xmms_log_error ("Service already registered!");
		return;
	}
	xmms_service_entry_t *e;
	e = xmms_service_entry_new (n, desc, major, minor, cookie);
	g_mutex_lock (xmms_service->mutex);
	g_hash_table_insert (xmms_service->registry, n, e);
	g_mutex_unlock (xmms_service->mutex);

	xmms_service_method_register (msg, e);
}

/**
 * Register a new method
 */
static void
xmms_service_method_register (xmms_ipc_msg_t *msg, xmms_service_entry_t *entry)
{
	gchar *n;
	guint l;
	gchar *desc;
	gchar *rt, *at;

	g_return_if_fail (msg);

	XMMS_DBG ("Registering new method");

	if (!xmms_ipc_msg_get_string_alloc (msg, &n, &l)) {
		xmms_log_error ("No method name in message!");
		return;
	}
	if (!xmms_ipc_msg_get_string_alloc (msg, &desc, &l)) {
		xmms_log_error ("No method description in message!");
		return;
	}
	if (!xmms_ipc_msg_get_string_alloc (msg, &rt, &l)) {
		xmms_log_error ("No return types in message!");
		return;
	}
	if (!xmms_ipc_msg_get_string_alloc (msg, &at, &l)) {
		xmms_log_error ("No argument types in message!");
		return;
	}

	if (!xmms_service_is_method_registered (entry, n)) {
		xmms_log_error ("Method already registered!");
		return;
	}

	gchar *c;
	for (c = rt; *c; c++) {
		switch (*c) {
		case 'u':
		case 'i':
		case 's':
		case 'l':
		case 'c':
		case 'b':
			break;
		default:
			xmms_log_error ("Invalid return type (%c)!", *c);
			return;
		}
	}
	l = 0;
	for (c = at; *c; c++, l++) {
		switch (*c) {
		case 'u':
		case 'i':
		case 's':
		case 'l':
		case 'c':
		case 'b':
			break;
		default:
			xmms_log_error ("Invalid argument type (%c)!", *c);
			return;
		}
	}

	xmms_service_method_t *m;
	m = xmms_service_method_new (n, desc, rt, l, at);
	g_mutex_lock (entry->mutex);
	g_hash_table_insert (entry->methods, n, m);
	g_mutex_unlock (entry->mutex);
}

static gboolean
xmms_service_is_registered (gchar *name)
{
	gboolean ret = TRUE;

	g_return_val_if_fail (name, FALSE);

	g_mutex_lock (xmms_service->mutex);
	if (!g_hash_table_lookup (xmms_service->registry, name))
		ret = FALSE;
	g_mutex_unlock (xmms_service->mutex);

	return ret;
}

static gboolean
xmms_service_is_method_registered (xmms_service_entry_t *entry, gchar *name)
{
	gboolean ret = TRUE;

	g_return_val_if_fail (entry, FALSE);
	g_return_val_if_fail (name, FALSE);

	g_mutex_lock (entry->mutex);
	if (!g_hash_table_lookup (entry->methods, name))
		ret = FALSE;
	g_mutex_unlock (entry->mutex);

	return ret;
}

static xmms_service_entry_t *
xmms_service_entry_new (gchar *name, gchar *description, guint major,
						guint minor, guint cookie)
{

}

static xmms_service_method_t *
xmms_service_method_new (gchar *name, gchar *description, gchar *ret_type,
						 guint num_args, gchar *args)
{

}

/** @} */
