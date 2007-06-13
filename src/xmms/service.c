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

	xmms_object_cmd_arg_type_t ret_type;

	/* Number of arguments */
	guint num_args;
	/* List of types of arguments */
	GList *args;
} xmms_service_method_t;

static xmms_service_t *xmms_service;

/**
 * Functions
 */
static void xmms_service_destroy (xmms_object_t *object);
static void xmms_service_registry_destroy (gpointer value);
static void xmms_service_method_destroy (gpointer value);

void xmms_service_handle (xmms_ipc_msg_t *msg, uint32_t cmdid,
						  uint32_t cookie);
static void xmms_service_register (xmms_ipc_msg_t *msg, uint32_t cookie);

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

	g_list_free (v->args);
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
	g_return_if_fail (msg);

	XMMS_DBG ("Registering new service");
}

/** @} */
