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

#include <glib.h>

#include "xmms/xmms_log.h"
#include "xmmspriv/xmms_ipc.h"
#include "xmmsc/xmmsc_ipc_msg.h"

/**
  * @defgroup Service Service
  * @ingroup XMMSServer
  * @brief Service client functions for XMMS2 Daemon
  * @{
  */

/**
 * A service representation
 */
typedef struct xmms_service_St {
	gchar *name;
	gchar *description;

	xmms_ipc_client_t *sc;

	xmms_object_cmd_arg_type_t ret_type;

	/* Number of arguments */
	guint num_args;
	/* List of types of arguments */
	GList *args;
} xmms_service_t;

static GMutex *service_registry_lock;
static GList *service_registry = NULL;

static GMutex *service_pool_lock;
static GHashTable *service_pool = NULL;

/**
 * Functions
 */
void xmms_service_registry_shutdown();

/**
 * Initialize service client handling
 */
void
xmms_service_init()
{

}

/** @} */
