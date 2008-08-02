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

#ifndef __XMMS_SERVICE_H__
#define __XMMS_SERVICE_H__

#include <glib.h>
#include "xmms/xmms_object.h"
#include "xmmsc/xmmsc_ipc_msg.h"

/**
 * Service structure
 *
 * This is the structure that will emit all the service signals.
 * It includes the service registry and client-service pool.
 */
struct xmms_service_registry_St {
	xmms_object_t object;

	GMutex *mutex;
	GTree *services;
	GTree *clients;
};

/**
 * A single service representation
 */
typedef struct xmms_service_entry_St {
	/* The description of the service and all its methods. */
	xmmsv_t *description;

	/* A client should have exclusive access when querying a service client. */
	GMutex *mutex;

	/* Service client fd */
	xmms_socket_t sc;
} xmms_service_entry_t;

/**
 * Public functions
 */
xmms_service_t *xmms_service_init (void);
gboolean xmms_service_handle (xmms_object_t *xmms_service, xmms_ipc_msg_t *msg,
                              xmms_ipc_cmds_t cmdid, xmms_socket_t client,
                              xmms_object_cmd_arg_t *arg);

#endif
