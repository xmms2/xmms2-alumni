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

#include "xmms/xmms_service.h"

/**
 * Public functions
 */
gboolean xmms_service_init (void);
gboolean xmms_service_handle (xmms_object_t *xmms_service, xmms_ipc_msg_t *msg,
                              xmms_ipc_cmds_t cmdid, xmms_socket_t client,
                              xmms_object_cmd_arg_t *arg);

#endif
