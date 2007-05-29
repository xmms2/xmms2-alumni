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




#ifndef __XMMSCLIENT_INT_H__
#define __XMMSCLIENT_INT_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "xmmsc/xmmsc_ipc_msg.h"

#include "xmmspriv/xmms_list.h"
#include "xmmsclientpriv/xmmsclient_ipc.h"
#include "xmmsc/xmmsc_stdint.h"

/**
 * @typedef xmmsc_connection_t
 *
 * Holds all data about the current connection to
 * the XMMS server.
 */

struct xmmsc_connection_St {
	int ref;
	
	xmmsc_ipc_t *ipc;

	x_list_t *callbacks;
	x_list_t *replies;
	char *error;
	int timeout;
	void *data;
	uint32_t cookie;

	char *clientname;
};

xmmsc_result_t *xmmsc_result_new (xmmsc_connection_t *c, xmmsc_result_type_t type, uint32_t cookie);

uint32_t xmmsc_result_cookie_get (xmmsc_result_t *result);

xmmsc_result_t *xmmsc_send_msg_no_arg (xmmsc_connection_t *c, int object, int cmd);
xmmsc_result_t *xmmsc_send_msg (xmmsc_connection_t *c, xmms_ipc_msg_t *msg);
xmmsc_result_t *xmmsc_send_msg_flush (xmmsc_connection_t *c, xmms_ipc_msg_t *msg);
xmmsc_result_t *xmmsc_send_broadcast_msg (xmmsc_connection_t *c, uint32_t signalid);
xmmsc_result_t *xmmsc_send_signal_msg (xmmsc_connection_t *c, uint32_t signalid);
char *_xmmsc_medialib_encode_url (const char *url, int narg, const char **args);
int _xmmsc_medialib_verify_url (const char *url);

void xmmsc_result_restartable (xmmsc_result_t *res, uint32_t signalid);
void xmmsc_result_seterror (xmmsc_result_t *res, const char *errstr);

#endif

