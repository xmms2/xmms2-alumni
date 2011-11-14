/*  XMMS2 - X Music Multiplexer System
 *
 *  Copyright (C) 2003-2011 XMMS2 Team
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

#include <xmmsclient/xmmsclient.h>
#include <xmmsc/xmmsc_idnumbers.h>

#include <ruby.h>

#include "rb_serviceclient.h"
#include "rb_xmmsclient.h"
#include "rb_result.h"

#define SC_METHOD_HANDLER_HEADER \
	RbServiceClient *sc = NULL; \
	RbXmmsClient *xmms = NULL; \
	xmmsc_result_t *res; \
\
	Data_Get_Struct (self, RbServiceClient, sc); \
	Data_Get_Struct (sc->xmms, RbXmmsClient, xmms); \
	CHECK_DELETED (xmms);

#define SC_METHOD_HANDLER_FOOTER \
	return TO_XMMS_CLIENT_RESULT (sc->xmms, res);

typedef struct {
	VALUE xmms;

	xmmsc_sc_namespace_t *namespace;
} RbServiceClient;
