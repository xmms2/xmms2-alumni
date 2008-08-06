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

#ifndef __XMMS_SERVICE2_H__
#define __XMMS_SERVICE2_H__

#include "xmms/xmms_object.h"
#include "xmmsc/xmmsc_value.h"

/* Data Types */
typedef struct xmms_service_registry_St xmms_service_registry_t;
typedef struct xmms_service_entry_St xmms_service_entry_t;

/* FIXME: Dirty Hack */
#define XMMS_SVC_CMD_DEFINE(cmdid, realfunc, argtype0, _rettype, argtype1, argtype2, argtype3, argtype4) static void \
__int_xmms_cmd_##cmdid (xmms_object_t *object, xmms_object_cmd_arg_t *arg) \
{ \
g_return_if_fail (XMMS_IS_OBJECT (object)); \
__XMMS_CMD_INIT_ARG_##argtype1 (0) \
__XMMS_CMD_INIT_ARG_##argtype2 (1) \
__XMMS_CMD_INIT_ARG_##argtype3 (2) \
__XMMS_CMD_INIT_ARG_##argtype4 (3) \
__XMMS_CMD_DO_RETVAL_##_rettype() (realfunc ((argtype0)object __XMMS_CMD_PRINT_ARG_##argtype1(0) __XMMS_CMD_PRINT_ARG_##argtype2(1) __XMMS_CMD_PRINT_ARG_##argtype3(2) __XMMS_CMD_PRINT_ARG_##argtype4(3), (int32_t) arg->values[4], (uint32_t) arg->values[5], &arg->error)); \
} \
xmms_object_cmd_desc_t __int_xmms_cmd_desc_##cmdid = { __int_xmms_cmd_##cmdid, XMMSV_TYPE_##_rettype, {XMMSV_TYPE_##argtype1, XMMSV_TYPE_##argtype2, XMMSV_TYPE_##argtype3, XMMSV_TYPE_##argtype4} }

#endif
