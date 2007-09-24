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

#ifndef __XMMSC_SERVICE_H__
#define __XMMSC_SERVICE_H__

#include "xmmsc/xmmsc_idnumbers.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
	XMMSC_SERVICE_ARG_TYPE_UINT32 = XMMS_OBJECT_CMD_ARG_UINT32,
	XMMSC_SERVICE_ARG_TYPE_INT32 = XMMS_OBJECT_CMD_ARG_INT32,
	XMMSC_SERVICE_ARG_TYPE_STRING = XMMS_OBJECT_CMD_ARG_STRING,
	XMMSC_SERVICE_ARG_TYPE_STRINGLIST = XMMS_OBJECT_CMD_ARG_STRINGLIST,
	XMMSC_SERVICE_ARG_TYPE_COLL = XMMS_OBJECT_CMD_ARG_COLL,
	XMMSC_SERVICE_ARG_TYPE_BIN = XMMS_OBJECT_CMD_ARG_BIN
} xmmsc_service_arg_type_t;

typedef struct xmmsc_service_method_St xmmsc_service_method_t;

void xmmsc_service_method_ref (xmmsc_service_method_t *method);
void xmmsc_service_method_unref (xmmsc_service_method_t *method);
int xmmsc_service_method_attribute_get (xmmsc_service_method_t *method, char **name, char **description);
int xmmsc_service_method_arg_type_add (xmmsc_service_method_t *method, const char *name, xmmsc_service_arg_type_t type, int32_t optional);
int xmmsc_service_method_ret_type_add (xmmsc_service_method_t *method, const char *name, xmmsc_service_arg_type_t type, int32_t optional);
int xmmsc_service_method_arg_add_uint32 (xmmsc_service_method_t *method, const char *name, uint32_t value);
int xmmsc_service_method_arg_add_int32 (xmmsc_service_method_t *method, const char *name, int32_t value);
int xmmsc_service_method_arg_add_string (xmmsc_service_method_t *method, const char *name, char *value);
int xmmsc_service_method_arg_add_stringlist (xmmsc_service_method_t *method, const char *name, char **value);
int xmmsc_service_method_arg_add_coll (xmmsc_service_method_t *method, const char *name, xmmsc_coll_t *value);
int xmmsc_service_method_arg_add_bin (xmmsc_service_method_t *method, const char *name, unsigned char *value, uint32_t len);
int xmmsc_service_method_ret_add_uint32 (xmmsc_service_method_t *method, const char *name, uint32_t value);
int xmmsc_service_method_ret_add_int32 (xmmsc_service_method_t *method, const char *name, int32_t value);
int xmmsc_service_method_ret_add_string (xmmsc_service_method_t *method, const char *name, char *value);
int xmmsc_service_method_ret_add_stringlist (xmmsc_service_method_t *method, const char *name, char **value);
int xmmsc_service_method_ret_add_coll (xmmsc_service_method_t *method, const char *name, xmmsc_coll_t *value);
int xmmsc_service_method_ret_add_bin (xmmsc_service_method_t *method, const char *name, unsigned char *value, uint32_t len);
int xmmsc_service_method_arg_size (xmmsc_service_method_t *method, uint32_t *size);
int xmmsc_service_method_ret_size (xmmsc_service_method_t *method, uint32_t *size);
int xmmsc_service_method_arg_attribute_get (xmmsc_service_method_t *method, const char *name, xmmsc_service_arg_type_t *type, uint32_t *optional, uint32_t *none);
int xmmsc_service_method_ret_attribute_get (xmmsc_service_method_t *method, const char *name, xmmsc_service_arg_type_t *type, uint32_t *optional, uint32_t *none);
int xmmsc_service_method_arg_remove (xmmsc_service_method_t *method, const char *name);
int xmmsc_service_method_ret_remove (xmmsc_service_method_t *method, const char *name);
const char *xmmsc_service_method_error_get (xmmsc_service_method_t *method);
int xmmsc_service_method_error_set (xmmsc_service_method_t *method, const char *err);
void xmmsc_service_method_error_reset (xmmsc_service_method_t *method);
int xmmsc_service_method_error_isset (xmmsc_service_method_t *method);

#ifdef __cplusplus
}
#endif

#endif
