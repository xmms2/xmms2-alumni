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

#ifndef __XMMSC_SERVICE_H__
#define __XMMSC_SERVICE_H__

#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_value.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Defines - ASCII Pr0n */
#define XMMSC_SERVICE_PROP_NAME                "name"
#define XMMSC_SERVICE_PROP_DESCRIPTION         "description"
#define XMMSC_SERVICE_PROP_MAJOR               "major"
#define XMMSC_SERVICE_PROP_MINOR               "minor"
#define XMMSC_SERVICE_PROP_METHODS             "methods"
#define XMMSC_SERVICE_METHOD_PROP_NAME         "name"
#define XMMSC_SERVICE_METHOD_PROP_DESCRIPTION  "description"
#define XMMSC_SERVICE_METHOD_PROP_ARGUMENTS    "args"
#define XMMSC_SERVICE_METHOD_PROP_RETURN_TYPE  "rettype"
#define XMMSC_SERVICE_METHOD_ARG_PROP_NAME     "name"
#define XMMSC_SERVICE_METHOD_ARG_PROP_TYPE     "type"
#define XMMSC_SERVICE_METHOD_ARG_PROP_VALUE    "value"

/* Definitions */
typedef struct xmmsc_service_St xmmsc_service_t;

/* Declarations */
int xmmsc_service_method_add (xmmsc_service_t *svc, const char *name,
                              const char *desc, xmms_value_type_t rettype,
                              xmmsc_service_notifier_t func, void *udata, ...);
int xmmsc_service_method_add_full (xmmsc_service_t *svc, const char *name,
                                   const char *desc, xmms_value_type_t rettype,
                                   xmmsc_service_notifier_t func, void *udata,
                                   xmmsc_user_data_free_func_t ufree, ...);
int xmmsc_service_method_add_noarg (xmmsc_service_t *svc, const char *name,
                                    const char *desc, xmms_value_type_t rettype,
                                    xmmsc_service_notifier_t func, void *udata,
                                    xmmsc_user_data_free_func_t ufree);
int xmmsc_service_method_add_arg (xmmsc_service_t *svc, const char *name,
                                  xmms_value_type_t type);

/* OOOOOLD */
/*
void xmmsc_service_method_ref (xmmsc_service_method_t *method);
void xmmsc_service_method_unref (xmmsc_service_method_t *method);
int xmmsc_service_method_attribute_get (xmmsc_service_method_t *method, char **name, char **description);
int xmmsc_service_method_arg_type_add (xmmsc_service_method_t *method, const char *name, xmms_value_type_t type, int32_t optional);
int xmmsc_service_method_ret_type_add (xmmsc_service_method_t *method, const char *name, xmms_value_type_t type, int32_t optional);
int xmmsc_service_method_arg_add (xmmsc_service_method_t *method, const char *name, xmms_value_t *value);
int xmmsc_service_method_arg_attribute_get (xmmsc_service_method_t *method, const char *name, xmms_value_type_t *type, uint32_t *optional, uint32_t *none);
int xmmsc_service_method_ret_attribute_get (xmmsc_service_method_t *method, const char *name, xmms_value_type_t *type, uint32_t *optional, uint32_t *none);
int xmmsc_service_method_arg_remove (xmmsc_service_method_t *method, const char *name);
int xmmsc_service_method_ret_remove (xmmsc_service_method_t *method, const char *name);
const char *xmmsc_service_method_error_get (xmmsc_service_method_t *method);
int xmmsc_service_method_error_set (xmmsc_service_method_t *method, const char *err);
void xmmsc_service_method_error_reset (xmmsc_service_method_t *method);
int xmmsc_service_method_error_isset (xmmsc_service_method_t *method);
*/

#ifdef __cplusplus
}
#endif

#endif
