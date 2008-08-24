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

#include "xmmsc/xmmsc_value.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Keys used in service structures, you better use the accessor
 * functions instead of low-level xmmsv_* functions. */
#define XMMSC_SERVICE_CHANGE_TYPE              "type"
#define XMMSC_SERVICE_PROP_NAME                "name"
#define XMMSC_SERVICE_PROP_DESCRIPTION         "description"
#define XMMSC_SERVICE_PROP_MAJOR               "major"
#define XMMSC_SERVICE_PROP_MINOR               "minor"
#define XMMSC_SERVICE_PROP_METHODS             "methods"
#define XMMSC_SERVICE_METHOD_PROP_DESCRIPTION  "description"
#define XMMSC_SERVICE_METHOD_PROP_ARGUMENTS    "args"
#define XMMSC_SERVICE_METHOD_PROP_RETURN_TYPE  "rettype"
#define XMMSC_SERVICE_METHOD_ARG_PROP_NAME     "name"
#define XMMSC_SERVICE_METHOD_ARG_PROP_TYPE     "type"
#define XMMSC_SERVICE_METHOD_ARG_PROP_OPTIONAL "optional"
#define XMMSC_SERVICE_METHOD_ARG_PROP_VALUE    "value"
#define XMMSC_SERVICE_QUERY_PROP_ARGUMENTS     "args"
#define XMMSC_SERVICE_QUERY_PROP_METHOD        "method"
#define XMMSC_SERVICE_QUERY_PROP_ID            "id"
#define XMMSC_SERVICE_QUERY_PROP_RETURN        "return"

int xmmsv_service_get_name (xmmsv_t *desc, const char **name);
int xmmsv_service_get_description (xmmsv_t *desc, const char **description);
int xmmsv_service_get_version (xmmsv_t *desc, unsigned int *major, unsigned int *minor);
int xmmsv_service_get_method_dict (xmmsv_t *desc, xmmsv_t **methods);
int xmmsv_service_get_method (xmmsv_t *desc, const char *name, xmmsv_t **method);

int xmmsv_service_method_get_description (xmmsv_t *method, const char **description);
int xmmsv_service_method_get_return_type (xmmsv_t *method, xmmsv_type_t *rettype);
int xmmsv_service_method_get_argument_list (xmmsv_t *method, xmmsv_t **args);
int xmmsv_service_method_get_argument (xmmsv_t *method, const char *name, xmmsv_t **arg);

int xmmsv_service_method_argument_get_name (xmmsv_t *arg, const char **name);
int xmmsv_service_method_argument_get_type (xmmsv_t *arg, xmmsv_type_t *type);
int xmmsv_service_method_argument_get_optional (xmmsv_t *arg, int *optional);

#ifdef __cplusplus
}
#endif

#endif
