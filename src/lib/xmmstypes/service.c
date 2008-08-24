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

#include <string.h>

#include "xmmsc/xmmsc_value.h"
#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_service.h"
#include "xmmsc/xmmsc_util.h"


static int dict_get_string (xmmsv_t *dict, const char *key, const char **v);
static int dict_get_type (xmmsv_t *dict, const char *key, xmmsv_type_t *type);


/**
 * @defgroup ServiceType ServiceType
 * @ingroup Services
 * @brief The API to be used to work with service value objects.
 *
 * @{
 */


int
xmmsv_service_get_name (xmmsv_t *desc, const char **name)
{
	x_return_val_if_fail (desc, 0);

	return dict_get_string (desc, XMMSC_SERVICE_PROP_NAME, name);
}

int
xmmsv_service_get_description (xmmsv_t *desc, const char **description)
{
	x_return_val_if_fail (desc, 0);

	return dict_get_string (desc, XMMSC_SERVICE_PROP_DESCRIPTION, description);
}

int
xmmsv_service_get_version (xmmsv_t *desc, unsigned int *major,
                           unsigned int *minor)
{
	xmmsv_t *val;
	unsigned int maj, min;

	x_return_val_if_fail (desc, 0);

	if (!xmmsv_dict_get (desc, XMMSC_SERVICE_PROP_MAJOR, &val) ||
	    !xmmsv_get_uint (val, &maj)) {
		return 0;
	}
	if (!xmmsv_dict_get (desc, XMMSC_SERVICE_PROP_MINOR, &val) ||
	    !xmmsv_get_uint (val, &min)) {
		return 0;
	}

	if (major) {
		*major = maj;
	}
	if (minor) {
		*minor = min;
	}

	return 1;
}

int
xmmsv_service_get_method_dict (xmmsv_t *desc, xmmsv_t **methods)
{
	xmmsv_t *meths;

	x_return_val_if_fail (desc, 0);

	if (!xmmsv_dict_get (desc, XMMSC_SERVICE_PROP_METHODS, &meths)) {
		return 0;
	}

	if (methods) {
		*methods = meths;
	}

	return 1;
}

int
xmmsv_service_get_method (xmmsv_t *desc, const char *name, xmmsv_t **method)
{
	xmmsv_t *meths, *m;

	x_return_val_if_fail (desc, 0);
	x_return_val_if_fail (name, 0);

	if (!xmmsv_service_get_method_dict (desc, &meths) ||
	    !xmmsv_dict_get (meths, name, &m)) {
		return 0;
	}

	if (method) {
		*method = m;
	}

	return 1;
}

int
xmmsv_service_method_get_description (xmmsv_t *method, const char **description)
{
	x_return_val_if_fail (method, 0);

	return dict_get_string (method, XMMSC_SERVICE_METHOD_PROP_DESCRIPTION, description);
}

int
xmmsv_service_method_get_return_type (xmmsv_t *method, xmmsv_type_t *rettype)
{
	x_return_val_if_fail (method, 0);

	return dict_get_type (method, XMMSC_SERVICE_METHOD_PROP_RETURN_TYPE, rettype);
}

int
xmmsv_service_method_get_argument_list (xmmsv_t *method, xmmsv_t **args)
{
	xmmsv_t *val;

	x_return_val_if_fail (method, 0);

	if (!xmmsv_dict_get (method, XMMSC_SERVICE_METHOD_PROP_ARGUMENTS, &val)) {
		return 0;
	}

	if (args) {
		*args = val;
	}

	return 1;
}

int
xmmsv_service_method_get_argument (xmmsv_t *method, const char *name,
                                   xmmsv_t **arg)
{
	xmmsv_t *args, *a;
	xmmsv_list_iter_t *argit;
	const char *arg_name;

	x_return_val_if_fail (method, 0);
	x_return_val_if_fail (name, 0);

	if (!xmmsv_service_method_get_argument_list (method, &args) ||
	    !xmmsv_get_list_iter (args, &argit)) {
		return 0;
	}

	while (xmmsv_list_iter_entry (argit, &a)) {
		if (!xmmsv_service_method_argument_get_name (a, &arg_name)) {
			return 0;
		}
		if (strcmp (name, arg_name) == 0) {
			break;
		}
		xmmsv_list_iter_next (argit);
	}

	if (arg) {
		*arg = a;
	}

	return 1;
}


int
xmmsv_service_method_argument_get_name (xmmsv_t *arg, const char **name)
{
	x_return_val_if_fail (arg, 0);

	return dict_get_string (arg, XMMSC_SERVICE_METHOD_ARG_PROP_NAME, name);
}

int
xmmsv_service_method_argument_get_type (xmmsv_t *arg, xmmsv_type_t *type)
{
	x_return_val_if_fail (arg, 0);

	return dict_get_type (arg, XMMSC_SERVICE_METHOD_ARG_PROP_TYPE, type);
}

int
xmmsv_service_method_argument_get_optional (xmmsv_t *arg, int *optional)
{
	xmmsv_t *val;
	int o;

	x_return_val_if_fail (arg, 0);

	if (!xmmsv_dict_get (arg, XMMSC_SERVICE_METHOD_ARG_PROP_OPTIONAL, &val) ||
	    !xmmsv_get_int (val, &o)) {
		return 0;
	}

	if (o) {
		*optional = o;
	}

	return 1;

}

/** @} */

static int
dict_get_string (xmmsv_t *dict, const char *key, const char **v)
{
	xmmsv_t *val;
	const char *buf;

	if (!xmmsv_dict_get (dict, key, &val) || !xmmsv_get_string (val, &buf)) {
		return 0;
	}

	if (v) {
		*v = buf;
	}

	return 1;
}

static int
dict_get_type (xmmsv_t *dict, const char *key, xmmsv_type_t *type)
{
	xmmsv_t *val;
	xmmsv_type_t t;
/* FIXME: type? */

	x_return_val_if_fail (dict, 0);

	if (!xmmsv_dict_get (dict, key, &val) ||
	    !xmmsv_get_uint (val, (uint32_t *) &t)) {
		return 0;
	}

	if (type) {
		*type = t;
	}

	return 1;
}
