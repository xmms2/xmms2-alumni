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

#ifndef __XMMS_VALUE_H__
#define __XMMS_VALUE_H__

#include "xmmsc/xmmsc_stdint.h"
#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_coll.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct xmms_value_St xmms_value_t;

typedef void (*xmmsc_propdict_foreach_func) (const void *key, xmms_value_type_t type, const void *value, const char *source, void *user_data);
typedef void (*xmmsc_dict_foreach_func) (const void *key, xmms_value_type_t type, const void *value, void *user_data);

xmms_value_t *xmms_value_new ();
xmms_value_t *xmms_value_ref (xmms_value_t *val);
void xmms_value_unref (xmms_value_t *val);

int xmms_value_iserror (xmms_value_t *val);
const char * xmms_value_get_error (xmms_value_t *val);
void xmms_value_seterror (xmms_value_t *val, const char *errstr);

int xmms_value_get_int (xmms_value_t *val, int32_t *r);
int xmms_value_get_uint (xmms_value_t *val, uint32_t *r);
int xmms_value_get_string (xmms_value_t *val, const char **r);
int xmms_value_get_collection (xmms_value_t *val, xmmsc_coll_t **coll);
int xmms_value_get_bin (xmms_value_t *val, unsigned char **r, unsigned int *rlen);

xmms_value_type_t xmms_value_get_dict_entry_type (xmms_value_t *val, const char *key);
int xmms_value_get_dict_entry_string (xmms_value_t *val, const char *key, const char **r);
int xmms_value_get_dict_entry_int (xmms_value_t *val, const char *key, int32_t *r);
int xmms_value_get_dict_entry_uint (xmms_value_t *val, const char *key, uint32_t *r);
int xmms_value_get_dict_entry_collection (xmms_value_t *val, const char *key, xmmsc_coll_t **coll);
int xmms_value_dict_foreach (xmms_value_t *val, xmmsc_dict_foreach_func func, void *user_data);
int xmms_value_propdict_foreach (xmms_value_t *val, xmmsc_propdict_foreach_func func, void *user_data);
void xmms_value_source_preference_set (xmms_value_t *val, const char **preference);
const char **xmms_value_source_preference_get (xmms_value_t *val);

int xmms_value_is_list (xmms_value_t *val);
int xmms_value_list_next (xmms_value_t *val);
int xmms_value_list_first (xmms_value_t *val);
int xmms_value_list_valid (xmms_value_t *val);

xmms_value_type_t xmms_value_get_type (xmms_value_t *val);

void xmms_value_set_int (xmms_value_t *val, int32_t r);
void xmms_value_set_uint (xmms_value_t *val, uint32_t r);
void xmms_value_set_string (xmms_value_t *val, const char *r);
void xmms_value_set_collection (xmms_value_t *val, xmmsc_coll_t *coll);
void xmms_value_set_bin (xmms_value_t *val, unsigned char *r, unsigned int rlen);

/* FIXME: Allah will punish me for this someday
void xmms_value_set_list (xmms_value_t *val, x_list_t *r);
void xmms_value_set_dict (xmms_value_t *val, x_list_t *r);
void xmms_value_set_propdict (xmms_value_t *val, x_list_t *r);
*/
void xmms_value_set_list (xmms_value_t *val, void *r);
void xmms_value_set_dict (xmms_value_t *val, void *r);
void xmms_value_set_propdict (xmms_value_t *val, void *r);

const char *xmms_value_decode_url (xmms_value_t *val, const char *string);


#ifdef __cplusplus
}
#endif

#endif 
