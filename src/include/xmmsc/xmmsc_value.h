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

typedef struct xmms_value_list_iter_St xmms_value_list_iter_t;
typedef struct xmms_value_dict_iter_St xmms_value_dict_iter_t;

xmms_value_t *xmms_value_new_none ();
xmms_value_t *xmms_value_new_error (const char *errstr); /* FIXME: err id? */
xmms_value_t *xmms_value_new_int (int32_t i);
xmms_value_t *xmms_value_new_uint (uint32_t u);
xmms_value_t *xmms_value_new_string (const char *s);
xmms_value_t *xmms_value_new_coll (xmmsc_coll_t *coll);
xmms_value_t *xmms_value_new_bin (unsigned char *data, unsigned int len);

xmms_value_t *xmms_value_new_list ();
xmms_value_t *xmms_value_new_dict ();

xmms_value_t *xmms_value_ref (xmms_value_t *val);
void xmms_value_unref (xmms_value_t *val);

xmms_value_type_t xmms_value_get_type (xmms_value_t *val);

/* legacy aliases */
int xmms_value_iserror (xmms_value_t *val);
int xmms_value_is_list (xmms_value_t *val);
const char * xmms_value_get_error_old (xmms_value_t *val);

/* legacy transitional utilities */
typedef void (*xmmsc_dict_foreach_func) (const void *key, xmms_value_t *value, void *user_data);

xmms_value_type_t xmms_value_get_dict_entry_type (xmms_value_t *val, const char *key);
int xmms_value_get_dict_entry_string (xmms_value_t *val, const char *key, const char **r);
int xmms_value_get_dict_entry_int (xmms_value_t *val, const char *key, int32_t *r);
int xmms_value_get_dict_entry_uint (xmms_value_t *val, const char *key, uint32_t *r);
int xmms_value_get_dict_entry_collection (xmms_value_t *val, const char *key, xmmsc_coll_t **coll);
int xmms_value_dict_foreach (xmms_value_t *val, xmmsc_dict_foreach_func func, void *user_data);
xmms_value_t *xmms_value_propdict_to_dict (xmms_value_t *propdict, const char **src_prefs);

int xmms_value_get_error (xmms_value_t *val, const char **r);
int xmms_value_get_int (xmms_value_t *val, int32_t *r);
int xmms_value_get_uint (xmms_value_t *val, uint32_t *r);
int xmms_value_get_string (xmms_value_t *val, const char **r);
int xmms_value_get_collection (xmms_value_t *val, xmmsc_coll_t **coll);
int xmms_value_get_bin (xmms_value_t *val, unsigned char **r, unsigned int *rlen);

int xmms_value_get_list_iter (xmms_value_t *val, xmms_value_list_iter_t **it);
int xmms_value_get_dict_iter (xmms_value_t *val, xmms_value_dict_iter_t **it);

int  xmms_value_list_iter_entry (xmms_value_list_iter_t *it, xmms_value_t **val);
int  xmms_value_list_iter_valid (xmms_value_list_iter_t *it);
void xmms_value_list_iter_first (xmms_value_list_iter_t *it);
void xmms_value_list_iter_next (xmms_value_list_iter_t *it);

/* FIXME: return a new iter, or keep 'state' */
int xmms_value_list_iter_insert (xmms_value_list_iter_t *it, xmms_value_t *val);
int xmms_value_list_iter_remove (xmms_value_list_iter_t *it);

int xmms_value_list_iter_append (xmms_value_list_iter_t *it, xmms_value_t *val);


int  xmms_value_dict_iter_pair (xmms_value_dict_iter_t *it, const char **key, xmms_value_t **val);
int  xmms_value_dict_iter_valid (xmms_value_dict_iter_t *it);
void xmms_value_dict_iter_first (xmms_value_dict_iter_t *it);
void xmms_value_dict_iter_next (xmms_value_dict_iter_t *it);
int  xmms_value_dict_iter_seek (xmms_value_dict_iter_t *it, const char *key);

int xmms_value_dict_iter_insert (xmms_value_dict_iter_t *it, const char *key, xmms_value_t *val);
int xmms_value_dict_iter_remove (xmms_value_dict_iter_t *it, const char *key);


/* Utils */

#define xmms_value_check_type(type) \
        ((type) > XMMS_VALUE_TYPE_NONE && \
         (type) < XMMS_VALUE_TYPE_END)


/* FIXME: utilities:

make_string_list
prepend
goto N
foreach

*/

/* FIXME: move to utils or something! */
char *xmms_value_decode_url (const char *string);


/* Obsolete: 

typedef void (*xmmsc_propdict_foreach_func) (const void *key, xmms_value_type_t type, const void *value, const char *source, void *user_data);
typedef void (*xmmsc_dict_foreach_func) (const void *key, xmms_value_type_t type, const void *value, void *user_data);

void xmms_value_seterror (xmms_value_t *val, const char *errstr);


xmms_value_type_t xmms_value_get_dict_entry_type (xmms_value_t *val, const char *key);
int xmms_value_get_dict_entry_string (xmms_value_t *val, const char *key, const char **r);
int xmms_value_get_dict_entry_int (xmms_value_t *val, const char *key, int32_t *r);
int xmms_value_get_dict_entry_uint (xmms_value_t *val, const char *key, uint32_t *r);
int xmms_value_get_dict_entry_collection (xmms_value_t *val, const char *key, xmmsc_coll_t **coll);
int xmms_value_dict_foreach (xmms_value_t *val, xmmsc_dict_foreach_func func, void *user_data);
int xmms_value_propdict_foreach (xmms_value_t *val, xmmsc_propdict_foreach_func func, void *user_data);
void xmms_value_source_preference_set (xmms_value_t *val, const char **preference);
const char **xmms_value_source_preference_get (xmms_value_t *val);

int xmms_value_list_next (xmms_value_t *val);
int xmms_value_list_first (xmms_value_t *val);
int xmms_value_list_valid (xmms_value_t *val);

*/

/* anders' EG:

for (i = value_iter_get(value); value_iter_inside (i);
	 value_iter_next (i)) { xmms_value_t *subval = iter_get(i); }

i = value_iter_get (value); iter_goto_key(i, "apan"); iter_set(i, subvalue);
*/


#ifdef __cplusplus
}
#endif

#endif 
