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

#ifndef __XMMSV_H__
#define __XMMSV_H__

#include "xmmsc/xmmsc_stdint.h"
#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_coll.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct xmmsv_St xmmsv_t;

typedef struct xmmsv_list_iter_St xmmsv_list_iter_t;
typedef struct xmmsv_dict_iter_St xmmsv_dict_iter_t;

xmmsv_t *xmmsv_new_none ();
xmmsv_t *xmmsv_new_error (const char *errstr); /* FIXME: err id? */
xmmsv_t *xmmsv_new_int (int32_t i);
xmmsv_t *xmmsv_new_uint (uint32_t u);
xmmsv_t *xmmsv_new_string (const char *s);
xmmsv_t *xmmsv_new_coll (xmmsv_coll_t *coll);
xmmsv_t *xmmsv_new_bin (unsigned char *data, unsigned int len);

xmmsv_t *xmmsv_new_list ();
xmmsv_t *xmmsv_new_dict ();

xmmsv_t *xmmsv_ref (xmmsv_t *val);
void xmmsv_unref (xmmsv_t *val);

xmmsv_type_t xmmsv_get_type (const xmmsv_t *val);

/* legacy aliases */
int xmmsv_is_error (const xmmsv_t *val);
int xmmsv_is_list (const xmmsv_t *val);
int xmmsv_is_dict (const xmmsv_t *val);
const char * xmmsv_get_error_old (const xmmsv_t *val);
xmmsv_t *xmmsv_make_stringlist (char *array[], int num);
xmmsv_t *xmmsv_make_dict (const char *array[]);

typedef void (*xmmsv_list_foreach_func) (xmmsv_t *value, void *user_data);
typedef void (*xmmsv_dict_foreach_func) (const char *key, xmmsv_t *value, void *user_data);

/* legacy transitional utilities */
xmmsv_type_t xmmsv_get_dict_entry_type (xmmsv_t *val, const char *key);
int xmmsv_get_dict_entry_string (xmmsv_t *val, const char *key, const char **r);
int xmmsv_get_dict_entry_int (xmmsv_t *val, const char *key, int32_t *r);
int xmmsv_get_dict_entry_uint (xmmsv_t *val, const char *key, uint32_t *r);
int xmmsv_get_dict_entry_collection (xmmsv_t *val, const char *key, xmmsv_coll_t **coll);
xmmsv_t *xmmsv_propdict_to_dict (xmmsv_t *propdict, const char **src_prefs);

int xmmsv_get_error (const xmmsv_t *val, const char **r);
int xmmsv_get_int (const xmmsv_t *val, int32_t *r);
int xmmsv_get_uint (const xmmsv_t *val, uint32_t *r);
int xmmsv_get_string (const xmmsv_t *val, const char **r);
int xmmsv_get_collection (const xmmsv_t *val, xmmsv_coll_t **coll);
int xmmsv_get_bin (const xmmsv_t *val, const unsigned char **r, unsigned int *rlen);

int xmmsv_get_list_iter (const xmmsv_t *val, xmmsv_list_iter_t **it);
int xmmsv_get_dict_iter (const xmmsv_t *val, xmmsv_dict_iter_t **it);

/* List */
int xmmsv_list_get (xmmsv_t *listv, int pos, xmmsv_t **val);
int xmmsv_list_append (xmmsv_t *listv, xmmsv_t *val);
int xmmsv_list_insert (xmmsv_t *listv, int pos, xmmsv_t *val);
int xmmsv_list_remove (xmmsv_t *listv, int pos);
int xmmsv_list_clear (xmmsv_t *listv);
int xmmsv_list_foreach (xmmsv_t *listv, xmmsv_list_foreach_func func, void* user_data);
int xmmsv_list_get_size (xmmsv_t *listv);

int  xmmsv_list_iter_entry (xmmsv_list_iter_t *it, xmmsv_t **val);
int  xmmsv_list_iter_valid (xmmsv_list_iter_t *it);
void xmmsv_list_iter_first (xmmsv_list_iter_t *it);
void xmmsv_list_iter_next (xmmsv_list_iter_t *it);
int  xmmsv_list_iter_goto (xmmsv_list_iter_t *it, int pos);

int  xmmsv_list_iter_insert (xmmsv_list_iter_t *it, xmmsv_t *val);
int  xmmsv_list_iter_remove (xmmsv_list_iter_t *it);

/* Dict */
int xmmsv_dict_get (xmmsv_t *dictv, const char *key, xmmsv_t **val);
int xmmsv_dict_insert (xmmsv_t *dictv, const char *key, xmmsv_t *val);
int xmmsv_dict_remove (xmmsv_t *dictv, const char *key);
int xmmsv_dict_clear (xmmsv_t *dictv);
int xmmsv_dict_foreach (xmmsv_t *dictv, xmmsv_dict_foreach_func func, void *user_data);

int  xmmsv_dict_iter_pair (xmmsv_dict_iter_t *it, const char **key, xmmsv_t **val);
int  xmmsv_dict_iter_valid (xmmsv_dict_iter_t *it);
void xmmsv_dict_iter_first (xmmsv_dict_iter_t *it);
void xmmsv_dict_iter_next (xmmsv_dict_iter_t *it);
int  xmmsv_dict_iter_seek (xmmsv_dict_iter_t *it, const char *key);

int  xmmsv_dict_iter_set (xmmsv_dict_iter_t *it, xmmsv_t *val);
int  xmmsv_dict_iter_remove (xmmsv_dict_iter_t *it);

/* Utils */

#define xmmsv_check_type(type) \
        ((type) > XMMSV_TYPE_NONE && \
         (type) < XMMSV_TYPE_END)


/* FIXME: utilities:

make_string_list

*/

/* FIXME: move to utils or something! */
xmmsv_t *xmmsv_decode_url (const xmmsv_t *url);

#ifdef __cplusplus
}
#endif

#endif
