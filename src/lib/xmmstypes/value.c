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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "xmmsc/xmmsc_value.h"
#include "xmmsclient/xmmsclient.h"
#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_strlist.h"
#include "xmmsc/xmmsc_errorcodes.h"
#include "xmmspriv/xmms_list.h"


typedef struct xmms_value_list_St xmms_value_list_t;
typedef struct xmms_value_dict_St xmms_value_dict_t;


typedef struct xmms_value_bin_St {
	unsigned char *data;
	uint32_t len;
} xmms_value_bin_t;

struct xmms_value_list_St {
	xmms_value_t **list;
	size_t size;
	size_t allocated;
	x_list_t *iterators;
};

static xmms_value_list_t *xmms_value_list_new ();
static void xmms_value_list_free (xmms_value_list_t *l);
static int xmms_value_list_insert (xmms_value_list_t *l, unsigned int index, xmms_value_t *val);
static int xmms_value_list_remove (xmms_value_list_t *l, unsigned int index);
static int xmms_value_list_resize (xmms_value_list_t *l, size_t newsize);

static xmms_value_dict_t *xmms_value_dict_new ();
static void xmms_value_dict_free (xmms_value_dict_t *dict);


struct xmms_value_list_iter_St {
	xmms_value_list_t *parent;
	unsigned int position;
};

static xmms_value_list_iter_t *xmms_value_list_iter_new (xmms_value_list_t *l);
static void xmms_value_list_iter_free (xmms_value_list_iter_t *it);


static xmms_value_dict_iter_t *xmms_value_dict_iter_new (xmms_value_dict_t *d);
static void xmms_value_dict_iter_free (xmms_value_dict_iter_t *it);



struct xmms_value_St {
	union {
		void *generic;
		char *error;
		uint32_t uint32;
		int32_t int32;
		char *string;
		xmmsc_coll_t *coll;
		xmms_value_bin_t bin;
		xmms_value_list_t *list;
		xmms_value_dict_t *dict;
	} value;
	xmms_value_type_t type;

	int ref;  /* refcounting */
};


static xmms_value_t *xmms_value_new (xmms_value_type_t type);
static void xmms_value_free (xmms_value_t *val);
static void value_data_free (xmms_value_t *val);



/**
 * @defgroup ValueType ValueType
 * @ingroup Values
 * @brief The API to be used to work with value objects.
 *
 * @{
 */

/**
 * Allocates a new error #xmms_value_t.
 * @param s The error message to store in the #xmms_value_t. The
 * string is copied in the value.
 * @return The new #xmms_value_t. Must be unreferenced with
 * #xmms_value_unref.
 */
xmms_value_t *
xmms_value_new_error (const char *errstr)
{
	xmms_value_t *val = xmms_value_new (XMMS_VALUE_TYPE_ERROR);

	if (val) {
		val->value.error = strdup (errstr);
	}

	return val;
}

/**
 * Allocates a new integer #xmms_value_t.
 * @param i The value to store in the #xmms_value_t.
 * @return The new #xmms_value_t. Must be unreferenced with
 * #xmms_value_unref.
 */
xmms_value_t *
xmms_value_new_int (int32_t i)
{
	xmms_value_t *val = xmms_value_new (XMMS_VALUE_TYPE_INT32);

	if (val) {
		val->value.int32 = i;
	}

	return val;
}

/**
 * Allocates a new unsigned integer #xmms_value_t.
 * @param u The value to store in the #xmms_value_t.
 * @return The new #xmms_value_t. Must be unreferenced with
 * #xmms_value_unref.
 */
xmms_value_t *
xmms_value_new_uint (uint32_t u)
{
	xmms_value_t *val = xmms_value_new (XMMS_VALUE_TYPE_UINT32);

	if (val) {
		val->value.uint32 = u;
	}

	return val;
}

/**
 * Allocates a new string #xmms_value_t.
 * @param s The value to store in the #xmms_value_t. The string is
 * copied in the value.
 * @return The new #xmms_value_t. Must be unreferenced with
 * #xmms_value_unref.
 */
xmms_value_t *
xmms_value_new_string (const char *s)
{
	xmms_value_t *val;

	x_return_val_if_fail (s, NULL);

	val = xmms_value_new (XMMS_VALUE_TYPE_STRING);
	if (val) {
		val->value.string = strdup (s);
	}

	return val;
}

/**
 * Allocates a new collection #xmms_value_t.
 * @param s The value to store in the #xmms_value_t.
 * @return The new #xmms_value_t. Must be unreferenced with
 * #xmms_value_unref.
 */
xmms_value_t *
xmms_value_new_coll (xmmsc_coll_t *c)
{
	xmms_value_t *val;

	x_return_val_if_fail (c, NULL);

	val = xmms_value_new (XMMS_VALUE_TYPE_COLL);
	if (val) {
		val->value.coll = c;
		xmmsc_coll_ref (c);
	}

	return val;
}

/**
 * Allocates a new binary data #xmms_value_t.
 * @param data The data to store in the #xmms_value_t.
 * @param len The size of the data.
 * @return The new #xmms_value_t. Must be unreferenced with
 * #xmms_value_unref.
 */
xmms_value_t *
xmms_value_new_bin (unsigned char *data, unsigned int len)
{
	xmms_value_t *val = xmms_value_new (XMMS_VALUE_TYPE_BIN);

	if (val) {
		/* FIXME: data is copied? it will be freed!*/
		val->value.bin.data = data;
		val->value.bin.len = len;
	}

	return val;
}

/**
 * Allocates a new list #xmms_value_t.
 * @return The new #xmms_value_t. Must be unreferenced with
 * #xmms_value_unref.
 */
xmms_value_t *
xmms_value_new_list ()
{
	xmms_value_t *val = xmms_value_new (XMMS_VALUE_TYPE_LIST);

	if (val) {
		val->value.list = xmms_value_list_new ();
	}

	return val;
}

/**
 * Allocates a new dict #xmms_value_t.
 * @return The new #xmms_value_t. Must be unreferenced with
 * #xmms_value_unref.
 */
xmms_value_t *
xmms_value_new_dict ()
{
	xmms_value_t *val = xmms_value_new (XMMS_VALUE_TYPE_DICT);

	if (val) {
		val->value.dict = xmms_value_dict_new ();
	}

	return val;
}



/**
 * References the #xmms_value_t
 *
 * @param val the value to reference.
 * @return val
 */
xmms_value_t *
xmms_value_ref (xmms_value_t *val)
{
	x_return_val_if_fail (val, NULL);
	val->ref++;

	return val;
}

/**
 * Decreases the references for the #xmms_value_t
 * When the number of references reaches 0 it will
 * be freed. And thus all data you extracted from it
 * will be deallocated.
 */
void
xmms_value_unref (xmms_value_t *val)
{
	x_return_if_fail (val);
	x_api_error_if (val->ref < 1, "with a freed value",);

	val->ref--;
	if (val->ref == 0) {
		xmms_value_free (val);
	}
}


/**
 * Allocates new #xmms_value_t and references it.
 * @internal
 */
static xmms_value_t *
xmms_value_new (xmms_value_type_t type)
{
	xmms_value_t *val;

	val = x_new0 (xmms_value_t, 1);
	if (!val) {
		x_oom ();
		return NULL;
	}

	val->type = type;

	xmms_value_ref (val);
	return val;
}

static void
xmms_value_free (xmms_value_t *val)
{
	x_return_if_fail (val);

	value_data_free (val);

	free (val);
}


static void
value_data_free (xmms_value_t *val)
{
	x_return_if_fail (val);

	switch (val->type) {
		case XMMS_VALUE_TYPE_NONE :
		case XMMS_VALUE_TYPE_END :
		case XMMS_VALUE_TYPE_UINT32 :
		case XMMS_VALUE_TYPE_INT32 :
			break;
		case XMMS_VALUE_TYPE_ERROR :
			free (val->value.error);
			val->value.error = NULL;
			break;
		case XMMS_VALUE_TYPE_STRING :
			free (val->value.string);
			val->value.string = NULL;
			break;
		case XMMS_VALUE_TYPE_COLL:
			xmmsc_coll_unref (val->value.coll);
			val->value.coll = NULL;
			break;
		case XMMS_VALUE_TYPE_BIN :
			free (val->value.bin.data);
			val->value.bin.len = 0;
			break;
		case XMMS_VALUE_TYPE_LIST:
			xmms_value_list_free (val->value.list);
			val->value.list = NULL;
			break;
		case XMMS_VALUE_TYPE_DICT:
			xmms_value_dict_free (val->value.dict);
			val->value.dict = NULL;
			break;
		/* FIXME: handle default? */
	}

	val->type = XMMS_VALUE_TYPE_NONE;
}


/**
 * Get the type of the value.
 * @param val a #xmms_value_t to get the type from.
 * @returns The data type in the value.
 */
xmms_value_type_t
xmms_value_get_type (xmms_value_t *val)
{
	x_api_error_if (!val, "NULL value",
	                XMMS_VALUE_TYPE_NONE);

	return val->type;
}


/* Merely legacy aliases */

/**
 * Check the #xmms_value_t for error.
 * @return 1 if error was encountered, else 0
 */
int
xmms_value_iserror (xmms_value_t *val)
{
	return xmms_value_get_type (val) == XMMS_VALUE_TYPE_ERROR;
}

/**
 * Check if the value stores a list.
 *
 * @param val a #xmms_value_t
 * @return 1 if value stores a list, 0 otherwise.
 */
int
xmms_value_is_list (xmms_value_t *val)
{
	return xmms_value_get_type (val) == XMMS_VALUE_TYPE_LIST;
}

const char *
xmms_value_get_error_old (xmms_value_t *val)
{
	if (!val || val->type != XMMS_VALUE_TYPE_ERROR) {
		return NULL;
	}

	return val->value.error;
}


xmms_value_type_t
xmms_value_get_dict_entry_type (xmms_value_t *val, const char *key)
{
	xmms_value_dict_iter_t *it;
	xmms_value_t *v;

	if (!val || !xmms_value_get_dict_iter (val, &it) ||
		!xmms_value_dict_iter_seek (it, key)) {
		return 0;
	}

	xmms_value_dict_iter_pair (it, NULL, &v);

	return xmms_value_get_type (v);
}


/* macro-magically define legacy dict extractors */
#define GEN_COMPAT_DICT_EXTRACTOR_FUNC(typename, type) \
	int \
	xmms_value_get_dict_entry_##typename (xmms_value_t *val, const char *key, \
	                                      type *r) \
	{ \
		xmms_value_dict_iter_t *it; \
		xmms_value_t *v; \
		if (!val || !xmms_value_get_dict_iter (val, &it) || \
			!xmms_value_dict_iter_seek (it, key)) { \
			return 0; \
		} \
		xmms_value_dict_iter_pair (it, NULL, &v); \
		return xmms_value_get_##typename (v, r); \
	}

GEN_COMPAT_DICT_EXTRACTOR_FUNC(string, const char *)
GEN_COMPAT_DICT_EXTRACTOR_FUNC(int, int32_t)
GEN_COMPAT_DICT_EXTRACTOR_FUNC(uint, uint32_t)
GEN_COMPAT_DICT_EXTRACTOR_FUNC(collection, xmmsc_coll_t *)


int
xmms_value_dict_foreach (xmms_value_t *val, xmmsc_dict_foreach_func func,
                         void *user_data)
{
	xmms_value_dict_iter_t *it;
	const char *key;
	xmms_value_t *v;

	if (!val || !xmms_value_get_dict_iter (val, &it)) {
		return 0;
	}

	while (xmms_value_dict_iter_valid (it)) {
		xmms_value_dict_iter_pair (it, &key, &v);
		func (key, v, user_data);
		xmms_value_dict_iter_next (it);
	}

	return 1;
}


/**
 * Retrieves an error string describing the server error from the
 * value.
 * @param val a #xmms_value_t containing a integer.
 * @param r the return error.
 * @return 1 upon success otherwise 0
 */
int
xmms_value_get_error (xmms_value_t *val, const char **r)
{
	if (!val || val->type != XMMS_VALUE_TYPE_ERROR) {
		return 0;
	}

	*r = val->value.error;

	return 1;
}

/**
 * Retrieves a signed integer from the value.
 * @param val a #xmms_value_t containing a integer.
 * @param r the return integer.
 * @return 1 upon success otherwise 0
 */
int
xmms_value_get_int (xmms_value_t *val, int32_t *r)
{
	if (!val || val->type != XMMS_VALUE_TYPE_INT32) {
		return 0;
	}

	*r = val->value.int32;

	return 1;
}

/**
 * Retrieves a unsigned integer from the resultset.
 * @param val a #xmms_value_t containing a integer.
 * @param r the return unsigned integer.
 * @return 1 upon success otherwise 0
 */
int
xmms_value_get_uint (xmms_value_t *val, uint32_t *r)
{
	if (!val || val->type != XMMS_VALUE_TYPE_UINT32)
		return 0;

	*r = val->value.uint32;

	return 1;
}

/**
 * Retrieves a string from the resultset.
 * @param val a #xmms_value_t containing a string.
 * @param r the return string. This string is owned by the value and
 * will be freed when the value is freed.
 * @return 1 upon success otherwise 0
 */
int
xmms_value_get_string (xmms_value_t *val, const char **r)
{
	if (!val || val->type != XMMS_VALUE_TYPE_STRING) {
		return 0;
	}

	*r = val->value.string;

	return 1;
}

/**
 * Retrieves a collection from the resultset.
 * @param val a #xmms_value_t containing a collection.
 * @param c the return collection. This collection is owned by the
 * value and will be unref'd when the value is freed.
 * @return 1 upon success otherwise 0
 */
int
xmms_value_get_collection (xmms_value_t *val, xmmsc_coll_t **c)
{
	if (!val || val->type != XMMS_VALUE_TYPE_COLL) {
		return 0;
	}

	*c = val->value.coll;

	return 1;
}

/**
 * Retrieves binary data from the resultset.
 * @param val a #xmms_value_t containing a string.
 * @param r the return data. This data is owned by the value and will
 * be freed when the value is freed.
 * @param rlen the return length of data.
 * @return 1 upon success otherwise 0
 */
int
xmms_value_get_bin (xmms_value_t *val, unsigned char **r, unsigned int *rlen)
{
	if (!val || val->type != XMMS_VALUE_TYPE_BIN) {
		return 0;
	}

	*r = val->value.bin.data;
	*rlen = val->value.bin.len;

	return 1;
}



int
xmms_value_get_list_iter (xmms_value_t *val, xmms_value_list_iter_t **it)
{
	xmms_value_list_iter_t *new_it;

	if (!val || val->type != XMMS_VALUE_TYPE_LIST) {
		return 0;
	}

	new_it = xmms_value_list_iter_new (val->value.list);
	if (!new_it) {
		return 0;
	}

	*it = new_it;

	return 1;
}

int
xmms_value_get_dict_iter (xmms_value_t *val, xmms_value_dict_iter_t **it)
{
	xmms_value_dict_iter_t *new_it;

	if (!val || val->type != XMMS_VALUE_TYPE_DICT) {
		return 0;
	}

	new_it = xmms_value_dict_iter_new (val->value.dict);
	if (!new_it) {
		return 0;
	}

	*it = new_it;

	return 1;
}


/* List stuff */

static xmms_value_list_t *
xmms_value_list_new ()
{
	xmms_value_list_t *list;

	list = x_new0 (xmms_value_list_t, 1);
	if (!list) {
		x_oom ();
		return NULL;
	}

	/* list is all empty for now! */

	return list;
}

static void
xmms_value_list_free (xmms_value_list_t *l)
{
	x_list_t *n;
	xmms_value_list_iter_t *it;
	size_t i;

	/* free iterators */
	for (n = l->iterators; n; n = n->next) {
		it = (xmms_value_list_iter_t *) n->data;
		xmms_value_list_iter_free (it);
	}
	x_list_free (l->iterators);

	/* unref contents */
	for (i = 0; i < l->size; i++) {
		xmms_value_unref (l->list[i]);
	}

	free (l->list);
	free (l);
}

static int
xmms_value_list_insert (xmms_value_list_t *l, unsigned int index,
                        xmms_value_t *val)
{
	xmms_value_list_iter_t *it;
	x_list_t *n;
	int i;

	if (index > l->size) {
		return 0;
	}

	/* We need more memory, reallocate */
	if (l->size == l->allocated) {
		int success;
		size_t double_size;
		if (l->allocated > 0) {
			double_size = l->allocated << 1;
		} else {
			double_size = 1;
		}
		success = xmms_value_list_resize (l, double_size);
		x_return_val_if_fail (success, 0);
	}

	for (i = l->size; i > index; i--) {
		l->list[i] = l->list[i - 1];
	}

	l->list[index] = val;
	l->size++;

	xmms_value_ref (val);

	/* update iterators pos */
	for (n = l->iterators; n; n = n->next) {
		it = (xmms_value_list_iter_t *) n->data;
		if (it->position >= index) {
			it->position++;
		}
	}

	return 1;
}

static int
xmms_value_list_remove (xmms_value_list_t *l, unsigned int index)
{
	xmms_value_list_iter_t *it;
	size_t half_size;
	x_list_t *n;
	int i;

	if (index >= l->size - 1) {
		return 0;
	}

	xmms_value_unref (l->list[index]);

	l->size--;
	for (i = index; i < l->size; i++) {
		l->list[i] = l->list[i + 1];
	}

	/* Reduce memory usage by two if possible */
	half_size = l->allocated >> 1;
	if (l->size <= half_size) {
		xmms_value_list_resize (l, half_size);
	}

	/* update iterator pos */
	for (n = l->iterators; n; n = n->next) {
		it = (xmms_value_list_iter_t *) n->data;
		if (it->position > index) {
			it->position--;
		}
	}

	return 1;
}

static int
xmms_value_list_resize (xmms_value_list_t *l, size_t newsize)
{
	xmms_value_t **newmem;

	newmem = realloc (l->list, newsize * sizeof (xmms_value_t *));

	if (newmem == NULL) {
		x_oom ();
		return 0;
	}

	l->list = newmem;
	l->allocated = newsize;

	return 1;
}




static xmms_value_list_iter_t *
xmms_value_list_iter_new (xmms_value_list_t *l)
{
	xmms_value_list_iter_t *it;

	it = x_new0 (xmms_value_list_iter_t, 1);
	if (!it) {
		x_oom ();
		return NULL;
	}

	it->parent = l;
	it->position = 0;

	/* register iterator into parent */
	l->iterators = x_list_prepend (l->iterators, it);

	return it;
}

static void
xmms_value_list_iter_free (xmms_value_list_iter_t *it)
{
	free (it);
}

int
xmms_value_list_iter_entry (xmms_value_list_iter_t *it, xmms_value_t **val)
{
	x_return_val_if_fail (!xmms_value_list_iter_valid (it), 0);

	*val = it->parent->list[it->position];

	return 1;
}

int
xmms_value_list_iter_valid (xmms_value_list_iter_t *it)
{
	x_return_val_if_fail (it, 0);

	return (it->position < it->parent->size);
}

void
xmms_value_list_iter_first (xmms_value_list_iter_t *it)
{
	x_return_if_fail (it);

	it->position = 0;
}

void
xmms_value_list_iter_next (xmms_value_list_iter_t *it)
{
	x_return_if_fail (it);

	if (it->position < it->parent->size) {
		it->position++;
	}
}

int
xmms_value_list_iter_insert (xmms_value_list_iter_t *it, xmms_value_t *val)
{
	x_return_val_if_fail (it, 0);
	x_return_val_if_fail (val, 0);

	return xmms_value_list_insert (it->parent, it->position, val);
}

int
xmms_value_list_iter_remove (xmms_value_list_iter_t *it)
{
	x_return_val_if_fail (it, 0);

	return xmms_value_list_remove (it->parent, it->position);
}

int
xmms_value_list_iter_append (xmms_value_list_iter_t *it, xmms_value_t *val)
{
	x_return_val_if_fail (it, 0);
	x_return_val_if_fail (val, 0);

	return xmms_value_list_insert (it->parent, it->parent->size, val);
}


/* Dict stuff */

struct xmms_value_dict_St {
	/* dict implemented as a flat [key1, val1, key2, val2, ...] list */
	xmms_value_list_t *flatlist;
	x_list_t *iterators;
};

static xmms_value_dict_t *
xmms_value_dict_new ()
{
	xmms_value_dict_t *dict;

	dict = x_new0 (xmms_value_dict_t, 1);
	if (!dict) {
		x_oom ();
		return NULL;
	}

	dict->flatlist = xmms_value_list_new ();

	return dict;
}

static void
xmms_value_dict_free (xmms_value_dict_t *dict)
{
	x_list_t *n;
	xmms_value_dict_iter_t *it;

	/* free iterators */
	for (n = dict->iterators; n; n = n->next) {
		it = (xmms_value_dict_iter_t *) n->data;
		xmms_value_dict_iter_free (it);
	}
	x_list_free (dict->iterators);

	xmms_value_list_free (dict->flatlist);

	free (dict);
}


struct xmms_value_dict_iter_St {
	/* iterator of the contained flatlist */
	xmms_value_list_iter_t *lit;
};

static xmms_value_dict_iter_t *
xmms_value_dict_iter_new (xmms_value_dict_t *d)
{
	xmms_value_dict_iter_t *it;

	it = x_new0 (xmms_value_dict_iter_t, 1);
	if (!it) {
		x_oom ();
		return NULL;
	}

	it->lit = xmms_value_list_iter_new (d->flatlist);

	/* register iterator into parent */
	d->iterators = x_list_prepend (d->iterators, it);

	return it;

}

static void
xmms_value_dict_iter_free (xmms_value_dict_iter_t *it)
{
	free (it);
}

int
xmms_value_dict_iter_pair (xmms_value_dict_iter_t *it, const char **key,
                           xmms_value_t **val)
{
	unsigned int orig;
	xmms_value_t *v;

	if (!xmms_value_dict_iter_valid (it)) {
		return 0;
	}

	/* FIXME: avoid leaking abstraction! */
	orig = it->lit->position;

	if (key) {
		xmms_value_list_iter_entry (it->lit, &v);
		xmms_value_get_string (v, key);
	}

	if (val) {
		xmms_value_list_iter_next (it->lit);
		xmms_value_list_iter_entry (it->lit, val);
	}

	it->lit->position = orig;

	return 1;
}

int
xmms_value_dict_iter_valid (xmms_value_dict_iter_t *it)
{
	return xmms_value_list_iter_valid (it->lit);
}

void
xmms_value_dict_iter_first (xmms_value_dict_iter_t *it)
{
	xmms_value_list_iter_first (it->lit);
}

void
xmms_value_dict_iter_next (xmms_value_dict_iter_t *it)
{
	/* skip a pair */
	xmms_value_list_iter_next (it->lit);
	xmms_value_list_iter_next (it->lit);
}

int
xmms_value_dict_iter_seek (xmms_value_dict_iter_t *it, const char *key)
{
	const char *startkey, *k;
	xmms_value_t *v;

	xmms_value_dict_iter_pair (it, &k, &v);
	startkey = k;

	while (strcmp (k, key) != 0) {
		/* walk the list */
		xmms_value_dict_iter_next (it);
		if (!xmms_value_dict_iter_valid (it)) {
			xmms_value_dict_iter_first (it);
		}
		xmms_value_dict_iter_pair (it, &k, &v);

		/* back to start, nothing found! */
		if (k == startkey) {
			return 0;
		}
	}

	return 1;
}

int
xmms_value_dict_iter_insert (xmms_value_dict_iter_t *it, const char *key,
                             xmms_value_t *val)
{
	unsigned int orig;
	int ret;

	/* FIXME: avoid leaking abstraction! */
	orig = it->lit->position;

	/* if key already present, replace value */
	if (xmms_value_dict_iter_seek (it, key)) {
		xmms_value_list_iter_next (it->lit);
		xmms_value_list_iter_remove (it->lit);
		ret = xmms_value_list_iter_insert (it->lit, val);

	/* else, insert a new key-value pair */
	} else {
		xmms_value_t *keyval;
		keyval = xmms_value_new_string (key);
		ret = xmms_value_list_iter_append (it->lit, keyval);
		if (ret) {
			ret = xmms_value_list_iter_append (it->lit, val);
			if (!ret) {
				/* FIXME: oops, remove previously inserted key */
			}
		}
		xmms_value_unref (keyval);
	}

	it->lit->position = orig;

	return ret;
}

int
xmms_value_dict_iter_remove (xmms_value_dict_iter_t *it, const char *key)
{
	unsigned int orig, size;
	int ret;

	/* FIXME: avoid leaking abstraction! */
	orig = it->lit->position;

	if (xmms_value_dict_iter_seek (it, key)) {
		ret = xmms_value_list_iter_remove (it->lit) &&
		      xmms_value_list_iter_remove (it->lit);
		/* FIXME: cleanup if only the first fails */
	}

	/* make sure the pointer is still in the list */
	size = it->lit->parent->size;
	it->lit->position = (orig <= size ? orig : size);

	return ret;
}








/* static xmms_value_t * */
/* xmms_value_dict_lookup (xmms_value_t *val, const char *key) */
/* { */
/* 	if (val->type == XMMS_VALUE_TYPE_DICT) { */
/* 		return plaindict_lookup (val, key); */
/* 	} */

/* 	return NULL; */
/* } */

/* /\** */
/*  * Retrieve integer associated for specified key in the resultset. */
/*  * */
/*  * If the key doesn't exist in the value the returned integer is */
/*  * undefined. */
/*  * */
/*  * @param val a #xmms_value_t containing dict list. */
/*  * @param key Key that should be retrieved */
/*  * @param r the return int */
/*  * @return 1 upon success otherwise 0 */
/*  * */
/*  *\/ */
/* int */
/* xmms_value_get_dict_entry_int (xmms_value_t *val, const char *key, int32_t *r) */
/* { */
/* 	xmms_value_t *v; */
/* 	if (!val || val->error != XMMS_ERROR_NONE) { */
/* 		*r = -1; */
/* 		return 0; */
/* 	} */

/* 	if (val->type != XMMS_VALUE_TYPE_DICT && */
/* 	    val->type != XMMS_VALUE_TYPE_PROPDICT) { */
/* 		*r = -1; */
/* 		return 0; */
/* 	} */

/* 	v = xmms_value_dict_lookup (val, key); */
/* 	if (v && v->type == XMMS_VALUE_TYPE_INT32) { */
/* 		*r = v->value.int32; */
/* 	} else { */
/* 		*r = -1; */
/* 		return 0; */
/* 	} */

/* 	return 1; */
/* } */

/* /\** */
/*  * Retrieve unsigned integer associated for specified key in the resultset. */
/*  * */
/*  * If the key doesn't exist in the value the returned integer is */
/*  * undefined. */
/*  * */
/*  * @param val a #xmms_value_t containing a hashtable. */
/*  * @param key Key that should be retrieved */
/*  * @param r the return uint */
/*  * @return 1 upon success otherwise 0 */
/*  * */
/*  *\/ */
/* int */
/* xmms_value_get_dict_entry_uint (xmms_value_t *val, const char *key, uint32_t *r) */
/* { */
/* 	xmms_value_t *v; */
/* 	if (!val || val->error != XMMS_ERROR_NONE) { */
/* 		*r = -1; */
/* 		return 0; */
/* 	} */

/* 	if (val->type != XMMS_VALUE_TYPE_DICT && */
/* 	    val->type != XMMS_VALUE_TYPE_PROPDICT) { */
/* 		*r = -1; */
/* 		return 0; */
/* 	} */

/* 	v = xmms_value_dict_lookup (val, key); */
/* 	if (v && v->type == XMMS_VALUE_TYPE_UINT32) { */
/* 		*r = v->value.uint32; */
/* 	} else { */
/* 		*r = -1; */
/* 		return 0; */
/* 	} */

/* 	return 1; */
/* } */

/* /\** */
/*  * Retrieve string associated for specified key in the resultset. */
/*  * */
/*  * If the key doesn't exist in the value the returned string is */
/*  * NULL. The string is owned by the value and will be freed when the */
/*  * value is freed. */
/*  * */
/*  * @param val a #xmms_value_t containing a hashtable. */
/*  * @param key Key that should be retrieved */
/*  * @param r the return string (owned by value) */
/*  * @return 1 upon success otherwise 0 */
/*  * */
/*  *\/ */
/* int */
/* xmms_value_get_dict_entry_string (xmms_value_t *val, */
/*                                    const char *key, const char **r) */
/* { */
/* 	xmms_value_t *v; */
/* 	if (!val || val->error != XMMS_ERROR_NONE) { */
/* 		*r = NULL; */
/* 		return 0; */
/* 	} */

/* 	if (val->type != XMMS_VALUE_TYPE_DICT && */
/* 	    val->type != XMMS_VALUE_TYPE_PROPDICT) { */
/* 		*r = NULL; */
/* 		return 0; */
/* 	} */

/* 	v = xmms_value_dict_lookup (val, key); */
/* 	if (v && v->type == XMMS_VALUE_TYPE_STRING) { */
/* 		*r = v->value.string; */
/* 	} else { */
/* 		*r = NULL; */
/* 		return 0; */
/* 	} */

/* 	return 1; */
/* } */

/* /\** */
/*  * Retrieve collection associated for specified key in the resultset. */
/*  * */
/*  * If the key doesn't exist in the value the returned collection is */
/*  * NULL. The collection is owned by the value and will be freed when the */
/*  * value is freed. */
/*  * */
/*  * @param val a #xmms_value_t containing a hashtable. */
/*  * @param key Key that should be retrieved */
/*  * @param c the return collection (owned by value) */
/*  * @return 1 upon success otherwise 0 */
/*  * */
/*  *\/ */
/* int */
/* xmms_value_get_dict_entry_collection (xmms_value_t *val, const char *key, */
/*                                        xmmsc_coll_t **c) */
/* { */
/* 	xmms_value_t *v; */
/* 	if (!val || val->error != XMMS_ERROR_NONE) { */
/* 		*c = NULL; */
/* 		return 0; */
/* 	} */

/* 	if (val->type != XMMS_VALUE_TYPE_DICT && */
/* 	    val->type != XMMS_VALUE_TYPE_PROPDICT) { */
/* 		*c = NULL; */
/* 		return 0; */
/* 	} */

/* 	v = xmms_value_dict_lookup (val, key); */
/* 	if (v && v->type == XMMS_VALUE_TYPE_COLL) { */
/* 		*c = v->value.coll; */
/* 	} else { */
/* 		*c = NULL; */
/* 		return 0; */
/* 	} */

/* 	return 1; */
/* } */

/* /\** */
/*  * Retrieve type associated for specified key in the resultset. */
/*  * */
/*  * @param val a #xmms_value_t containing a hashtable. */
/*  * @param key Key that should be retrieved */
/*  * @return type of key */
/*  * */
/*  *\/ */
/* xmms_value_type_t */
/* xmms_value_get_dict_entry_type (xmms_value_t *val, const char *key) */
/* { */
/* 	xmms_value_t *v; */
/* 	if (!val || val->error != XMMS_ERROR_NONE) { */
/* 		return XMMS_VALUE_TYPE_NONE; */
/* 	} */

/* 	if (val->type != XMMS_VALUE_TYPE_DICT && */
/* 	    val->type != XMMS_VALUE_TYPE_PROPDICT) { */
/* 		return XMMS_VALUE_TYPE_NONE; */
/* 	} */

/* 	v = xmms_value_dict_lookup (val, key); */
/* 	if (!v) { */
/* 		return XMMS_VALUE_TYPE_NONE; */
/* 	} */

/* 	return v->type; */
/* } */

/* /\** */
/*  * Iterate over all key/value-pair in the resultset. */
/*  * */
/*  * Calls specified function for each key/value-pair in the dictionary. */
/*  * */
/*  * void function (const void *key, #xmms_value_type_t type, const void *value, void *user_data); */
/*  * */
/*  * @param val a #xmms_value_t containing a dict. */
/*  * @param func function that is called for each key/value-pair */
/*  * @param user_data extra data passed to func */
/*  * @return 1 upon success otherwise 0 */
/*  * */
/*  *\/ */
/* int */
/* xmms_value_dict_foreach (xmms_value_t *val, xmmsc_dict_foreach_func func, void *user_data) */
/* { */
/* 	x_list_t *n; */

/* 	if (!val || val->error != XMMS_ERROR_NONE) { */
/* 		return 0; */
/* 	} */

/* 	if (val->type != XMMS_VALUE_TYPE_DICT) { */
/* 		x_print_err ("xmms_value_dict_foreach", "on a source dict!"); */
/* 		return 0; */
/* 	} */

/* 	if (val->type == XMMS_VALUE_TYPE_DICT) { */
/* 		for (n = val->value.dict; n; n = x_list_next (n)) { */
/* 			xmms_value_t *val = NULL; */
/* 			if (n->next) { */
/* 				val = n->next->data; */
/* 			} */
/* 			func ((const void *)n->data, val->type, (void *)val->value.string, user_data); */
/* 			n = x_list_next (n); /\* skip value part *\/ */
/* 		} */
/* 	} */

/* 	return 1; */
/* } */

/* /\** */
/*  * Check if current listnode is inside list boundary. */
/*  * */
/*  * When xmms_value_list_valid returns 1, there is a list entry */
/*  * available for access with xmms_value_get_{type}. */
/*  * */
/*  * @param val a #xmms_value_t that is a list. */
/*  * @return 1 if inside, 0 otherwise */
/*  *\/ */
/* int */
/* xmms_value_list_valid (xmms_value_t *val) */
/* { */
/* 	if (!val || val->error != XMMS_ERROR_NONE) { */
/* 		return 0; */
/* 	} */

/* 	if (!val->islist) { */
/* 		return 0; */
/* 	} */

/* 	return !!val->current; */
/* } */

/* /\** */
/*  * Skip to next entry in list. */
/*  * */
/*  * Advances to next list entry. May advance outside of list, so */
/*  * #xmms_value_list_valid should be used to determine if end of list */
/*  * was reached. */
/*  * */
/*  * @param val a #xmms_value_t that is a list. */
/*  * @return 1 upon succes, 0 otherwise */
/*  *\/ */
/* int */
/* xmms_value_list_next (xmms_value_t *val) */
/* { */
/* 	if (!val || val->error != XMMS_ERROR_NONE) { */
/* 		return 0; */
/* 	} */

/* 	if (!val->islist) { */
/* 		return 0; */
/* 	} */

/* 	if (!val->current) { */
/* 		return 0; */
/* 	} */

/* 	val->current = val->current->next; */

/* 	if (val->current) { */
/* 		xmms_value_t *val2 = val->current->data; */
/* 		val->value.generic = val2->value.generic; */
/* 		val->type = val2->type; */
/* 	} else { */
/* 		val->value.generic = NULL; */
/* 		val->type = XMMS_VALUE_TYPE_NONE; */
/* 	} */

/* 	return 1; */
/* } */

/* /\** */
/*  * Return to first entry in list. */
/*  * */
/*  * @param val a #xmms_value_t that is a list. */
/*  * @return 1 upon succes, 0 otherwise */
/*  *\/ */
/* int */
/* xmms_value_list_first (xmms_value_t *val) */
/* { */
/* 	if (!val || val->error != XMMS_ERROR_NONE) { */
/* 		return 0; */
/* 	} */

/* 	if (!val->islist) { */
/* 		return 0; */
/* 	} */

/* 	val->current = val->list; */

/* 	if (val->current) { */
/* 		xmms_value_t *val = val->current->data; */
/* 		val->value.generic = val->value.generic; */
/* 		val->type = val->type; */
/* 	} else { */
/* 		val->value.generic = NULL; */
/* 		val->type = XMMS_VALUE_TYPE_NONE; */
/* 	} */

/* 	return 1; */
/* } */

/* FIXME: MOVE THIS TO UTILS OR SOMETHING! */
/**
 * Decode an URL-encoded string.
 *
 * Some strings (currently only the url of media) has no known
 * encoding, and must be encoded in an UTF-8 clean way. This is done
 * similar to the url encoding web browsers do. This functions decodes
 * a string encoded in that way. OBSERVE that the decoded string HAS
 * NO KNOWN ENCODING and you cannot display it on screen in a 100%
 * guaranteed correct way (a good heuristic is to try to validate the
 * decoded string as UTF-8, and if it validates assume that it is an
 * UTF-8 encoded string, and otherwise fall back to some other
 * encoding).
 *
 * Do not use this function if you don't understand the
 * implications. The best thing is not to try to display the url at
 * all.
 *
 * Note that the fact that the string has NO KNOWN ENCODING and CAN
 * NOT BE DISPLAYED does not stop you from open the file if it is a
 * local file (if it starts with "file://").
 *
FIXME: er so how do we manually free this stuff? free()?
 *
 * The string returned string will be owned by the value and
 * freed when the value is freed. Or, if the value passed is NULL,
 * the user is responsible for freeing the returned string. However,
 * the user has no way of knowing what allocation routine was used to
 * create the string and thus no way to know which free routine to
 * use. Passing a NULL value is generall frowned upon and we won't
 * offer you tissues and a blanket if you come crying to us with
 * broken code.
 *
 * @param val the #xmms_value_t that the string comes from
 * @param string the url encoded string
 * @return decoded string, owned by the #xmms_value_t
 *
 */
char *
xmms_value_decode_url (const char *string)
{
	int i = 0, j = 0;
	char *url;

	url = strdup (string);
	if (!url) {
		x_oom ();
		return NULL;
	}

	while (url[i]) {
		unsigned char chr = url[i++];

		if (chr == '+') {
			chr = ' ';
		} else if (chr == '%') {
			char ts[3];
			char *t;

			ts[0] = url[i++];
			if (!ts[0])
				goto err;
			ts[1] = url[i++];
			if (!ts[1])
				goto err;
			ts[2] = '\0';

			chr = strtoul (ts, &t, 16);

			if (t != &ts[2])
				goto err;
		}

		url[j++] = chr;
	}

	url[j] = '\0';

	return url;

 err:
	free (url);
	return NULL;
}

/** @} */



/** @internal */

/* static xmms_value_t * */
/* plaindict_lookup (xmms_value_t *val, const char *key) */
/* { */
/* 	x_list_t *n; */

/* 	for (n = val->value.dict; n; n = x_list_next (n)) { */
/* 		const char *k = n->data; */
/* 		if (strcasecmp (k, key) == 0 && n->next) { */
/* 			/\* found right key, return value *\/ */
/* 			return (xmms_value_t*) n->next->data; */
/* 		} else { */
/* 			/\* skip data part of this entry *\/ */
/* 			n = x_list_next (n); */
/* 		} */
/* 	} */

/* 	return NULL; */
/* } */
