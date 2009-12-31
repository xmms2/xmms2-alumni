/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2009 XMMS2 Team
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

#include <stdarg.h>
#include <string.h>
#include <stdlib.h>

#include "xmmsc/xmmsc_sockets.h"
#include "xmmsc/xmmsc_stdint.h"
#include "xmmsc/xmmsc_util.h"
#include "xmmsc/xmmsv.h"
#include "xmmsc/xmmsv_coll.h"

#define ALLOC_CHUNK_SIZE 64 /* must be a power of two */

typedef struct xmmsv_serializer_St {
	unsigned char *data;
	uint32_t allocated;
	uint32_t length;
} xmmsv_serializer_t;

static void xmmsv_serializer_put_bin (xmmsv_serializer_t *serializer, const unsigned char *data, unsigned int len);
static void xmmsv_serializer_put_uint32 (xmmsv_serializer_t *serializer, uint32_t v);
static void xmmsv_serializer_put_int32 (xmmsv_serializer_t *serializer, int32_t v);
static void xmmsv_serializer_put_string (xmmsv_serializer_t *serializer, const char *str);

static void xmmsv_serializer_put_collection (xmmsv_serializer_t *serializer, xmmsv_coll_t *coll);

static void xmmsv_serializer_put_value (xmmsv_serializer_t *serializer, xmmsv_t *v);

static void xmmsv_serializer_add_list_item (xmmsv_t *value, void *udata);
static void xmmsv_serializer_add_dict_item (const char *key, xmmsv_t *value, void *udata);

static void
xmmsv_serializer_put_data (xmmsv_serializer_t *serializer,
                           const void *data, unsigned int len)
{
	uint32_t needed_length;

	x_return_if_fail (serializer);

	needed_length = serializer->length + len;

	/* Do we need to allocate more memory? */
	if (needed_length > serializer->allocated) {
		int align = ALLOC_CHUNK_SIZE - 1;

		serializer->allocated = (needed_length + align) & ~align;

		/* Realloc data portion */
		serializer->data = realloc (serializer->data,
		                            serializer->allocated);
	}

	memcpy (&serializer->data[serializer->length], data, len);
	serializer->length += len;
}

static void
xmmsv_serializer_put_bin (xmmsv_serializer_t *serializer,
                          const unsigned char *data,
                          unsigned int len)
{
	xmmsv_serializer_put_uint32 (serializer, len);

	xmmsv_serializer_put_data (serializer, data, len);
}

static void
xmmsv_serializer_put_uint32 (xmmsv_serializer_t *serializer, uint32_t v)
{
	v = htonl (v);

	return xmmsv_serializer_put_data (serializer, &v, sizeof (v));
}

static void
xmmsv_serializer_put_int32 (xmmsv_serializer_t *serializer, int32_t v)
{
	v = htonl (v);

	xmmsv_serializer_put_data (serializer, &v, sizeof (v));
}

static void
xmmsv_serializer_put_string (xmmsv_serializer_t *serializer, const char *str)
{
	size_t len;

	if (!str) {
		return xmmsv_serializer_put_uint32 (serializer, 0);
	}

	len = strlen (str);

	xmmsv_serializer_put_uint32 (serializer, len + 1);
	xmmsv_serializer_put_data (serializer, str, len + 1);
}

static void
xmmsv_serializer_put_coll_attr (const char *key, xmmsv_t *value, void *udata)
{
	xmmsv_serializer_t *serializer = udata;
	const char *s;
	int r;

	r = xmmsv_get_string (value, &s);
	x_return_if_fail (r);

	xmmsv_serializer_put_string (serializer, key);
	xmmsv_serializer_put_string (serializer, s);
}

static void
xmmsv_serializer_put_collection (xmmsv_serializer_t *serializer,
                                 xmmsv_coll_t *coll)
{
	xmmsv_list_iter_t *it;
	xmmsv_t *v, *attrs;
	int n;
	uint32_t *idlist;

	x_return_if_fail (serializer);
	x_return_if_fail (coll);

	/* push type */
	xmmsv_serializer_put_uint32 (serializer, xmmsv_coll_get_type (coll));

	/* attribute counter and values */
	attrs = xmmsv_coll_attributes_get (coll);

	n = xmmsv_dict_get_size (attrs);
	xmmsv_serializer_put_uint32 (serializer, n);

	xmmsv_dict_foreach (attrs, xmmsv_serializer_put_coll_attr, serializer);

	attrs = NULL; /* no unref needed. */

	/* idlist counter and content */
	idlist = xmmsv_coll_get_idlist (coll);
	for (n = 0; idlist[n] != 0; n++) { }

	xmmsv_serializer_put_uint32 (serializer, n);
	for (n = 0; idlist[n] != 0; n++) {
		xmmsv_serializer_put_uint32 (serializer, idlist[n]);
	}

	/* operands counter and objects */
	n = 0;
	if (xmmsv_coll_get_type (coll) != XMMS_COLLECTION_TYPE_REFERENCE) {
		n = xmmsv_list_get_size (xmmsv_coll_operands_get (coll));
	}

	xmmsv_serializer_put_uint32 (serializer, n);

	if (n > 0) {
		xmmsv_get_list_iter (xmmsv_coll_operands_get (coll), &it);

		while (xmmsv_list_iter_entry (it, &v)) {
			xmmsv_serializer_put_value (serializer, v);

			xmmsv_list_iter_next (it);
		}
	}
}

static void
xmmsv_serializer_add_dict_item (const char *key, xmmsv_t *value,
                                void *udata)
{
	xmmsv_serializer_t *s = udata;

	xmmsv_serializer_put_string (s, key);
	xmmsv_serializer_put_value (s, value);
}

static void
xmmsv_serializer_put_dict (xmmsv_serializer_t *serializer, xmmsv_t *v)
{
	xmmsv_serializer_put_uint32 (serializer, xmmsv_dict_get_size (v));

	xmmsv_dict_foreach (v, xmmsv_serializer_add_dict_item,
			            serializer);
}

static void
xmmsv_serializer_add_list_item (xmmsv_t *value, void *udata)
{
	xmmsv_serializer_t *s = udata;

	xmmsv_serializer_put_value (s, value);
}

static void
xmmsv_serializer_put_list (xmmsv_serializer_t *serializer, xmmsv_t *v)
{
	xmmsv_serializer_put_uint32 (serializer, xmmsv_list_get_size (v));

	xmmsv_list_foreach (v, xmmsv_serializer_add_list_item,
	                    serializer);
}

static void
xmmsv_serializer_put_value (xmmsv_serializer_t *serializer, xmmsv_t *v)
{
	int32_t i;
	const char *s;
	xmmsv_coll_t *c;
	const unsigned char *bc;
	unsigned int bl;
	xmmsv_type_t type;
	int r;

	type = xmmsv_get_type (v);

	xmmsv_serializer_put_uint32 (serializer, type);

	switch (type) {
		case XMMSV_TYPE_ERROR:
			r = xmmsv_get_error (v, &s);

			if (r) {
				xmmsv_serializer_put_string (serializer, s);
			} else {
				/* FIXME */
			}

			break;

		case XMMSV_TYPE_INT32:
			r = xmmsv_get_int (v, &i);

			if (r) {
				xmmsv_serializer_put_int32 (serializer, i);
			} else {
				/* FIXME */
			}

			break;

		case XMMSV_TYPE_STRING:
			r = xmmsv_get_string (v, &s);

			if (r) {
				xmmsv_serializer_put_string (serializer, s);
			} else {
				/* FIXME */
			}

			break;

		case XMMSV_TYPE_DICT:
			xmmsv_serializer_put_dict (serializer, v);
			break;

		case XMMSV_TYPE_LIST:
			xmmsv_serializer_put_list (serializer, v);
			break;

		case XMMSV_TYPE_COLL:
			r = xmmsv_get_coll (v, &c);

			if (r) {
				xmmsv_serializer_put_collection (serializer, c);
			} else {
				/* FIXME */
			}

			break;

		case XMMSV_TYPE_BIN:
			r = xmmsv_get_bin (v, &bc, &bl);

			if (r) {
				xmmsv_serializer_put_bin (serializer, bc, bl);
			} else {
				/* FIXME */
			}

			break;

		case XMMSV_TYPE_NONE:
			/* empty */
			break;

		default:
			/* x_internal_error ("Tried to serialize value of unsupported type"); */
			/* FIXME */
			break;
	}
}

/**
 * Serializes an arbitrary xmmsv_t to a binary xmmsv_t.
 */
xmmsv_t *
xmmsv_serialize (xmmsv_t *v)
{
	xmmsv_serializer_t s;
	xmmsv_t *result;

	x_return_val_if_fail (v, NULL);

	s.length = 0;
	s.allocated = 0;
	s.data = NULL;

	xmmsv_serializer_put_value (&s, v);

	result = xmmsv_new_bin (s.data, s.length);

	free (s.data);

	return result;
}
