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
#include <assert.h>

#include "xmmsc/xmmsc_stdbool.h"
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

typedef struct xmmsv_deserializer_St {
	const unsigned char *data;
	unsigned int left;
} xmmsv_deserializer_t;

static void xmmsv_serializer_put_bin (xmmsv_serializer_t *serializer, const unsigned char *data, unsigned int len);
static void xmmsv_serializer_put_uint32 (xmmsv_serializer_t *serializer, uint32_t v);
static void xmmsv_serializer_put_int32 (xmmsv_serializer_t *serializer, int32_t v);
static void xmmsv_serializer_put_string (xmmsv_serializer_t *serializer, const char *str);

static void xmmsv_serializer_put_collection (xmmsv_serializer_t *serializer, xmmsv_coll_t *coll);

static void xmmsv_serializer_put_value (xmmsv_serializer_t *serializer, xmmsv_t *v);

static void xmmsv_serializer_add_list_item (xmmsv_t *value, void *udata);
static void xmmsv_serializer_add_dict_item (const char *key, xmmsv_t *value, void *udata);

static bool xmmsv_deserializer_get_data (xmmsv_deserializer_t *deserializer, void *buf, size_t length);
static bool xmmsv_deserializer_get_uint32 (xmmsv_deserializer_t *deserializer, uint32_t *v);
static bool xmmsv_deserializer_get_int32 (xmmsv_deserializer_t *deserializer, int32_t *v);
static bool xmmsv_deserializer_get_collection (xmmsv_deserializer_t *deserializer, xmmsv_coll_t **coll);
static bool xmmsv_deserializer_get_value (xmmsv_deserializer_t *d, xmmsv_t **value);

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

static bool
xmmsv_deserializer_get_data (xmmsv_deserializer_t *deserializer,
                             void *buf, size_t length)
{
	if (length > deserializer->left)
		return false;

	if (buf) {
		memcpy (buf, deserializer->data, length);
	}

	deserializer->data += length;
	deserializer->left -= length;

	return true;
}

static bool
xmmsv_deserializer_get_uint32 (xmmsv_deserializer_t *deserializer,
                               uint32_t *v)
{
	bool ret;

	ret = xmmsv_deserializer_get_data (deserializer, v, sizeof (*v));

	if (v) {
		*v = ntohl (*v);
	}

	return ret;
}

static bool
xmmsv_deserializer_get_int32 (xmmsv_deserializer_t *deserializer,
                              int32_t *v)
{
	bool ret;

	ret = xmmsv_deserializer_get_data (deserializer, v, sizeof (*v));

	if (v) {
		*v = ntohl (*v);
	}

	return ret;
}

static bool
xmmsv_deserializer_get_string (xmmsv_deserializer_t *deserializer,
                               char **s)
{
	uint32_t length;
	bool ret;

	assert (s);

	ret = xmmsv_deserializer_get_uint32 (deserializer, &length);

	if (ret) {
		if (length > deserializer->left) {
			ret = false;
		} else {
			*s = malloc (length + 1);

			ret = xmmsv_deserializer_get_data (deserializer, *s, length);

			if (ret) {
				(*s)[length] = 0;
			} else {
				free (*s);
				*s = NULL;
			}
		}
	}

	return ret;
}

static bool
xmmsv_deserializer_get_dict (xmmsv_deserializer_t *d, xmmsv_t **dict)
{
	char *key;
	uint32_t length;
	bool ret;

	assert (dict);

	*dict = NULL;

	ret = xmmsv_deserializer_get_uint32 (d, &length);

	if (!ret) {
		goto err;
	}

	*dict = xmmsv_new_dict ();

	while (length--) {
		xmmsv_t *value;

		ret = xmmsv_deserializer_get_string (d, &key);

		if (!ret) {
			goto err;
		}

		ret = xmmsv_deserializer_get_value (d, &value);

		if (!ret) {
			free (key);
			goto err;
		}

		xmmsv_dict_set (*dict, key, value);

		free (key);
		xmmsv_unref (value);
	}

	return true;

err:
	if (*dict) {
		xmmsv_unref (*dict);
		*dict = NULL;
	}

	return false;
}

static bool
xmmsv_deserializer_get_list (xmmsv_deserializer_t *d, xmmsv_t **list)
{
	uint32_t length;
	bool ret;

	assert (list);

	*list = NULL;

	ret = xmmsv_deserializer_get_uint32 (d, &length);

	if (!ret) {
		goto err;
	}

	*list = xmmsv_new_list ();

	while (length--) {
		xmmsv_t *value;

		ret = xmmsv_deserializer_get_value (d, &value);

		if (!ret) {
			goto err;
		}

		xmmsv_list_append (*list, value);
		xmmsv_unref (value);
	}

	return true;

err:
	if (*list) {
		xmmsv_unref (*list);
		*list = NULL;
	}

	return false;
}

static bool
xmmsv_deserializer_get_collection (xmmsv_deserializer_t *deserializer,
                                   xmmsv_coll_t **coll)
{
	uint32_t *idlist = NULL, type, n_items, id, i;
	bool ret;

	assert (coll);

	*coll = NULL;

	ret = xmmsv_deserializer_get_uint32 (deserializer, &type);

	if (!ret) {
		goto err;
	}

	*coll = xmmsv_coll_new (type);

	/* Get the list of attributes */
	ret = xmmsv_deserializer_get_uint32 (deserializer, &n_items);

	if (!ret) {
		goto err;
	}

	for (i = 0; i < n_items; i++) {
		char *key, *value;

		ret = xmmsv_deserializer_get_string (deserializer, &key);

		if (!ret) {
			goto err;
		}

		ret = xmmsv_deserializer_get_string (deserializer, &value);

		if (!ret) {
			free (key);
			goto err;
		}

		xmmsv_coll_attribute_set (*coll, key, value);

		free (key);
		free (value);
	}

	/* Get the idlist */
	ret = xmmsv_deserializer_get_uint32 (deserializer, &n_items);

	if (!ret) {
		goto err;
	}

	idlist = x_new (uint32_t, n_items + 1);

	if (!idlist) {
		goto err;
	}

	for (i = 0; i < n_items; i++) {
		ret = xmmsv_deserializer_get_uint32 (deserializer, &id);

		if (!ret) {
			goto err;
		}

		idlist[i] = id;
	}

	idlist[i] = 0;
	xmmsv_coll_set_idlist (*coll, idlist);
	free (idlist);
	idlist = NULL;

	/* Get the operands */
	ret = xmmsv_deserializer_get_uint32 (deserializer, &n_items);

	if (!ret) {
		goto err;
	}

	for (i = 0; i < n_items; i++) {
		xmmsv_coll_t *operand;
		xmmsv_type_t type;

		ret = xmmsv_deserializer_get_uint32 (deserializer, &type);

		if (!ret || type != XMMSV_TYPE_COLL) {
			goto err;
		}

		ret = xmmsv_deserializer_get_collection (deserializer, &operand);

		if (!ret) {
			goto err;
		}

		xmmsv_coll_add_operand (*coll, operand);
		xmmsv_coll_unref (operand);
	}

	return true;

err:
	if (*coll) {
		xmmsv_coll_unref (*coll);
		*coll = NULL;
	}

	return false;
}

static bool
xmmsv_deserializer_get_bin (xmmsv_deserializer_t *deserializer,
                            xmmsv_t **value)
{
	unsigned char *buf;
	uint32_t length;
	bool ret;

	assert (value);

	*value = NULL;

	ret = xmmsv_deserializer_get_uint32 (deserializer, &length);

	if (ret) {
		if (length > deserializer->left) {
			ret = false;
		} else {
			buf = malloc (length);
			ret = xmmsv_deserializer_get_data (deserializer, buf, length);

			if (ret) {
				*value = xmmsv_new_bin (buf, length);
			}

			free (buf);
		}
	}

	return ret;
}

static bool
xmmsv_deserializer_get_value (xmmsv_deserializer_t *d, xmmsv_t **value)
{
	xmmsv_coll_t *coll;
	char *s;
	uint32_t type;
	int32_t i;
	bool ret;

	assert (value);

	*value = NULL;

	ret = xmmsv_deserializer_get_uint32 (d, &type);

	if (!ret) {
		goto out;
	}

	switch (type) {
		case XMMSV_TYPE_ERROR:
			ret = xmmsv_deserializer_get_string (d, &s);

			if (ret) {
				*value = xmmsv_new_error (s);
				free (s);
			}

			break;
		case XMMSV_TYPE_INT32:
			ret = xmmsv_deserializer_get_int32 (d, &i);

			if (ret) {
				*value = xmmsv_new_int (i);
			}

			break;
		case XMMSV_TYPE_STRING:
			ret = xmmsv_deserializer_get_string (d, &s);

			if (ret) {
				*value = xmmsv_new_string (s);
				free (s);
			}

			break;
		case XMMSV_TYPE_DICT:
			ret = xmmsv_deserializer_get_dict (d, value);
			break;
		case XMMSV_TYPE_LIST:
			ret = xmmsv_deserializer_get_list (d, value);
			break;
		case XMMSV_TYPE_COLL:
			ret = xmmsv_deserializer_get_collection (d, &coll);

			if (ret) {
				*value = xmmsv_new_coll (coll);
				xmmsv_coll_unref (coll);
			}

			break;
		case XMMSV_TYPE_BIN:
			ret = xmmsv_deserializer_get_bin (d, value);
			break;
		case XMMSV_TYPE_NONE:
			*value = xmmsv_new_none ();
			break;
		default:
			ret = false;
			break;
	}

out:
	return ret;
}

/**
 * Deserializes an arbitrary xmmsv_t from a binary xmmsv_t.
 */
xmmsv_t *
xmmsv_deserialize (xmmsv_t *bin)
{
	xmmsv_deserializer_t d;
	xmmsv_t *result;
	bool ret = false;
	int r;

	x_return_val_if_fail (bin, NULL);

	r = xmmsv_get_bin (bin, &d.data, &d.left);

	if (r) {
		ret = xmmsv_deserializer_get_value (&d, &result);
	}

	return ret ? result : NULL;
}
