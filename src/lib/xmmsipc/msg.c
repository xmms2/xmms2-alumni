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

#include <stdarg.h>
#include <string.h>
#include <stdlib.h>

#include <errno.h>
#include <time.h>
#include <assert.h>

#include "xmmspriv/xmms_list.h"
#include "xmmsc/xmmsc_ipc_transport.h"
#include "xmmsc/xmmsc_ipc_msg.h"
#include "xmmsc/xmmsc_util.h"
#include "xmmsc/xmmsc_sockets.h"
#include "xmmsc/xmmsc_stdint.h"
#include "xmmsc/xmmsc_coll.h"


typedef union {
	struct {
		uint32_t object;
		uint32_t cmd;
		uint32_t cookie;
		uint32_t length;
		uint8_t data[0];
	} header;
	uint8_t rawdata[0];
} xmms_ipc_msg_data_t;

struct xmms_ipc_msg_St {
	xmms_ipc_msg_data_t *data;
	uint32_t get_pos;
	uint32_t size;
	uint32_t xfered;
};


void
xmms_ipc_append_coll_attr (const char* key, const char* value, void *userdata) {
	xmms_ipc_msg_t *msg = (xmms_ipc_msg_t *)userdata;
	xmms_ipc_msg_put_string (msg, key);
	xmms_ipc_msg_put_string (msg, value);
}

void
xmms_ipc_count_coll_attr (const char* key, const char* value, void *userdata) {
	int *n = (int *)userdata;
	++(*n);
}


xmms_ipc_msg_t *
xmms_ipc_msg_alloc (void)
{
	xmms_ipc_msg_t *msg;

	msg = x_new0 (xmms_ipc_msg_t, 1);
	msg->data = malloc (XMMS_IPC_MSG_DEFAULT_SIZE);
	memset (msg->data, 0, XMMS_IPC_MSG_HEAD_LEN);
	msg->size = XMMS_IPC_MSG_DEFAULT_SIZE;

	return msg;
}

xmms_ipc_msg_t *
xmms_ipc_msg_new (uint32_t object, uint32_t cmd)
{
	xmms_ipc_msg_t *msg;

	msg = xmms_ipc_msg_alloc ();

	xmms_ipc_msg_set_cmd (msg, cmd);
	xmms_ipc_msg_set_object (msg, object);

	return msg;
}

void
xmms_ipc_msg_destroy (xmms_ipc_msg_t *msg)
{
	x_return_if_fail (msg);

	free (msg->data);
	free (msg);
}

void
xmms_ipc_msg_set_length (xmms_ipc_msg_t *msg, uint32_t len)
{
	x_return_if_fail (msg);

	msg->data->header.length = htonl (len);
}

uint32_t
xmms_ipc_msg_get_length (const xmms_ipc_msg_t *msg)
{
	x_return_val_if_fail (msg, 0);

	return ntohl (msg->data->header.length);
}

uint32_t
xmms_ipc_msg_get_object (const xmms_ipc_msg_t *msg)
{
	x_return_val_if_fail (msg, 0);

	return ntohl (msg->data->header.object);
}

void
xmms_ipc_msg_set_object (xmms_ipc_msg_t *msg, uint32_t object)
{
	x_return_if_fail (msg);

	msg->data->header.object = htonl (object);
}

uint32_t
xmms_ipc_msg_get_cmd (const xmms_ipc_msg_t *msg)
{
	x_return_val_if_fail (msg, 0);

	return ntohl (msg->data->header.cmd);
}

void
xmms_ipc_msg_set_cmd (xmms_ipc_msg_t *msg, uint32_t cmd)
{
	x_return_if_fail (msg);

	msg->data->header.cmd = htonl (cmd);
}

void
xmms_ipc_msg_set_cookie (xmms_ipc_msg_t *msg, uint32_t cookie)
{
	msg->data->header.cookie = htonl (cookie);
}

uint32_t
xmms_ipc_msg_get_cookie (const xmms_ipc_msg_t *msg)
{
	x_return_val_if_fail (msg, 0);

	return ntohl (msg->data->header.cookie);
}

/**
 * Try to write message to transport. If full message isn't written
 * the message will keep track of the amount of data written and not
 * write already written data next time.
 *
 * @returns TRUE if full message was written, FALSE otherwise.
 *               disconnected is set if transport was disconnected
 */
bool
xmms_ipc_msg_write_transport (xmms_ipc_msg_t *msg,
                              xmms_ipc_transport_t *transport,
                              bool *disconnected)
{
	char *buf;
	unsigned int ret, len;

	x_return_val_if_fail (msg, false);
	x_return_val_if_fail (msg->data, false);
	x_return_val_if_fail (transport, false);

	len = xmms_ipc_msg_get_length (msg) + XMMS_IPC_MSG_HEAD_LEN;

	x_return_val_if_fail (len > msg->xfered, true);

	buf = (char *) (msg->data->rawdata + msg->xfered);
	ret = xmms_ipc_transport_write (transport, buf, len - msg->xfered);

	if (ret == SOCKET_ERROR) {
		if (xmms_socket_error_recoverable ()) {
			return false;
		}

		if (disconnected) {
			*disconnected = true;
		}

		return false;
	} else if (!ret) {
		if (disconnected) {
			*disconnected = true;
		}
	} else {
		msg->xfered += ret;
	}

	return (len == msg->xfered);
}

/**
 * Try to read message from transport into msg.
 *
 * @returns TRUE if message is fully read.
 */
bool
xmms_ipc_msg_read_transport (xmms_ipc_msg_t *msg,
                             xmms_ipc_transport_t *transport,
                             bool *disconnected)
{
	char *buf;
	unsigned int ret, len;

	x_return_val_if_fail (msg, false);
	x_return_val_if_fail (transport, false);

	while (true) {
		len = XMMS_IPC_MSG_HEAD_LEN;

		if (msg->xfered >= XMMS_IPC_MSG_HEAD_LEN) {
			len += xmms_ipc_msg_get_length (msg);

			if (len > msg->size) {
				void *newbuf;
				newbuf = realloc (msg->data, len);
				if (!newbuf) {
					if (disconnected) {
						*disconnected = true;
					}
					return false;
				}
				msg->size = len;
				msg->data = newbuf;
			}

			if (msg->xfered == len) {
				return true;
			}
		}

		x_return_val_if_fail (msg->xfered < len, false);

		buf = (char *) (msg->data->rawdata + msg->xfered);
		ret = xmms_ipc_transport_read (transport, buf, len - msg->xfered);

		if (ret == SOCKET_ERROR) {
			if (xmms_socket_error_recoverable ()) {
				return false;
			}

			if (disconnected) {
				*disconnected = true;
			}

			return false;
		} else if (ret == 0) {
			if (disconnected) {
				*disconnected = true;
			}

			return false;
		} else {
			msg->xfered += ret;
		}
	}
}

static uint32_t
xmms_ipc_msg_put_data (xmms_ipc_msg_t *msg, const void *data, unsigned int len)
{
	uint32_t total;

	x_return_val_if_fail (msg, -1);

	total = xmms_ipc_msg_get_length (msg) + XMMS_IPC_MSG_HEAD_LEN + len;

	if (total > msg->size) {
		int realloc_size = XMMS_IPC_MSG_DEFAULT_SIZE;

		if (len > XMMS_IPC_MSG_DEFAULT_SIZE) {
			realloc_size = len;
		}

		/* Realloc data portion */
		msg->data = realloc (msg->data, msg->size + realloc_size);
		msg->size += realloc_size;
	}

	total = xmms_ipc_msg_get_length (msg);
	memcpy (&msg->data->header.data[total], data, len);
	xmms_ipc_msg_set_length (msg, total + len);

	/* return the offset that which we placed this value */
	return total;
}

uint32_t
xmms_ipc_msg_put_bin (xmms_ipc_msg_t *msg,
                      const unsigned char *data,
                      unsigned int len)
{
	xmms_ipc_msg_put_uint32 (msg, len);
	return xmms_ipc_msg_put_data (msg, data, len);
}

uint32_t
xmms_ipc_msg_put_uint32 (xmms_ipc_msg_t *msg, uint32_t v)
{
	v = htonl (v);

	return xmms_ipc_msg_put_data (msg, &v, sizeof (v));
}

void
xmms_ipc_msg_store_uint32 (xmms_ipc_msg_t *msg,
                           uint32_t offset, uint32_t v)
{
	v = htonl (v);

	memcpy (&msg->data->header.data[offset], &v, sizeof (v));
}

uint32_t
xmms_ipc_msg_put_int32 (xmms_ipc_msg_t *msg, int32_t v)
{
	v = htonl (v);

	return xmms_ipc_msg_put_data (msg, &v, sizeof (v));
}

uint32_t
xmms_ipc_msg_put_float (xmms_ipc_msg_t *msg, float v)
{
	/** @todo do we need to convert ? */
	return xmms_ipc_msg_put_data (msg, &v, sizeof (v));
}

uint32_t
xmms_ipc_msg_put_string (xmms_ipc_msg_t *msg, const char *str)
{
	if (!msg) {
		return -1;
	}

	if (!str) {
		return xmms_ipc_msg_put_uint32 (msg, 0);
	}

	xmms_ipc_msg_put_uint32 (msg, strlen (str) + 1);

	return xmms_ipc_msg_put_data (msg, str, strlen (str) + 1);
}

uint32_t
xmms_ipc_msg_put_string_list (xmms_ipc_msg_t *msg, const char* strings[])
{
	uint32_t ret;
	int n;

	for (n = 0; strings && strings[n] != NULL; n++) { }
	ret = xmms_ipc_msg_put_uint32 (msg, n);

	for (n = 0; strings && strings[n] != NULL; n++) {
		ret = xmms_ipc_msg_put_string (msg, strings[n]);
	}

	return ret;
}

uint32_t
xmms_ipc_msg_put_collection (xmms_ipc_msg_t *msg, xmmsc_coll_t *coll)
{
	int n;
	uint32_t ret, *idlist;
	xmmsc_coll_t *op;

	if (!msg || !coll) {
		return -1;
	}

	/* save internal status */
	xmmsc_coll_operand_list_save (coll);

	/* push type */
	xmms_ipc_msg_put_uint32 (msg, xmmsc_coll_get_type (coll));

	/* attribute counter and values */
	n = 0;
	xmmsc_coll_attribute_foreach (coll, xmms_ipc_count_coll_attr, &n);
	xmms_ipc_msg_put_uint32 (msg, n);

	xmmsc_coll_attribute_foreach (coll, xmms_ipc_append_coll_attr, msg);

	/* idlist counter and content */
	idlist = xmmsc_coll_get_idlist (coll);
	for (n = 0; idlist[n] != 0; n++) { }

	xmms_ipc_msg_put_uint32 (msg, n);
	for (n = 0; idlist[n] != 0; n++) {
		xmms_ipc_msg_put_uint32 (msg, idlist[n]);
	}

	/* operands counter and objects */
	n = 0;
	if (xmmsc_coll_get_type (coll) != XMMS_COLLECTION_TYPE_REFERENCE) {
		xmmsc_coll_operand_list_first (coll);
		while (xmmsc_coll_operand_list_entry (coll, &op)) {
			n++;
			xmmsc_coll_operand_list_next (coll);
		}
	}

	ret = xmms_ipc_msg_put_uint32 (msg, n);

	if (n > 0) {
		xmmsc_coll_operand_list_first (coll);
		while (xmmsc_coll_operand_list_entry (coll, &op)) {
			ret = xmms_ipc_msg_put_collection (msg, op);
			xmmsc_coll_operand_list_next (coll);
		}
	}

	/* restore internal status */
	xmmsc_coll_operand_list_restore (coll);

	return ret;
}

static bool
xmms_ipc_msg_get_data (xmms_ipc_msg_t *msg, void *buf, unsigned int len)
{
	if (!msg)
		return false;

	if (len > xmms_ipc_msg_get_length (msg) - msg->get_pos)
		return false;

	if (buf) {
		memcpy (buf, &msg->data->header.data[msg->get_pos], len);
	}

	msg->get_pos += len;

	return true;
}

bool
xmms_ipc_msg_get_uint32 (xmms_ipc_msg_t *msg, uint32_t *v)
{
	bool ret;

	ret = xmms_ipc_msg_get_data (msg, v, sizeof (*v));

	if (v) {
		*v = ntohl (*v);
	}

	return ret;
}

bool
xmms_ipc_msg_get_int32 (xmms_ipc_msg_t *msg, int32_t *v)
{
	bool ret;

	ret = xmms_ipc_msg_get_data (msg, v, sizeof (*v));

	if (v) {
		*v = ntohl (*v);
	}

	return ret;
}

bool
xmms_ipc_msg_get_float (xmms_ipc_msg_t *msg, float *v)
{
	/** @todo do we need to convert? */
	return xmms_ipc_msg_get_data (msg, v, sizeof (*v));
}

bool
xmms_ipc_msg_get_string_alloc (xmms_ipc_msg_t *msg, char **buf,
                               unsigned int *len)
{
	char *str;
	unsigned int l;

	if (!xmms_ipc_msg_get_uint32 (msg, &l)) {
		return false;
	}

	if (l > xmms_ipc_msg_get_length (msg) - msg->get_pos)
		return false;

	str = x_malloc (l + 1);
	if (!str) {
		return false;
	}

	if (!xmms_ipc_msg_get_data (msg, str, l)) {
		free (str);
		return false;
	}

	str[l] = '\0';

	*buf = str;
	*len = l;

	return true;
}

bool
xmms_ipc_msg_get_bin_alloc (xmms_ipc_msg_t *msg,
                            unsigned char **buf,
                            unsigned int *len)
{
	unsigned char *b;
	unsigned int l;

	if (!xmms_ipc_msg_get_uint32 (msg, &l)) {
		return false;
	}

	if (l > xmms_ipc_msg_get_length (msg) - msg->get_pos)
		return false;

	b = x_malloc (l);
	if (!b) {
		return false;
	}

	if (!xmms_ipc_msg_get_data (msg, b, l)) {
		free (b);
		return false;
	}

	*buf = b;
	*len = l;

	return true;
}

bool
xmms_ipc_msg_get_string (xmms_ipc_msg_t *msg, char *buf, unsigned int maxlen)
{
	uint32_t len;

	if (buf) {
		buf[maxlen - 1] = '\0';
		maxlen--;
	}

	if (!xmms_ipc_msg_get_uint32 (msg, &len)) {
		return false;
	}

	if (!len) {
		buf[0] = '\0';
		return true;
	}

	if (!xmms_ipc_msg_get_data (msg, buf, MIN (maxlen, len))) {
		return false;
	}

	if (maxlen < len) {
		xmms_ipc_msg_get_data (msg, NULL, len - maxlen);
	}

	return true;
}

bool
xmms_ipc_msg_get_collection_alloc (xmms_ipc_msg_t *msg, xmmsc_coll_t **coll)
{
	unsigned int i;
	unsigned int type;
	unsigned int n_items;
	unsigned int id;
	uint32_t *idlist = NULL;
	char *key, *val;

	/* Get the type and create the collection */
	if (!xmms_ipc_msg_get_uint32 (msg, &type)) {
		return false;
	}

	*coll = xmmsc_coll_new (type);

	/* Get the list of attributes */
	if (!xmms_ipc_msg_get_uint32 (msg, &n_items)) {
		goto err;
	}

	for (i = 0; i < n_items; i++) {
		unsigned int len;
		if (!xmms_ipc_msg_get_string_alloc (msg, &key, &len)) {
			goto err;
		}
		if (!xmms_ipc_msg_get_string_alloc (msg, &val, &len)) {
			free (key);
			goto err;
		}

		xmmsc_coll_attribute_set (*coll, key, val);
		free (key);
		free (val);
	}

	/* Get the idlist */
	if (!xmms_ipc_msg_get_uint32 (msg, &n_items)) {
		goto err;
	}

	if (!(idlist = x_new (uint32_t, n_items + 1))) {
		goto err;
	}

	for (i = 0; i < n_items; i++) {
		if (!xmms_ipc_msg_get_uint32 (msg, &id)) {
			goto err;
		}

		idlist[i] = id;
	}

	idlist[i] = 0;
	xmmsc_coll_set_idlist (*coll, idlist);
	free (idlist);
	idlist = NULL;

	/* Get the operands */
	if (!xmms_ipc_msg_get_uint32 (msg, &n_items)) {
		goto err;
	}

	for (i = 0; i < n_items; i++) {
		xmmsc_coll_t *operand;

		if (!xmms_ipc_msg_get_collection_alloc (msg, &operand)) {
			goto err;
		}

		xmmsc_coll_add_operand (*coll, operand);
		xmmsc_coll_unref (operand);
	}

	return true;

err:
	if (idlist != NULL) {
		free (idlist);
	}

	xmmsc_coll_unref (*coll);

	return false;
}










static void
free_dict_list (x_list_t *list)
{
	while (list) {
		free (list->data); /* key */
		list = x_list_delete_link (list, list);

		/* xmmsc_deserialize_dict guarantees that the list is
		 * well-formed
		 */
		assert (list);

		xmms_value_unref ((xmms_value_t *) list->data); /* value */
		list = x_list_delete_link (list, list);
	}
}

static xmms_value_t *
xmmsc_parse_value (xmms_ipc_msg_t *msg)
{
	xmms_value_t *val;

	val = xmms_value_new ();

	if (!xmms_ipc_msg_get_value (msg, val)) {
		x_internal_error ("Message from server did not parse correctly!");
		free (val);
		val = NULL;
	}

	return val;
}

static x_list_t *
xmmsc_deserialize_dict (xmms_ipc_msg_t *msg)
{
	unsigned int entries;
	unsigned int len;
	x_list_t *n = NULL;
	char *key;

	if (!xmms_ipc_msg_get_uint32 (msg, &entries)) {
		return NULL;
	}

	while (entries--) {
		xmms_value_t *val;

		if (!xmms_ipc_msg_get_string_alloc (msg, &key, &len)) {
			goto err;
		}

		val = xmmsc_parse_value (msg);
		if (!val) {
			free (key);
			goto err;
		}

		n = x_list_prepend (n, key);
		n = x_list_prepend (n, val);
	}

	return x_list_reverse (n);

err:
	x_internal_error ("Message from server did not parse correctly!");

	free_dict_list (x_list_reverse (n));

	return NULL;
}

/*
static bool
xmmsc_result_parse_msg (xmmsc_result_t *res, xmms_ipc_msg_t *msg)
{
	int type;
	x_list_t *list = NULL;

	if (xmms_value_iserror (res->data)) {
		res->parsed = true;
		return true;
	}

	if (!xmms_ipc_msg_get_int32 (msg, &type))
		return false;

	res->data->type = type;

	switch (type) {

		case XMMS_VALUE_TYPE_UINT32 :
			if (!xmms_ipc_msg_get_uint32 (msg, &res->data->value.uint32)) {
				return false;
			}
			break;
		case XMMS_VALUE_TYPE_INT32 :
			if (!xmms_ipc_msg_get_int32 (msg, &res->data->value.int32)) {
				return false;
			}
			break;
		case XMMS_VALUE_TYPE_BIN:
			{
				xmms_value_bin_t *bin;
				bin = x_new0 (xmms_value_bin_t, 1);
				if (!xmms_ipc_msg_get_bin_alloc (msg, &bin->data, &bin->len)) {
					free (bin);
					return false;
				}
				res->data->value.bin = bin;
				break;
			}
		case XMMS_VALUE_TYPE_STRING :
			{
				uint32_t len;

				if (!xmms_ipc_msg_get_string_alloc (msg,
				                                    &res->data->value.string,
				                                    &len)) {
					return false;
				}
			}
			break;
		case XMMS_VALUE_TYPE_DICT:
			{
				x_list_t *dict;

				dict = xmmsc_deserialize_dict (msg);
				if (!dict)
					return false;

				res->data->value.dict = dict;

			}
			break;
		case XMMS_VALUE_TYPE_LIST :
		case XMMS_VALUE_TYPE_PROPDICT :
			{
				uint32_t len, i;

				if (!xmms_ipc_msg_get_uint32 (msg, &len))
					return false;

				for (i = 0; i < len; i ++) {
					xmms_value_t *val;
					val = xmmsc_parse_value (msg);
					list = x_list_prepend (list, val);
				}

				if (list)
					list = x_list_reverse (list);

				res->data->current = res->data->list = list;

				if (type == XMMS_VALUE_TYPE_LIST) {
					res->data->islist = 1;

					if (res->data->current) {
						xmms_value_t *val = res->data->current->data;
						res->data->value.generic = val->value.generic;
						res->data->type = val->type;
					} else {
						res->data->value.generic = NULL;
						res->data->type = XMMS_VALUE_TYPE_NONE;
					}
				}
			}
			break;

		case XMMS_VALUE_TYPE_COLL:
			{
				xmmsc_coll_t *coll;

				if (!xmms_ipc_msg_get_collection_alloc (msg, &coll))
					return false;

				res->data->value.coll = coll;
				xmmsc_coll_ref (res->data->value.coll);
			}
			break;

		case XMMS_VALUE_TYPE_NONE :
			break;

		default :
			return false;
	}

	res->parsed = true;

	return true;
}
*/


bool
xmms_ipc_msg_get_value (xmms_ipc_msg_t *msg, xmms_value_t *val)
{
	int32_t type, i;
	uint32_t len, u;
	x_list_t *list = NULL;
	char *s;
	xmmsc_coll_t *c;
	unsigned char *d;

	if (xmms_value_iserror (val)) {
		return true;
	}

	if (!xmms_ipc_msg_get_int32 (msg, (int32_t *) &type)) {
		return false;
	}

	switch (type) {
		case XMMS_VALUE_TYPE_UINT32:
			if (!xmms_ipc_msg_get_uint32 (msg, &u)) {
				return false;
			}
			xmms_value_set_uint (val, u);
			break;
		case XMMS_VALUE_TYPE_INT32:
			if (!xmms_ipc_msg_get_int32 (msg, &i)) {
				return false;
			}
			xmms_value_set_int (val, i);
			break;
		case XMMS_VALUE_TYPE_STRING:
			if (!xmms_ipc_msg_get_string_alloc (msg, &s, &len)) {
				return false;
			}
			xmms_value_set_string (val, s);
			break;
		case XMMS_VALUE_TYPE_DICT:
			list = xmmsc_deserialize_dict (msg);
			if (!list) {
				return false;
			}

			xmms_value_set_dict (val, list);
			break;

		case XMMS_VALUE_TYPE_LIST :
		case XMMS_VALUE_TYPE_PROPDICT :
			if (!xmms_ipc_msg_get_uint32 (msg, &len)) {
				return false;
			}

			for (u = 0; u < len; u ++) {
				xmms_value_t *v;
				v = xmms_value_new ();
				if (xmms_ipc_msg_get_value (msg, v)) {
					list = x_list_prepend (list, v);
				} else {
					/* FIXME: free all, error! */
				}
			}
			list = x_list_reverse (list);

			if (type == XMMS_VALUE_TYPE_LIST) {
				xmms_value_set_list (val, list);
			} else {
				xmms_value_set_propdict (val, list);
			}
			break;

		case XMMS_VALUE_TYPE_COLL:
			xmms_ipc_msg_get_collection_alloc (msg, &c);
			if (!c) {
				return false;
			}
			xmms_value_set_collection (val, c);
			break;

		case XMMS_VALUE_TYPE_BIN:
			if (!xmms_ipc_msg_get_bin_alloc (msg, &d, &len)) {
				return false;
			}
			xmms_value_set_bin (val, d, len);
			break;

		case XMMS_VALUE_TYPE_NONE:
			break;
		default:
			return false;
			break;
	}

	return true;
}

