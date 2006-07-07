/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2006 XMMS2 Team
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

/**
 * @file
 * xforms
 */

#include <glib.h>

#include "xmmspriv/xmms_xform.h"
#include "xmms/xmms_log.h"
#include "xmms/xmms_object.h"


struct xmms_stream_type_St {
	xmms_object_t obj;
	GList *list;
};

typedef enum xmms_stream_type_val_type_E {
	STRING,
	INT,
} xmms_stream_type_val_type_t;

typedef struct xmms_stream_type_val_St {
	xmms_stream_type_key_t key;
	xmms_stream_type_val_type_t type;
	union {
		char *string;
		int num;
	} d;
} xmms_stream_type_val_t;


static void
xmms_stream_type_destroy (xmms_object_t *obj)
{
	xmms_stream_type_t *st = (xmms_stream_type_t *)obj;
	GList *n;

	for (n = st->list; n; n = g_list_next (n)) {
		xmms_stream_type_val_t *val = n->data;
		if (val->type == STRING) {
			g_free (val->d.string);
		}
		g_free (val);
	}

	g_list_free (st->list);
}

xmms_stream_type_t *
xmms_stream_type_parse (va_list ap)
{
	xmms_stream_type_t *res;

	res = xmms_object_new (xmms_stream_type_t, xmms_stream_type_destroy);
	if (!res) {
		return NULL;
	}

	for (;;) {
		xmms_stream_type_val_t *val;
		val = g_new0 (xmms_stream_type_val_t, 1);
		val->key = va_arg (ap, int);

		switch (val->key) {
		case XMMS_STREAM_TYPE_END:
			g_free (val);
			return res;
		case XMMS_STREAM_TYPE_MIMETYPE:
		case XMMS_STREAM_TYPE_URL:
			val->type = STRING;
			val->d.string = g_strdup (va_arg (ap, char *));
			break;
		case XMMS_STREAM_TYPE_FMT_FORMAT:
		case XMMS_STREAM_TYPE_FMT_CHANNELS:
		case XMMS_STREAM_TYPE_FMT_SAMPLERATE:
		case XMMS_STREAM_TYPE_PRIVATE_INT:
			val->type = INT;
			val->d.num = va_arg (ap, int);
			break;
		default:
			XMMS_DBG ("UNKNOWN TYPE!!");
			return NULL;
		}
		res->list = g_list_append (res->list, val);
	}
}


const char *
xmms_stream_type_get_str (const xmms_stream_type_t *st, xmms_stream_type_key_t key)
{
	GList *n;

	for (n = st->list; n; n = g_list_next (n)) {
		xmms_stream_type_val_t *val = n->data;
		if (val->key == key) {
			if (val->type != STRING) {
				XMMS_DBG ("Key passed to get_str is not string");
				return NULL;
			}
			return val->d.string;
		}
	}
	return NULL;
}


gint
xmms_stream_type_get_int (const xmms_stream_type_t *st, xmms_stream_type_key_t key)
{
	GList *n;

	for (n = st->list; n; n = g_list_next (n)) {
		xmms_stream_type_val_t *val = n->data;
		if (val->key == key) {
			if (val->type != INT) {
				XMMS_DBG ("Key passed to get_int is not int");
				return -1;
			}
			return val->d.num;
		}
	}
	return -1;
}




static gboolean
match_val (xmms_stream_type_val_t *vin, xmms_stream_type_val_t *vout)
{
	if (vin->type != vout->type)
		return FALSE;
	switch (vin->type) {
	case STRING:
		return g_pattern_match_simple (vin->d.string, vout->d.string);
	case INT:
		return vin->d.num == vout->d.num;
	}
	return FALSE;
}

gboolean
xmms_stream_type_match (const xmms_stream_type_t *in_type, const xmms_stream_type_t *out_type)
{
	GList *in;

	for (in = in_type->list; in; in = g_list_next (in)) {
		xmms_stream_type_val_t *inval = in->data;
		GList *n;
		
		for (n = out_type->list; n; n = g_list_next (n)) {
			xmms_stream_type_val_t *outval = n->data;
			if (inval->key == outval->key) {
				if (!match_val (inval, outval))
					return FALSE;
				break;
			}
			
		}
		if (!n) {
			/* didn't exist in out */
			return FALSE;
		}
	}

	return TRUE;
}


/*
	XMMS_DBG ("Looking for xform with intypes matching:");
	for (n = prev->out_types; n; n = g_list_next (n)) {
		xmms_stream_type_val_t *val = n->data;
		switch (val->type) {
		case INT:
			XMMS_DBG (" - %d = %d", val->key, val->d.num);
			break;
		case STRING:
			XMMS_DBG (" - %d = '%s'", val->key, val->d.string);
			break;
		default:
			XMMS_DBG (" - ????");
			break;
		}
	}

*/

xmms_stream_type_t *
_xmms_stream_type_new (void *dumb, ...)
{
	xmms_stream_type_t *res;
	va_list ap;

	va_start (ap, dumb);
	res = xmms_stream_type_parse (ap);
	va_end (ap);

	return res;
}
