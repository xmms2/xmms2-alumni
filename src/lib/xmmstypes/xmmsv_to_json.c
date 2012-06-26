#include <string.h>

#include "xmmspriv/xmmsv.h"
#include "xmmsc/xmmsv.h"
#include "xmmsc/xmmsc_util.h"
#include "xmmsc/xmmsc_stdbool.h"

#define CAPACITY 16
#define GROWTH_FACTOR 2
#define INT32_ASCII_LENGTH 11
#define INNER "inner"

typedef struct serialize_St {
	char *buffer;
	int capacity;
	int position;
} serialize_t;

typedef bool (*serialize_func_t)(serialize_t *, xmmsv_t *);

static bool _serialize_coll (serialize_t *data, xmmsv_t *value);
static bool _serialize_value (serialize_t *data, xmmsv_t *value);

static bool
_serialize_init (serialize_t *data)
{
	char *buffer;

	memset (data, 0, sizeof (serialize_t));

	buffer = x_malloc (CAPACITY);
	if (!buffer)
		return false;

	data->buffer = buffer;
	data->capacity = CAPACITY;
	data->position = 0;

	return true;
}

static char *
_serialize_finish (serialize_t *data)
{
	char *buffer;

	buffer = x_realloc (data->buffer, data->position + 1);
	if (!buffer) {
		free (data->buffer);
		return NULL;
	}

	buffer[data->position] = '\0';

	return buffer;
}

static bool
_serialize_ensure_capacity (serialize_t *data, int length)
{
	char *buffer;
	int capacity, required;

	required = data->position + length;
	if (required < data->capacity)
		return true;

	capacity = data->capacity;
	while (capacity <= required)
		capacity *= GROWTH_FACTOR;

	buffer = x_realloc (data->buffer, capacity);
	if (!buffer)
		return false;

	data->buffer = buffer;
	data->capacity = capacity;

	return true;
}

static int
_serialize_escape (char c, char out[7])
{
	out[4] = '\\';
	out[5] = c;

	switch (c) {
		case '\b':
			out[5] = 'b';
			return 2;
		case '\f':
			out[5] = 'f';
			return 2;
		case '\t':
			out[5] = 't';
			return 2;
		case '\r':
			out[5] = 'r';
			return 2;
		case '\n':
			out[5] = 'n';
			return 2;
		case '\\':
		case '"':
		case '/':
			return 2;
		default:
			if (c < ' ') {
				snprintf (out, 7, "\\u00%.2x", c);
				return 6;
			}
	}

	return 1;
}

static bool
_serialize_char (serialize_t *data, char c)
{
	char tmp[7] = { '\0' };
	int size = _serialize_escape (c, tmp);

	if (!_serialize_ensure_capacity (data, size))
		return false;

	data->position += snprintf (data->buffer + data->position,
	                            data->capacity - data->position,
	                            "%s", tmp + 6 - size);

	return true;
}

static bool
_serialize_cstring (serialize_t *data, const char *value)
{
	int length;

	length = strlen (value) + 2; /* quote before/after */
	if (!_serialize_ensure_capacity (data, length))
		return false;

	data->buffer[data->position++] = '\"';
	while (*value) {
		_serialize_char (data, *value++);
	}

	if (!_serialize_ensure_capacity (data, 1))
		return false;
	data->buffer[data->position++] = '\"';

	return true;
}

static bool
_serialize_string (serialize_t *data, xmmsv_t *value)
{
	const char *inner;

	if (!xmmsv_get_string (value, &inner))
		return false;

	return _serialize_cstring (data, inner);
}

static bool
_serialize_integer (serialize_t *data, xmmsv_t *value)
{
	int inner;

	if (!xmmsv_get_int (value, &inner))
		return false;

	if (!_serialize_ensure_capacity (data, INT32_ASCII_LENGTH))
		return false;

	data->position += snprintf (data->buffer + data->position,
	                            data->capacity - data->position,
	                            "%d", inner);

	return true;
}

static bool
_serialize_dict_entry_cstring (serialize_t *data,
                               const char *key, const char *value)
{
	if (!_serialize_cstring (data, key))
		return false;

	if (!_serialize_char (data, ':'))
		return false;

	if (!_serialize_cstring (data, value))
		return false;

	return true;
}

static bool
_serialize_dict_entry (serialize_t *data, const char *key, xmmsv_t *value,
                       serialize_func_t inner_func)
{
	if (!_serialize_cstring (data, key))
		return false;

	if (!_serialize_char (data, ':'))
		return false;

	if (!inner_func (data, value))
		return false;

	return true;
}

static bool
_serialize_dict (serialize_t *data, xmmsv_t *value)
{
	xmmsv_dict_iter_t *it;

	if (!xmmsv_get_dict_iter (value, &it))
		return false;

	if (!_serialize_char (data, '{'))
		return false;

	if (xmmsv_dict_iter_valid (it)) {
		const char *key;
		xmmsv_t *entry;

		if (!xmmsv_dict_iter_pair (it, &key, &entry))
			return false;

		if (!_serialize_dict_entry (data, key, entry, _serialize_value))
			return false;

		xmmsv_dict_iter_next (it);

		while (xmmsv_dict_iter_valid (it)) {
			if (!xmmsv_dict_iter_pair (it, &key, &entry))
				return false;

			if (!_serialize_char (data, ','))
				return false;

			if (!_serialize_dict_entry (data, key, entry, _serialize_value))
				return false;

			xmmsv_dict_iter_next (it);
		}
	}

	if (!_serialize_char (data, '}'))
		return false;

	return true;
}

static bool
_serialize_list (serialize_t *data, xmmsv_t *value, serialize_func_t func)
{
	xmmsv_list_iter_t *it;
	xmmsv_type_t type;

	if (!xmmsv_get_list_iter (value, &it))
		return false;

	if (!_serialize_char (data, '['))
		return false;

	if (xmmsv_list_iter_valid (it)) {
		xmmsv_t *entry;

		if (!xmmsv_list_iter_entry (it, &entry))
			return false;

		if (!func (data, entry))
			return false;

		xmmsv_list_iter_next (it);

		while (xmmsv_list_iter_valid (it)) {
			if (!xmmsv_list_iter_entry (it, &entry))
				return false;

			if (!_serialize_char (data, ','))
				return false;

			if (!func (data, entry))
				return false;

			xmmsv_list_iter_next (it);
		}
	}

	if (!_serialize_char (data, ']'))
		return false;

	return true;
}


static bool
_serialize_object (serialize_t *data, const char *type, xmmsv_t *inner,
                   serialize_func_t inner_func)
{
	if (!_serialize_char (data, '{'))
		return false;

	if (!_serialize_dict_entry_cstring (data, "type", type))
		return false;

	if (!_serialize_char (data, ','))
		return false;

	if (!_serialize_dict_entry (data, "inner", inner, inner_func))
		return false;

	if (!_serialize_char (data, '}'))
		return false;

	return true;
}

static bool
_serialize_normal_list (serialize_t *data, xmmsv_t *value)
{
	return _serialize_list (data, value, _serialize_value);
}

static bool
_serialize_dict_object (serialize_t *data, xmmsv_t *value)
{
	return _serialize_object (data, "dict", value, _serialize_dict);
}

static bool
_serialize_get_coll_name (xmmsv_t *coll, const char **type)
{
	switch (xmmsv_coll_get_type (coll)) {
		case XMMS_COLLECTION_TYPE_REFERENCE:
			*type = "reference";
			return true;
		case XMMS_COLLECTION_TYPE_UNIVERSE:
			*type = "universe";
			return true;
		case XMMS_COLLECTION_TYPE_UNION:
			*type = "union";
			return true;
		case XMMS_COLLECTION_TYPE_INTERSECTION:
			*type = "intersection";
			return true;
		case XMMS_COLLECTION_TYPE_COMPLEMENT:
			*type = "complement";
			return true;
		case XMMS_COLLECTION_TYPE_HAS:
			*type = "has";
			return true;
		case XMMS_COLLECTION_TYPE_MATCH:
			*type = "match";
			return true;
		case XMMS_COLLECTION_TYPE_TOKEN:
			*type = "token";
			return true;
		case XMMS_COLLECTION_TYPE_EQUALS:
			*type = "equals";
			return true;
		case XMMS_COLLECTION_TYPE_NOTEQUAL:
			*type = "notequals";
			return true;
		case XMMS_COLLECTION_TYPE_SMALLER:
			*type = "smaller";
			return true;
		case XMMS_COLLECTION_TYPE_SMALLEREQ:
			*type = "smallereq";
			return true;
		case XMMS_COLLECTION_TYPE_GREATER:
			*type = "greater";
			return true;
		case XMMS_COLLECTION_TYPE_GREATEREQ:
			*type = "greatereq";
			return true;
		case XMMS_COLLECTION_TYPE_ORDER:
			*type = "order";
			return true;
		case XMMS_COLLECTION_TYPE_LIMIT:
			*type = "limit";
			return true;
		case XMMS_COLLECTION_TYPE_MEDIASET:
			*type = "mediaset";
			return true;
		case XMMS_COLLECTION_TYPE_IDLIST:
			*type = "idlist";
			return true;
		default:
			*type = NULL;
			x_api_error ("Trying to serialize invalid collection type", false);
	}
	return false;
}

static bool
_serialize_coll_operands (serialize_t *data, xmmsv_t *value)
{
	return _serialize_list (data, value, _serialize_coll);
}

static bool
_serialize_coll_idlist (serialize_t *data, xmmsv_t *value)
{
	return _serialize_list (data, value, _serialize_integer);
}

static bool
_serialize_coll (serialize_t *data, xmmsv_t *value)
{
	const char *type;
	xmmsv_t *inner;

	if (!_serialize_get_coll_name (value, &type))
		return false;

	if (!_serialize_char (data, '{'))
		return false;

	if (!_serialize_dict_entry_cstring (data, "type", type))
		return false;

	inner = xmmsv_coll_operands_get (value);
	if (xmmsv_list_get_size (inner) > 0) {
		if (!_serialize_char (data, ','))
			return false;

		if (!_serialize_dict_entry (data, "operands", inner, _serialize_coll_operands))
			return false;
	}

	inner = xmmsv_coll_attributes_get (value);
	if (xmmsv_dict_get_size (inner) > 0) {
		if (!_serialize_char (data, ','))
			return false;

		if (!_serialize_dict_entry (data, "attributes", inner, _serialize_dict))
			return false;
	}

	inner = xmmsv_coll_idlist_get (value);
	if (xmmsv_list_get_size (inner) > 0) {
		if (!_serialize_char (data, ','))
			return false;

		if (!_serialize_dict_entry (data, "idlist", inner, _serialize_coll_idlist))
			return false;
	}

	if (!_serialize_char (data, '}'))
		return false;

	return true;
}

static bool
_serialize_coll_object (serialize_t *data, xmmsv_t *value)
{
	return _serialize_object (data, "coll", value, _serialize_coll);
}

static serialize_func_t
_serialize_get_serializer (xmmsv_t *value)
{
	switch (xmmsv_get_type (value)) {
		case XMMSV_TYPE_STRING:
			return _serialize_string;
		case XMMSV_TYPE_INT32:
			return _serialize_integer;
		case XMMSV_TYPE_LIST:
			return _serialize_normal_list;
		case XMMSV_TYPE_DICT:
			return _serialize_dict_object;
		case XMMSV_TYPE_COLL:
			return _serialize_coll_object;
		default:
			x_internal_error ("Trying to serialize unsupported type");
			return NULL;
	}
}

static bool
_serialize_value (serialize_t *data, xmmsv_t *value)
{
	serialize_func_t serializer;

	serializer = _serialize_get_serializer (value);
	if (!serializer)
		return false;

	if (!serializer (data, value))
		return false;

	return true;
}

char *
xmmsv_to_json (xmmsv_t *value)
{
	serialize_t data;

	_serialize_init (&data);
	if (!_serialize_value (&data, value)) {
		free (data.buffer);
		return NULL;
	}

	return _serialize_finish (&data);
}
