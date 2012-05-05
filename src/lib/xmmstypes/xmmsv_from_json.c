/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2012 XMMS2 Team
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

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "json.h"

#include "xmmsc/xmmsv.h"
#include "xmmsc/xmmsc_stdbool.h"

static bool _normalize_xmmsv (xmmsv_t **value);
static bool _normalize_object (xmmsv_t **value);
static bool _normalize_inner_list (xmmsv_t **value);
static bool _normalize_inner_dict (xmmsv_t **value);
static bool _normalize_inner_collection (xmmsv_t **value);

static bool
_normalize_collection_type_from_name (const char *name, xmmsv_coll_type_t *type)
{
	if (strcmp ("reference", name) == 0)
		*type = XMMS_COLLECTION_TYPE_REFERENCE;
	else if (strcmp ("universe", name) == 0)
		*type = XMMS_COLLECTION_TYPE_UNIVERSE;
	else if (strcmp ("union", name) == 0)
		*type = XMMS_COLLECTION_TYPE_UNION;
	else if (strcmp ("intersection", name) == 0)
		*type = XMMS_COLLECTION_TYPE_INTERSECTION;
	else if (strcmp ("complement", name) == 0)
		*type = XMMS_COLLECTION_TYPE_COMPLEMENT;
	else if (strcmp ("has", name) == 0)
		*type = XMMS_COLLECTION_TYPE_HAS;
	else if (strcmp ("match", name) == 0)
		*type = XMMS_COLLECTION_TYPE_MATCH;
	else if (strcmp ("token", name) == 0)
		*type = XMMS_COLLECTION_TYPE_TOKEN;
	else if (strcmp ("equals", name) == 0)
		*type = XMMS_COLLECTION_TYPE_EQUALS;
	else if (strcmp ("notequals", name) == 0)
		*type = XMMS_COLLECTION_TYPE_NOTEQUAL;
	else if (strcmp ("smaller", name) == 0)
		*type = XMMS_COLLECTION_TYPE_SMALLER;
	else if (strcmp ("smallereq", name) == 0)
		*type = XMMS_COLLECTION_TYPE_SMALLEREQ;
	else if (strcmp ("greater", name) == 0)
		*type = XMMS_COLLECTION_TYPE_GREATER;
	else if (strcmp ("greatereq", name) == 0)
		*type = XMMS_COLLECTION_TYPE_GREATEREQ;
	else if (strcmp ("order", name) == 0)
		*type = XMMS_COLLECTION_TYPE_ORDER;
	else if (strcmp ("limit", name) == 0)
		*type = XMMS_COLLECTION_TYPE_LIMIT;
	else if (strcmp ("mediaset", name) == 0)
		*type = XMMS_COLLECTION_TYPE_MEDIASET;
	else if (strcmp ("idlist", name) == 0)
		*type = XMMS_COLLECTION_TYPE_IDLIST;
	else
		return false;
	return true;
}

static bool
_normalize_inner_collection (xmmsv_t **value)
{
	xmmsv_t *coll, *attributes, *operands, *idlist;
	xmmsv_coll_type_t type;
	const char *name;

	if (!xmmsv_dict_entry_get_string (*value, "type", &name))
		return false;

	if (!_normalize_collection_type_from_name (name, &type))
		return false;

	coll = xmmsv_new_coll (type);

	if (xmmsv_dict_get (*value, "attributes", &attributes)) {
		if (!_normalize_inner_dict (&attributes))
			goto _normalize_collection_failed;

		xmmsv_coll_attributes_set (coll, attributes);
	}

	if (xmmsv_dict_get (*value, "operands", &operands)) {
		xmmsv_list_iter_t *it;

		if (!xmmsv_get_list_iter (operands, &it))
			goto _normalize_collection_failed;

		while (xmmsv_list_iter_valid (it)) {
			xmmsv_t *operand;

			xmmsv_list_iter_entry (it, &operand);

			if (!_normalize_inner_collection (&operand))
				goto _normalize_collection_failed;

			xmmsv_coll_add_operand (coll, operand);
			xmmsv_unref (operand);

			xmmsv_list_iter_next (it);
		}

		xmmsv_list_iter_explicit_destroy (it);
	}

	if (xmmsv_dict_get (*value, "idlist", &idlist)) {
		xmmsv_list_iter_t *it;

		if (!xmmsv_get_list_iter (idlist, &it))
			goto _normalize_collection_failed;

		while (xmmsv_list_iter_valid (it)) {
			int mid;

			if (!xmmsv_list_iter_entry_int (it, &mid))
				goto _normalize_collection_failed;

			xmmsv_coll_idlist_append (coll, mid);

			xmmsv_list_iter_next (it);
		}

		xmmsv_list_iter_explicit_destroy (it);
	}

	*value = coll;

	return true;

_normalize_collection_failed:
	xmmsv_unref (coll);
	return false;
}

static bool
_normalize_inner_dict (xmmsv_t **value)
{
	xmmsv_dict_iter_t *it;

	if (!xmmsv_get_dict_iter (*value, &it))
		return false;

	while (xmmsv_dict_iter_valid (it)) {
		xmmsv_t *entry, *original;
		const char *key;

		xmmsv_dict_iter_pair (it, &key, &entry);

		/* incref as we might be removing an intermediate node */
		original = xmmsv_ref (entry);

		if (!_normalize_xmmsv (&entry)) {
			xmmsv_unref (entry);
			return false;
		}

		if (original != entry) {
			xmmsv_dict_iter_set (it, entry);
		}

		xmmsv_unref (entry);

		xmmsv_dict_iter_next (it);
	}

	xmmsv_dict_iter_explicit_destroy (it);

	return true;
}

static bool
_normalize_list (xmmsv_t **value)
{
	xmmsv_list_iter_t *it;
	xmmsv_t *entries;
	const char *name;

	if (!xmmsv_get_list_iter (*value, &it))
		return false;

	while (xmmsv_list_iter_valid (it)) {
		xmmsv_t *entry, *original;

		xmmsv_list_iter_entry (it, &entry);

		/* incref as we might be removing an intermediate node */
		original = xmmsv_ref (entry);

		if (!_normalize_xmmsv (&entry)) {
			xmmsv_unref (entry);
			return false;
		}

		if (original != entry) {
			xmmsv_list_iter_set (it, entry);
		}

		xmmsv_unref (entry);

		xmmsv_list_iter_next (it);
	}

	xmmsv_list_iter_explicit_destroy (it);

	return true;
}

static bool
_normalize_object (xmmsv_t **value)
{
	bool result = false;
	const char *type;
	xmmsv_t *inner;

	if (!xmmsv_dict_entry_get_string (*value, "type", &type))
		return false;

	if (!xmmsv_dict_get (*value, "inner", &inner))
		return false;

	if (strcmp ("dict", type) == 0) {
		result = _normalize_inner_dict (&inner);
		/* we're not removing any extra node for dict,
		 * so lets just incref the current inner node
		 */
		xmmsv_ref (inner);
	} else if (strcmp ("coll", type) == 0) {
		result = _normalize_inner_collection (&inner);
	}

	if (!result)
		printf ("object of type %s failed", type);

	xmmsv_unref (*value);
	*value = inner;

	return result;
}

/**
 * Prune nodes and convert json structure into a xmmsv_t structure.
 *
 * At this point everything is deserialized into basic values, lists and dicts.
 * Here we need to convert the object types into their corresponding xmmsv_t
 * values such as restricted lists, collections etc.
 */
bool
_normalize_xmmsv (xmmsv_t **value)
{
	if (xmmsv_is_type (*value, XMMSV_TYPE_DICT))
		return _normalize_object (value);
	if (xmmsv_is_type (*value, XMMSV_TYPE_LIST))
		return _normalize_list (value);
	return true;
}

static xmmsv_t *
_json_create_structure (int stack_offset, int is_object)
{
	if (is_object)
		return xmmsv_new_dict ();
	return xmmsv_new_list ();
}

static xmmsv_t *
_json_create_data (int type, const char *data, uint32_t len)
{
	switch (type) {
		case JSON_STRING:
			return xmmsv_new_string (data);
		case JSON_INT:
			return xmmsv_new_int (atoi(data));
		case JSON_FLOAT:
			return xmmsv_new_string (data);
		case JSON_NULL:
			return xmmsv_new_none ();
		case JSON_TRUE:
			return xmmsv_new_int (1);
		case JSON_FALSE:
			return xmmsv_new_int (0);
		default:
			return NULL;
	}
}

static int
_json_append (xmmsv_t *obj, const char *key, uint32_t key_len, xmmsv_t *value)
{
	if (xmmsv_is_type (obj, XMMSV_TYPE_LIST)) {
		xmmsv_list_append (obj, value);
	} else if (xmmsv_is_type (obj, XMMSV_TYPE_DICT) && key) {
		xmmsv_dict_set (obj, key, value);
	} else {
		/* Should never be reached */
		assert (0);
	}
	xmmsv_unref (value);
	return 0;
}

xmmsv_t *
xmmsv_from_json (const char *spec)
{
	json_config conf = {
		0, /* buffer_initial_size (0=default) */
		0, /* max_nesting (0=no limit) */
		0, /* max_data (0=no limit) */
		1, /* allow_c_comments */
		0, /* allow_yaml_comments */
		NULL, /* user_calloc */
		NULL /* user_realloc */
	};
	json_parser_dom dom;
	json_parser parser;
	xmmsv_t *value;
	int error;

	json_parser_dom_init (&dom,
	                      (json_parser_dom_create_structure) _json_create_structure,
	                      (json_parser_dom_create_data) _json_create_data,
	                      (json_parser_dom_append) _json_append);

	json_parser_init (&parser, &conf, json_parser_dom_callback, &dom);

	error = json_parser_string (&parser, spec, strlen (spec), NULL);
	if (error != 0) {
		switch (error) {
			case JSON_ERROR_BAD_CHAR:
				fprintf (stderr, "Failed to parse due to bad character!\n");
				break;
			case JSON_ERROR_UNEXPECTED_CHAR:
				fprintf (stderr, "Failed to parse due to unexpected character!\n");
				break;
			case JSON_ERROR_NO_MEMORY:
				fprintf (stderr, "Failed to parse (%d)!\n", error);
				break;
		}

		return NULL;
	}

	assert (dom.root_structure != NULL);
	assert (dom.stack_offset == 0);

	value = (xmmsv_t *) dom.root_structure;

	json_parser_dom_free (&dom);
	json_parser_free (&parser);

	if (value != NULL && !_normalize_xmmsv (&value)) {
		xmmsv_unref (value);
		printf ("normalization failed\n");
		return NULL;
	}

	return value;
}
