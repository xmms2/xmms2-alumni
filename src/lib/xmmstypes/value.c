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


typedef struct xmms_value_bin_St {
	unsigned char *data;
	uint32_t len;
} xmms_value_bin_t;


struct xmms_value_St {
	union {
		void *generic;
		uint32_t uint32;
		int32_t int32;
		char *string;
		x_list_t *dict;
		xmmsc_coll_t *coll;
		xmms_value_bin_t *bin;
	} value;
	xmms_value_type_t type;

	/** refcounting */
	int ref;

	int error;
	char *error_str;

	int islist;
	x_list_t *list;
	x_list_t *current;

	/* the list of sources from most to least prefered.
	 * if this is NULL, then default_source_pref will be used instead.
	 */
	char **source_pref;

	/* things we want to free when the value is freed*/
	x_list_t *extra_free;
};


static void xmms_value_free (xmms_value_t *val);
static xmms_value_t *propdict_lookup (xmms_value_t *val, const char *key);
static xmms_value_t *plaindict_lookup (xmms_value_t *val, const char *key);
static int source_match_pattern (const char *source, const char *pattern);
static void value_data_free (xmms_value_t *val);


static const char *default_source_pref[] = {
	"server",
	"client/*",
	"plugin/id3v2",
	"plugin/segment",
	"plugin/*",
	"*",
	NULL
};


/**
 * @defgroup ValueType ValueType
 * @ingroup Values
 * @brief The API to be used to work with value objects.
 *
 * @{
 */

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
 * Should not be used from a client.
 * @internal
 */
xmms_value_t *
xmms_value_new ()
{
	xmms_value_t *val;

	val = x_new0 (xmms_value_t, 1);
	if (!val) {
		x_oom ();
		return NULL;
	}

	xmms_value_ref (val);
	return val;
}

/* FIXME: duplicated from msg.c = BAD!!!! */
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


static void
xmms_value_free (xmms_value_t *val)
{
	x_return_if_fail (val);

	value_data_free (val);

	free (val->error_str);

	if (val->source_pref) {
		xmms_strlist_destroy (val->source_pref);
	}

	while (val->extra_free) {
		free (val->extra_free->data);
		val->extra_free = x_list_delete_link (val->extra_free,
		                                      val->extra_free);
	}

	free (val);
}


static void
value_data_free (xmms_value_t *val)
{
	x_return_if_fail (val);

	/* No data to free */
	if (val->type != XMMS_VALUE_TYPE_NONE) {
		return;
	}

	if (val->islist) {
		val->type = XMMS_VALUE_TYPE_LIST;
	}

	switch (val->type) {
		case XMMS_VALUE_TYPE_NONE :
		case XMMS_VALUE_TYPE_UINT32 :
		case XMMS_VALUE_TYPE_INT32 :
			break;
		case XMMS_VALUE_TYPE_STRING :
			free (val->value.string);
			val->value.string = NULL;
			break;
		case XMMS_VALUE_TYPE_BIN :
			free (val->value.bin->data);
			free (val->value.bin);
			val->value.bin = NULL;
			break;
		case XMMS_VALUE_TYPE_LIST:
		case XMMS_VALUE_TYPE_PROPDICT:
			while (val->list) {
				xmms_value_unref ((xmms_value_t *) val->list->data);
				val->list = x_list_delete_link (val->list, val->list);
			}
			break;
		case XMMS_VALUE_TYPE_DICT:
			free_dict_list (val->value.dict);
			val->value.dict = NULL;
			break;
		case XMMS_VALUE_TYPE_COLL:
			xmmsc_coll_unref (val->value.coll);
			val->value.coll = NULL;
			break;
			/* FIXME: handle STRINGLIST */
	}

	val->type = XMMS_VALUE_TYPE_NONE;
	val->islist = 0;
}



/**
 * Check the #xmms_value_t for error.
 * @return 1 if error was encountered, else 0
 */
int
xmms_value_iserror (xmms_value_t *val)
{
	x_return_val_if_fail (val, 1);

	if (val->error > 0) {
		return 1;
	}

	return 0;
}

/**
 * Get an error string describing the error that occoured
 */

const char *
xmms_value_get_error (xmms_value_t *val)
{
	x_return_null_if_fail (val);

	return val->error_str;
}


/**
 * Set the error in the value object.
 * @param val The value in which to set the error.
 * @param errstr The error message.
 */
void
xmms_value_seterror (xmms_value_t *val, const char *errstr)
{
	/* free if already set to avoid leaks */
	free (val->error_str);

	val->error_str = strdup (errstr);
	val->error = 1;
}


/**
 * Set sources to be used when fetching stuff from a propdict.
 * @param val a #xmms_value_t that you got from a command dispatcher.
 * @param preference a list of sources from most to least preferrable.
 * You may use a wildcard "*" character.
 */
void
xmms_value_source_preference_set (xmms_value_t *val, const char **preference)
{
	x_return_if_fail (val);
	x_return_if_fail (preference);

	if (val->source_pref) {
		xmms_strlist_destroy (val->source_pref);
	}

	val->source_pref = xmms_strlist_copy ((char **) preference);
}

/**
 * Get sources to be used when fetching stuff from a propdict.
 * @param val a #xmms_value_t that you got from a command dispatcher.
 * @returns The current sources from most to least preferable, as a
 * NULL-terminated array of immutable strings.
 * This array is owned by the value and will be freed with it.
 */
const char **
xmms_value_source_preference_get (xmms_value_t *val)
{
	x_return_val_if_fail (val, NULL);

	if (val->source_pref)
		return (const char **) val->source_pref;
	else
		return default_source_pref;
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

void
xmms_value_set_int (xmms_value_t *val, int32_t r)
{
	x_return_if_fail (val);

	value_data_free (val);

	val->type = XMMS_VALUE_TYPE_INT32;
	val->value.int32 = r;
}

void
xmms_value_set_uint (xmms_value_t *val, uint32_t r)
{
	x_return_if_fail (val);

	value_data_free (val);

	val->type = XMMS_VALUE_TYPE_UINT32;
	val->value.uint32 = r;
}

void
xmms_value_set_string (xmms_value_t *val, const char *r)
{
	x_return_if_fail (val);

	value_data_free (val);

	val->type = XMMS_VALUE_TYPE_STRING;
	val->value.string = strdup (r);
}

void
xmms_value_set_collection (xmms_value_t *val, xmmsc_coll_t *coll)
{
	x_return_if_fail (val);

	value_data_free (val);

	xmmsc_coll_ref (coll);

	val->type = XMMS_VALUE_TYPE_COLL;
	val->value.coll = coll;
}

void
xmms_value_set_bin (xmms_value_t *val, unsigned char *r, unsigned int rlen)
{
	x_return_if_fail (val);

	value_data_free (val);

	/* FIXME: copy data? */

	val->type = XMMS_VALUE_TYPE_BIN;
	val->value.bin = x_new0 (xmms_value_bin_t, 1);
	val->value.bin->data = r;
	val->value.bin->len = rlen;
}

void
xmms_value_set_list (xmms_value_t *val, void *rv)
{
	x_return_if_fail (val);

	x_list_t *r = (x_list_t *) rv;

	value_data_free (val);

	val->islist = 1;

	val->current = val->list = r;

	if (val->current) {
		xmms_value_t *v = val->current->data;
		val->value.generic = v->value.generic;
		val->type = v->type;
	} else {
		val->value.generic = NULL;
		val->type = XMMS_VALUE_TYPE_NONE;
	}
}

void
xmms_value_set_propdict (xmms_value_t *val, void *rv)
{
	x_return_if_fail (val);

	x_list_t *r = (x_list_t *) rv;

	value_data_free (val);

	val->type = XMMS_VALUE_TYPE_PROPDICT;
	val->current = val->list = r;
}

void
xmms_value_set_dict (xmms_value_t *val, void *rv)
{
	x_return_if_fail (val);

	x_list_t *r = (x_list_t *) rv;

	value_data_free (val);

	val->type = XMMS_VALUE_TYPE_DICT;
	val->value.dict = r;
}


/**
 * Retrives a signed integer from the value.
 * @param val a #xmms_value_t containing a integer.
 * @param r the return integer.
 * @return 1 upon success otherwise 0
 */

int
xmms_value_get_int (xmms_value_t *val, int32_t *r)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (val->type != XMMS_VALUE_TYPE_INT32) {
		return 0;
	}

	*r = val->value.int32;

	return 1;
}

/**
 * Retrives a unsigned integer from the resultset.
 * @param val a #xmms_value_t containing a integer.
 * @param r the return integer.
 * @return 1 upon success otherwise 0
 */

int
xmms_value_get_uint (xmms_value_t *val, uint32_t *r)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (val->type != XMMS_VALUE_TYPE_UINT32)
		return 0;

	*r = val->value.uint32;

	return 1;
}

/**
 * Retrives a string from the resultset.
 * @param val a #xmms_value_t containing a string.
 * @param r the return string. This string is owned by the value and will be freed when the value is freed.
 * @return 1 upon success otherwise 0
 */
int
xmms_value_get_string (xmms_value_t *val, const char **r)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (val->type != XMMS_VALUE_TYPE_STRING) {
		return 0;
	}

	*r = val->value.string;

	return 1;
}

/**
 * Retrieves a collection from the resultset.
 * @param val a #xmms_value_t containing a collection.
 * @param c the return collection. This collection is owned by the value and will be freed when the value is freed.
 * @return 1 upon success otherwise 0
 */
int
xmms_value_get_collection (xmms_value_t *val, xmmsc_coll_t **c)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (val->type != XMMS_VALUE_TYPE_COLL) {
		return 0;
	}

	*c = val->value.coll;

	return 1;
}

/**
 * Retrives binary data from the resultset.
 * @param val a #xmms_value_t containing a string.
 * @param r the return data. This data is owned by the value and will be freed when the value is freed.
 * @param rlen the return length of data.
 * @return 1 upon success otherwise 0
 */
int
xmms_value_get_bin (xmms_value_t *val, unsigned char **r, unsigned int *rlen)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (val->type != XMMS_VALUE_TYPE_BIN) {
		return 0;
	}

	*r = val->value.bin->data;
	*rlen = val->value.bin->len;

	return 1;
}



static xmms_value_t *
xmms_value_dict_lookup (xmms_value_t *val, const char *key)
{
	if (val->type == XMMS_VALUE_TYPE_DICT) {
		return plaindict_lookup (val, key);
	} else if (val->type == XMMS_VALUE_TYPE_PROPDICT) {
		return propdict_lookup (val, key);
	}

	return NULL;
}

/**
 * Retrieve integer associated for specified key in the resultset.
 *
 * If the key doesn't exist in the value the returned integer is
 * undefined.
 *
 * @param val a #xmms_value_t containing dict list.
 * @param key Key that should be retrieved
 * @param r the return int
 * @return 1 upon success otherwise 0
 *
 */
int
xmms_value_get_dict_entry_int (xmms_value_t *val, const char *key, int32_t *r)
{
	xmms_value_t *v;
	if (!val || val->error != XMMS_ERROR_NONE) {
		*r = -1;
		return 0;
	}

	if (val->type != XMMS_VALUE_TYPE_DICT &&
	    val->type != XMMS_VALUE_TYPE_PROPDICT) {
		*r = -1;
		return 0;
	}

	v = xmms_value_dict_lookup (val, key);
	if (v && v->type == XMMS_VALUE_TYPE_INT32) {
		*r = v->value.int32;
	} else {
		*r = -1;
		return 0;
	}

	return 1;
}

/**
 * Retrieve unsigned integer associated for specified key in the resultset.
 *
 * If the key doesn't exist in the value the returned integer is
 * undefined.
 *
 * @param val a #xmms_value_t containing a hashtable.
 * @param key Key that should be retrieved
 * @param r the return uint
 * @return 1 upon success otherwise 0
 *
 */
int
xmms_value_get_dict_entry_uint (xmms_value_t *val, const char *key, uint32_t *r)
{
	xmms_value_t *v;
	if (!val || val->error != XMMS_ERROR_NONE) {
		*r = -1;
		return 0;
	}

	if (val->type != XMMS_VALUE_TYPE_DICT &&
	    val->type != XMMS_VALUE_TYPE_PROPDICT) {
		*r = -1;
		return 0;
	}

	v = xmms_value_dict_lookup (val, key);
	if (v && v->type == XMMS_VALUE_TYPE_UINT32) {
		*r = v->value.uint32;
	} else {
		*r = -1;
		return 0;
	}

	return 1;
}

/**
 * Retrieve string associated for specified key in the resultset.
 *
 * If the key doesn't exist in the value the returned string is
 * NULL. The string is owned by the value and will be freed when the
 * value is freed.
 *
 * @param val a #xmms_value_t containing a hashtable.
 * @param key Key that should be retrieved
 * @param r the return string (owned by value)
 * @return 1 upon success otherwise 0
 *
 */
int
xmms_value_get_dict_entry_string (xmms_value_t *val,
                                   const char *key, const char **r)
{
	xmms_value_t *v;
	if (!val || val->error != XMMS_ERROR_NONE) {
		*r = NULL;
		return 0;
	}

	if (val->type != XMMS_VALUE_TYPE_DICT &&
	    val->type != XMMS_VALUE_TYPE_PROPDICT) {
		*r = NULL;
		return 0;
	}

	v = xmms_value_dict_lookup (val, key);
	if (v && v->type == XMMS_VALUE_TYPE_STRING) {
		*r = v->value.string;
	} else {
		*r = NULL;
		return 0;
	}

	return 1;
}

/**
 * Retrieve collection associated for specified key in the resultset.
 *
 * If the key doesn't exist in the value the returned collection is
 * NULL. The collection is owned by the value and will be freed when the
 * value is freed.
 *
 * @param val a #xmms_value_t containing a hashtable.
 * @param key Key that should be retrieved
 * @param c the return collection (owned by value)
 * @return 1 upon success otherwise 0
 *
 */
int
xmms_value_get_dict_entry_collection (xmms_value_t *val, const char *key,
                                       xmmsc_coll_t **c)
{
	xmms_value_t *v;
	if (!val || val->error != XMMS_ERROR_NONE) {
		*c = NULL;
		return 0;
	}

	if (val->type != XMMS_VALUE_TYPE_DICT &&
	    val->type != XMMS_VALUE_TYPE_PROPDICT) {
		*c = NULL;
		return 0;
	}

	v = xmms_value_dict_lookup (val, key);
	if (v && v->type == XMMS_VALUE_TYPE_COLL) {
		*c = v->value.coll;
	} else {
		*c = NULL;
		return 0;
	}

	return 1;
}

/**
 * Retrieve type associated for specified key in the resultset.
 *
 * @param val a #xmms_value_t containing a hashtable.
 * @param key Key that should be retrieved
 * @return type of key
 *
 */
xmms_value_type_t
xmms_value_get_dict_entry_type (xmms_value_t *val, const char *key)
{
	xmms_value_t *v;
	if (!val || val->error != XMMS_ERROR_NONE) {
		return XMMS_VALUE_TYPE_NONE;
	}

	if (val->type != XMMS_VALUE_TYPE_DICT &&
	    val->type != XMMS_VALUE_TYPE_PROPDICT) {
		return XMMS_VALUE_TYPE_NONE;
	}

	v = xmms_value_dict_lookup (val, key);
	if (!v) {
		return XMMS_VALUE_TYPE_NONE;
	}

	return v->type;
}

int
xmms_value_propdict_foreach (xmms_value_t *val,
                              xmmsc_propdict_foreach_func func,
                              void *user_data)
{
	x_list_t *n;

	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (val->type != XMMS_VALUE_TYPE_PROPDICT) {
		x_print_err ("xmms_value_propdict_foreach", "on a normal dict!");
		return 0;
	}

	for (n = val->list; n; n = x_list_next (n)) {
		xmms_value_t *source = NULL;
		xmms_value_t *key = NULL;
		xmms_value_t *val = NULL;
		if (n->next && n->next->next) {
			source = n->data;
			key = n->next->data;
			val = n->next->next->data;
		}
		func ((const void *)key->value.string, val->type, (void *)val->value.string, source->value.string, user_data);
		n = x_list_next (n); /* skip key part */
		n = x_list_next (n); /* skip value part */
	}

	return 1;
}

/**
 * Iterate over all key/value-pair in the resultset.
 *
 * Calls specified function for each key/value-pair in the dictionary.
 *
 * void function (const void *key, #xmms_value_type_t type, const void *value, void *user_data);
 *
 * @param val a #xmms_value_t containing a dict.
 * @param func function that is called for each key/value-pair
 * @param user_data extra data passed to func
 * @return 1 upon success otherwise 0
 *
 */
int
xmms_value_dict_foreach (xmms_value_t *val, xmmsc_dict_foreach_func func, void *user_data)
{
	x_list_t *n;

	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (val->type != XMMS_VALUE_TYPE_DICT) {
		x_print_err ("xmms_value_dict_foreach", "on a source dict!");
		return 0;
	}

	if (val->type == XMMS_VALUE_TYPE_DICT) {
		for (n = val->value.dict; n; n = x_list_next (n)) {
			xmms_value_t *val = NULL;
			if (n->next) {
				val = n->next->data;
			}
			func ((const void *)n->data, val->type, (void *)val->value.string, user_data);
			n = x_list_next (n); /* skip value part */
		}
	}

	return 1;
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
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	return val->islist;
}

/**
 * Check if current listnode is inside list boundary.
 *
 * When xmms_value_list_valid returns 1, there is a list entry
 * available for access with xmms_value_get_{type}.
 *
 * @param val a #xmms_value_t that is a list.
 * @return 1 if inside, 0 otherwise
 */
int
xmms_value_list_valid (xmms_value_t *val)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (!val->islist) {
		return 0;
	}

	return !!val->current;
}

/**
 * Skip to next entry in list.
 *
 * Advances to next list entry. May advance outside of list, so
 * #xmms_value_list_valid should be used to determine if end of list
 * was reached.
 *
 * @param val a #xmms_value_t that is a list.
 * @return 1 upon succes, 0 otherwise
 */
int
xmms_value_list_next (xmms_value_t *val)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (!val->islist) {
		return 0;
	}

	if (!val->current) {
		return 0;
	}

	val->current = val->current->next;

	if (val->current) {
		xmms_value_t *val2 = val->current->data;
		val->value.generic = val2->value.generic;
		val->type = val2->type;
	} else {
		val->value.generic = NULL;
		val->type = XMMS_VALUE_TYPE_NONE;
	}

	return 1;
}

/**
 * Return to first entry in list.
 *
 * @param val a #xmms_value_t that is a list.
 * @return 1 upon succes, 0 otherwise
 */
int
xmms_value_list_first (xmms_value_t *val)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (!val->islist) {
		return 0;
	}

	val->current = val->list;

	if (val->current) {
		xmms_value_t *val = val->current->data;
		val->value.generic = val->value.generic;
		val->type = val->type;
	} else {
		val->value.generic = NULL;
		val->type = XMMS_VALUE_TYPE_NONE;
	}

	return 1;
}

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
const char *
xmms_value_decode_url (xmms_value_t *val, const char *string)
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

	if (val)
		val->extra_free = x_list_prepend (val->extra_free, url);

	return url;

 err:
	free (url);
	return NULL;
}

/** @} */



/** @internal */

static xmms_value_t *
plaindict_lookup (xmms_value_t *val, const char *key)
{
	x_list_t *n;

	for (n = val->value.dict; n; n = x_list_next (n)) {
		const char *k = n->data;
		if (strcasecmp (k, key) == 0 && n->next) {
			/* found right key, return value */
			return (xmms_value_t*) n->next->data;
		} else {
			/* skip data part of this entry */
			n = x_list_next (n);
		}
	}

	return NULL;
}

static xmms_value_t *
propdict_lookup (xmms_value_t *val, const char *key)
{
	x_list_t *n;
	const char **sources, **ptr;

	sources = val->source_pref ?
		(const char **) val->source_pref : default_source_pref;

	for (ptr = sources; *ptr; ptr++) {
		const char *source = *ptr;

		for (n = val->list; n; n = x_list_next (n)) {
			xmms_value_t *k = n->data;

			if (source_match_pattern (k->value.string, source) &&
			    n->next && n->next->next) {

				n = x_list_next (n);
				k = n->data;

				if (strcasecmp (k->value.string, key) == 0) {
					return (xmms_value_t*) n->next->data;
				} else {
					n = x_list_next (n);
				}

			} else {
				n = x_list_next (n);
				n = x_list_next (n);
			}
		}
	}

	return NULL;
}

static int
source_match_pattern (const char *source, const char *pattern)
{
	int match = 0;
	int lpos = strlen (pattern) - 1;

	if (strcasecmp (pattern, source) == 0) {
		match = 1;
	}
	else if (lpos >= 0 && pattern[lpos] == '*' &&
	        (lpos == 0 || strncasecmp (source, pattern, lpos) == 0)) {
		match = 1;
	}

	return match;
}
