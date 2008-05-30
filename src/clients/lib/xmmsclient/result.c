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
#include <sys/types.h>
#include <string.h>
#include <ctype.h>

#include <sys/types.h>
#include <assert.h>

#include "xmmsclient/xmmsclient.h"
#include "xmmsclientpriv/xmmsclient.h"
#include "xmmsclientpriv/xmmsclient_ipc.h"
#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_errorcodes.h"
#include "xmmsc/xmmsc_stdint.h"
#include "xmmsc/xmmsc_strlist.h"

static void free_dict_list (x_list_t *list);
static x_list_t *xmmsc_deserialize_dict (xmms_ipc_msg_t *msg);
static int source_match_pattern (const char *source, const char *pattern);

xmmsc_result_t *xmmsc_result_restart (xmmsc_result_t *res);
void xmmsc_result_disconnect (xmmsc_result_t *res);

typedef struct xmmsc_value_bin_St {
	unsigned char *data;
	uint32_t len;
} xmmsc_value_bin_t;

typedef struct xmmsc_result_callback_St {
	xmmsc_result_notifier_t func;
	void *user_data;
	xmmsc_user_data_free_func_t free_func;
	int want_restart; /* bool, set according to func retval */
} xmmsc_result_callback_t;

static xmmsc_result_callback_t *xmmsc_result_callback_new (xmmsc_result_notifier_t f, void *udata, xmmsc_user_data_free_func_t free_f);
static int xmmsc_result_callback_exec (xmmsc_result_callback_t *cb, xmmsc_value_t *val);
static void xmmsc_result_callback_free (xmmsc_result_callback_t *cb);

struct xmmsc_value_St {
	union {
		void *generic;
		uint32_t uint32;
		int32_t int32;
		char *string;
		x_list_t *dict;
		xmmsc_coll_t *coll;
		xmmsc_value_bin_t *bin;
	} value;
	xmmsc_value_type_t type;

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

static xmmsc_value_t *xmmsc_parse_value (xmms_ipc_msg_t *msg);
static void xmmsc_value_free (xmmsc_value_t *val);
static void xmmsc_value_seterror (xmmsc_value_t *val, const char *errstr);

struct xmmsc_result_St {
	xmmsc_connection_t *c;

	/** refcounting */
	int ref;

	xmmsc_result_type_t type;

	/** notifiers (as a list of xmmsc_result_notifier_data_t *) */
	x_list_t *notifiers;

	uint32_t cookie;
	uint32_t restart_signal;

	xmmsc_ipc_t *ipc;

	int parsed;

	xmmsc_value_t *data;
};

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
 * @defgroup Result Result
 * @brief Result manipulation and error handling
 * @ingroup XMMSClient
 *
 * Each command to the server will return a #xmmsc_result_t
 * to the programmer. This object will be used to see the results
 * off the call. It will handle errors and the results.
 *
 * results could be used in both sync and async fashions. Here
 * is a sync example:
 * @code
 * xmmsc_result_t *res;
 * xmmsc_value_t *val;
 * uint32_t id;
 * res = xmmsc_playback_get_current_id (connection);
 * xmmsc_result_wait (res);
 * if (!val = xmmsc_result_get_value (res)) {
 *   printf ("error: failed to retrieve value!");
 * }
 * if (xmmsc_value_iserror (val)) {
 *   printf ("error: %s", xmmsc_value_get_error (val));
 * }
 * xmmsc_value_get_uint (val, &id);
 * xmmsc_result_unref (res);
 * printf ("current id is: %d", id);
 * @endcode
 *
 * an async example is a bit more complex...
 * @code
 * static void handler (xmmsc_value_t *val, void *userdata) {
 *   uint32_t id;
 *   if (xmmsc_value_iserror (val)) {
 *      printf ("error: %s", xmmsc_value_get_error (val));
 *   }
 *   xmmsc_value_get_uint (val, &id);
 *   printf ("current id is: %d", id);
 * }
 *
 * int main () {
 *   // Connect blah blah ...
 *   xmmsc_result_t *res;
 *   res = xmmsc_playback_get_current_id (connection);
 *   xmmsc_result_notifier_set (res, handler);
 *   xmmsc_result_unref (res);
 * }
 * @endcode
 * When the answer arrives handler will be called. with the resulting #xmmsc_value_t
 * @{
**/

/**
 * References the #xmmsc_result_t
 *
 * @param result the result to reference.
 * @return result
 */
xmmsc_result_t *
xmmsc_result_ref (xmmsc_result_t *res)
{
	x_return_val_if_fail (res, NULL);
	res->ref++;

	return res;
}

/**
 * @todo Deallocate all types here
 */
static void
xmmsc_result_free (xmmsc_result_t *res)
{
	x_list_t *n;

	x_return_if_fail (res);

	/* Free memory! */

	xmmsc_ipc_result_unregister (res->ipc, res);

	xmmsc_unref (res->c);

	xmmsc_value_unref (res->data);

	for (n = res->notifiers; n; n = x_list_next (n)) {
		xmmsc_result_callback_t *cb = n->data;
		xmmsc_result_callback_free (cb);
	}

	x_list_free (res->notifiers);

	free (res);
}

/**
 * Get the class of the result (default, signal, broadcast).
 * @returns The class of the result of type #xmmsc_result_type_t
 */
xmmsc_result_type_t
xmmsc_result_get_class (xmmsc_result_t *res)
{
	x_return_val_if_fail (res, XMMSC_RESULT_CLASS_DEFAULT);

	return res->type;
}

/**
 * Check the #xmmsc_value_t for error.
 * @return 1 if error was encountered, else 0
 */
int
xmmsc_value_iserror (xmmsc_value_t *val)
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
xmmsc_value_get_error (xmmsc_value_t *val)
{
	x_return_null_if_fail (val);

	return val->error_str;
}

/**
 * Disconnect a signal or a broadcast.
 * @param res The result to disconnect, must be of class signal or broadcast.
 */
void
xmmsc_result_disconnect (xmmsc_result_t *res)
{
	x_return_if_fail (res);

	switch (res->type) {
		case XMMSC_RESULT_CLASS_SIGNAL:
		case XMMSC_RESULT_CLASS_BROADCAST:
			xmmsc_result_unref (res);
			break;
		default:
			x_api_error_if (1, "invalid result type",);
	}
}

/**
 * A lot of signals you would like to get notified about
 * when they change, instead of polling the server all the time.
 * This results are "restartable".
 * Here is an example on how you use a restartable signal.
 * @code
 * static void handler (xmmsc_result_t *res, void *userdata) {
 *   uint32_t id;
 *   xmmsc_result_t *newres;
 *
 *   if (xmmsc_result_iserror) {
 *      printf ("error: %s", xmmsc_result_get_error (res);
 *   }
 *
 *   xmmsc_result_get_uint (res, &id);
 *   newres = xmmsc_result_restart (res); // this tells the server to send updates to the SAME function again.
 *   xmmsc_result_unref (res);
 *   xmmsc_result_unref (newres);
 *   printf ("current id is: %d", id);
 * }
 *
 * int main () {
 *   // Connect blah blah ...
 *   xmmsc_result_t *res;
 *   res = xmmsc_signal_playback_playtime (connection);
 *   xmmsc_result_notifier_set (res, handler);
 *   xmmsc_result_unref (res);
 * }
 * @endcode
 * In the above example the handler would be called when the playtime is updated.
 * Only signals are restatable. Broadcasts will automaticly restart.
 */
xmmsc_result_t *
xmmsc_result_restart (xmmsc_result_t *res)
{
	xmmsc_result_t *newres;
	xmms_ipc_msg_t *msg;
	x_list_t *n;

	x_return_null_if_fail (res);
	x_return_null_if_fail (res->c);

	x_api_error_if (res->type != XMMSC_RESULT_CLASS_SIGNAL,
	                "result is not restartable", NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_SIGNAL, XMMS_IPC_CMD_SIGNAL);
	xmms_ipc_msg_put_uint32 (msg, res->restart_signal);

	newres = xmmsc_send_msg (res->c, msg);

	for (n = res->notifiers; n; n = x_list_next (n)) {
		xmmsc_result_callback_t *cb = n->data;
		if (cb->want_restart) {
			xmmsc_result_notifier_set_full (newres, cb->func,
			                                cb->user_data,
			                                cb->free_func);
		}
	}
	xmmsc_result_restartable (newres, res->restart_signal);

	return newres;
}

/**
 * References the #xmmsc_value_t
 *
 * @param val the value to reference.
 * @return val
 */
xmmsc_value_t *
xmmsc_value_ref (xmmsc_value_t *val)
{
	x_return_val_if_fail (val, NULL);
	val->ref++;

	return val;
}

/**
 * Decreases the references for the #xmmsc_value_t
 * When the number of references reaches 0 it will
 * be freed. And thus all data you extracted from it
 * will be deallocated.
 */
void
xmmsc_value_unref (xmmsc_value_t *val)
{
	x_return_if_fail (val);
	x_api_error_if (val->ref < 1, "with a freed value",);

	val->ref--;
	if (val->ref == 0) {
		xmmsc_value_free (val);
	}
}

/**
 * Allocates new #xmmsc_result_t and refereces it.
 * Should not be used from a client.
 * @internal
 */
xmmsc_value_t *
xmmsc_value_new ()
{
	xmmsc_value_t *val;

	val = x_new0 (xmmsc_value_t, 1);
	if (!val) {
		x_oom ();
		return NULL;
	}

	xmmsc_value_ref (val);
	return val;
}

static void
xmmsc_value_free (xmmsc_value_t *val)
{
	x_return_if_fail (val);

	if (val->islist) {
		val->type = XMMSC_VALUE_TYPE_LIST;
	}

	switch (val->type) {
		case XMMSC_VALUE_TYPE_NONE :
		case XMMSC_VALUE_TYPE_UINT32 :
		case XMMSC_VALUE_TYPE_INT32 :
			break;
		case XMMSC_VALUE_TYPE_STRING :
			free (val->value.string);
			val->value.string = NULL;
			break;
		case XMMSC_VALUE_TYPE_BIN :
			free (val->value.bin->data);
			free (val->value.bin);
			val->value.bin = NULL;
			break;
		case XMMSC_VALUE_TYPE_LIST:
		case XMMSC_VALUE_TYPE_PROPDICT:
			while (val->list) {
				xmmsc_value_unref ((xmmsc_value_t *) val->list->data);
				val->list = x_list_delete_link (val->list, val->list);
			}
			break;
		case XMMSC_VALUE_TYPE_DICT:
			free_dict_list (val->value.dict);
			val->value.dict = NULL;
			break;
		case XMMSC_VALUE_TYPE_COLL:
			xmmsc_coll_unref (val->value.coll);
			val->value.coll = NULL;
			break;
	}

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

static bool
xmmsc_result_parse_msg (xmmsc_result_t *res, xmms_ipc_msg_t *msg)
{
	int type;
	x_list_t *list = NULL;

	if (xmmsc_value_iserror (res->data)) {
		res->parsed = true;
		return true;
	}

	if (!xmms_ipc_msg_get_int32 (msg, &type))
		return false;

	res->data->type = type;

	switch (type) {

		case XMMSC_VALUE_TYPE_UINT32 :
			if (!xmms_ipc_msg_get_uint32 (msg, &res->data->value.uint32)) {
				return false;
			}
			break;
		case XMMSC_VALUE_TYPE_INT32 :
			if (!xmms_ipc_msg_get_int32 (msg, &res->data->value.int32)) {
				return false;
			}
			break;
		case XMMSC_VALUE_TYPE_BIN:
			{
				xmmsc_value_bin_t *bin;
				bin = x_new0 (xmmsc_value_bin_t, 1);
				if (!xmms_ipc_msg_get_bin_alloc (msg, &bin->data, &bin->len)) {
					free (bin);
					return false;
				}
				res->data->value.bin = bin;
				break;
			}
		case XMMSC_VALUE_TYPE_STRING :
			{
				uint32_t len;

				if (!xmms_ipc_msg_get_string_alloc (msg,
				                                    &res->data->value.string,
				                                    &len)) {
					return false;
				}
			}
			break;
		case XMMSC_VALUE_TYPE_DICT:
			{
				x_list_t *dict;

				dict = xmmsc_deserialize_dict (msg);
				if (!dict)
					return false;

				res->data->value.dict = dict;

			}
			break;
		case XMMSC_VALUE_TYPE_LIST :
		case XMMSC_VALUE_TYPE_PROPDICT :
			{
				uint32_t len, i;

				if (!xmms_ipc_msg_get_uint32 (msg, &len))
					return false;

				for (i = 0; i < len; i ++) {
					xmmsc_value_t *val;
					val = xmmsc_parse_value (msg);
					list = x_list_prepend (list, val);
				}

				if (list)
					list = x_list_reverse (list);

				res->data->current = res->data->list = list;

				if (type == XMMSC_VALUE_TYPE_LIST) {
					res->data->islist = 1;

					if (res->data->current) {
						xmmsc_value_t *val = res->data->current->data;
						res->data->value.generic = val->value.generic;
						res->data->type = val->type;
					} else {
						res->data->value.generic = NULL;
						res->data->type = XMMSC_VALUE_TYPE_NONE;
					}
				}
			}
			break;

		case XMMSC_VALUE_TYPE_COLL:
			{
				xmmsc_coll_t *coll;

				if (!xmms_ipc_msg_get_collection_alloc (msg, &coll))
					return false;

				res->data->value.coll = coll;
				xmmsc_coll_ref (res->data->value.coll);
			}
			break;

		case XMMSC_VALUE_TYPE_NONE :
			break;

		default :
			return false;
	}

	res->parsed = true;

	return true;
}


/**
 * return the cookie of a resultset.
 */
uint32_t
xmmsc_result_cookie_get (xmmsc_result_t *res)
{
	x_return_val_if_fail (res, 0);

	return res->cookie;
}

/**
 * Decreases the references for the #xmmsc_result_t
 * When the number of references reaches 0 it will
 * be freed. And thus all data you extracted from it
 * will be deallocated.
 */

void
xmmsc_result_unref (xmmsc_result_t *res)
{
	x_return_if_fail (res);
	x_api_error_if (res->ref < 1, "with a freed result",);

	res->ref--;
	if (res->ref == 0) {
		xmmsc_result_free (res);
	}
}

/**
 * Set up a callback for the result retrival. This callback
 * Will be called when the answers arrives.
 * @param res a #xmmsc_result_t that you got from a command dispatcher.
 * @param func the function that should be called when we receive the answer
 * @param user_data optional user data to the callback
 */

void
xmmsc_result_notifier_set (xmmsc_result_t *res, xmmsc_result_notifier_t func, void *user_data)
{
	xmmsc_result_notifier_set_full (res, func, user_data, NULL);
}

/**
 * Set up a callback for the result retrieval. This callback
 * will be called when the answer arrives. This function differs from
 * xmmsc_result_notifier_set in the additional free_func parameter,
 * which allows to pass a pointer to a function which will be called
 * to free the user_data when needed.
 * @param res a #xmmsc_result_t that you got from a command dispatcher.
 * @param func the function that should be called when we receive the answer
 * @param user_data optional user data to the callback
 * @param free_func optional function that should be called to free the user_data
 */

void
xmmsc_result_notifier_set_full (xmmsc_result_t *res, xmmsc_result_notifier_t func,
                                void *user_data, xmmsc_user_data_free_func_t free_func)
{
	xmmsc_result_callback_t *cb;

	x_return_if_fail (res);
	x_return_if_fail (func);

	/* The pending call takes one ref */
	xmmsc_result_ref (res);

	cb = xmmsc_result_callback_new (func, user_data, free_func);
	res->notifiers = x_list_append (res->notifiers, cb);
}


/**
 * Block for the reply. In a synchronous application this
 * can be used to wait for the result. Will return when
 * the server replyed.
 */

void
xmmsc_result_wait (xmmsc_result_t *res)
{
	const char *err = NULL;
	x_return_if_fail (res);

	while (!res->parsed && !(err = xmmsc_ipc_error_get (res->ipc))) {
		xmmsc_ipc_wait_for_event (res->ipc, 5);
	}

	if (err) {
		xmmsc_value_seterror (res->data, err);
	}
}

/**
 * Set sources to be used when fetching stuff from a propdict.
 * @param val a #xmmsc_value_t that you got from a command dispatcher.
 * @param preference a list of sources from most to least preferrable.
 * You may use a wildcard "*" character.
 */
void
xmmsc_value_source_preference_set (xmmsc_value_t *val, const char **preference)
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
 * @param val a #xmmsc_value_t that you got from a command dispatcher.
 * @returns The current sources from most to least preferable, as a
 * NULL-terminated array of immutable strings.
 * This array is owned by the value and will be freed with it.
 */
const char **
xmmsc_value_source_preference_get (xmmsc_value_t *val)
{
	x_return_val_if_fail (val, NULL);

	if (val->source_pref)
		return (const char **) val->source_pref;
	else
		return default_source_pref;
}

/**
 * @defgroup ResultValueRetrieval ResultValueRetrieval
 * @ingroup Result
 * @brief Explains how you can retrive values from a #xmmsc_result_t
 * @{
 */

/**
 * Get the value from a result.
 * @param res a #xmmsc_result_t containing the value.
 * @returns The value received by the result.
 */
xmmsc_value_t *
xmmsc_result_get_value (xmmsc_result_t *res)
{
	x_return_val_if_fail (res, NULL);
	x_return_val_if_fail (res->parsed, NULL);

	return res->data;
}

/**
 * Get the type of the value.
 * @param val a #xmmsc_value_t to get the type from.
 * @returns The data type in the value.
 */
xmmsc_value_type_t
xmmsc_value_get_type (xmmsc_value_t *val)
{
	x_api_error_if (!val, "NULL value",
	                XMMSC_VALUE_TYPE_NONE);

	return val->type;
}

/**
 * Retrives a signed integer from the value.
 * @param val a #xmmsc_value_t containing a integer.
 * @param r the return integer.
 * @return 1 upon success otherwise 0
 */

int
xmmsc_value_get_int (xmmsc_value_t *val, int32_t *r)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (val->type != XMMSC_VALUE_TYPE_INT32) {
		return 0;
	}

	*r = val->value.int32;

	return 1;
}

/**
 * Retrives a unsigned integer from the resultset.
 * @param val a #xmmsc_value_t containing a integer.
 * @param r the return integer.
 * @return 1 upon success otherwise 0
 */

int
xmmsc_value_get_uint (xmmsc_value_t *val, uint32_t *r)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (val->type != XMMSC_VALUE_TYPE_UINT32)
		return 0;

	*r = val->value.uint32;

	return 1;
}

/**
 * Retrives a string from the resultset.
 * @param val a #xmmsc_value_t containing a string.
 * @param r the return string. This string is owned by the value and will be freed when the value is freed.
 * @return 1 upon success otherwise 0
 */
int
xmmsc_value_get_string (xmmsc_value_t *val, const char **r)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (val->type != XMMSC_VALUE_TYPE_STRING) {
		return 0;
	}

	*r = val->value.string;

	return 1;
}

/**
 * Retrieves a collection from the resultset.
 * @param val a #xmmsc_value_t containing a collection.
 * @param c the return collection. This collection is owned by the value and will be freed when the value is freed.
 * @return 1 upon success otherwise 0
 */
int
xmmsc_value_get_collection (xmmsc_value_t *val, xmmsc_coll_t **c)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (val->type != XMMSC_VALUE_TYPE_COLL) {
		return 0;
	}

	*c = val->value.coll;

	return 1;
}

/**
 * Retrives binary data from the resultset.
 * @param val a #xmmsc_value_t containing a string.
 * @param r the return data. This data is owned by the value and will be freed when the value is freed.
 * @param rlen the return length of data.
 * @return 1 upon success otherwise 0
 */
int
xmmsc_value_get_bin (xmmsc_value_t *val, unsigned char **r, unsigned int *rlen)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (val->type != XMMSC_VALUE_TYPE_BIN) {
		return 0;
	}

	*r = val->value.bin->data;
	*rlen = val->value.bin->len;

	return 1;
}

static xmmsc_value_t *
plaindict_lookup (xmmsc_value_t *val, const char *key)
{
	x_list_t *n;

	for (n = val->value.dict; n; n = x_list_next (n)) {
		const char *k = n->data;
		if (strcasecmp (k, key) == 0 && n->next) {
			/* found right key, return value */
			return (xmmsc_value_t*) n->next->data;
		} else {
			/* skip data part of this entry */
			n = x_list_next (n);
		}
	}

	return NULL;
}

static xmmsc_value_t *
propdict_lookup (xmmsc_value_t *val, const char *key)
{
	x_list_t *n;
	const char **sources, **ptr;

	sources = val->source_pref ?
		(const char **) val->source_pref : default_source_pref;

	for (ptr = sources; *ptr; ptr++) {
		const char *source = *ptr;

		for (n = val->list; n; n = x_list_next (n)) {
			xmmsc_value_t *k = n->data;

			if (source_match_pattern (k->value.string, source) &&
			    n->next && n->next->next) {

				n = x_list_next (n);
				k = n->data;

				if (strcasecmp (k->value.string, key) == 0) {
					return (xmmsc_value_t*) n->next->data;
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

static xmmsc_value_t *
xmmsc_value_dict_lookup (xmmsc_value_t *val, const char *key)
{
	if (val->type == XMMSC_VALUE_TYPE_DICT) {
		return plaindict_lookup (val, key);
	} else if (val->type == XMMSC_VALUE_TYPE_PROPDICT) {
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
 * @param val a #xmmsc_value_t containing dict list.
 * @param key Key that should be retrieved
 * @param r the return int
 * @return 1 upon success otherwise 0
 *
 */
int
xmmsc_value_get_dict_entry_int (xmmsc_value_t *val, const char *key, int32_t *r)
{
	xmmsc_value_t *v;
	if (!val || val->error != XMMS_ERROR_NONE) {
		*r = -1;
		return 0;
	}

	if (val->type != XMMSC_VALUE_TYPE_DICT &&
	    val->type != XMMSC_VALUE_TYPE_PROPDICT) {
		*r = -1;
		return 0;
	}

	v = xmmsc_value_dict_lookup (val, key);
	if (v && v->type == XMMSC_VALUE_TYPE_INT32) {
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
 * @param val a #xmmsc_value_t containing a hashtable.
 * @param key Key that should be retrieved
 * @param r the return uint
 * @return 1 upon success otherwise 0
 *
 */
int
xmmsc_value_get_dict_entry_uint (xmmsc_value_t *val, const char *key, uint32_t *r)
{
	xmmsc_value_t *v;
	if (!val || val->error != XMMS_ERROR_NONE) {
		*r = -1;
		return 0;
	}

	if (val->type != XMMSC_VALUE_TYPE_DICT &&
	    val->type != XMMSC_VALUE_TYPE_PROPDICT) {
		*r = -1;
		return 0;
	}

	v = xmmsc_value_dict_lookup (val, key);
	if (v && v->type == XMMSC_VALUE_TYPE_UINT32) {
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
 * @param val a #xmmsc_value_t containing a hashtable.
 * @param key Key that should be retrieved
 * @param r the return string (owned by value)
 * @return 1 upon success otherwise 0
 *
 */
int
xmmsc_value_get_dict_entry_string (xmmsc_value_t *val,
                                   const char *key, const char **r)
{
	xmmsc_value_t *v;
	if (!val || val->error != XMMS_ERROR_NONE) {
		*r = NULL;
		return 0;
	}

	if (val->type != XMMSC_VALUE_TYPE_DICT &&
	    val->type != XMMSC_VALUE_TYPE_PROPDICT) {
		*r = NULL;
		return 0;
	}

	v = xmmsc_value_dict_lookup (val, key);
	if (v && v->type == XMMSC_VALUE_TYPE_STRING) {
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
 * @param val a #xmmsc_value_t containing a hashtable.
 * @param key Key that should be retrieved
 * @param c the return collection (owned by value)
 * @return 1 upon success otherwise 0
 *
 */
int
xmmsc_value_get_dict_entry_collection (xmmsc_value_t *val, const char *key,
                                       xmmsc_coll_t **c)
{
	xmmsc_value_t *v;
	if (!val || val->error != XMMS_ERROR_NONE) {
		*c = NULL;
		return 0;
	}

	if (val->type != XMMSC_VALUE_TYPE_DICT &&
	    val->type != XMMSC_VALUE_TYPE_PROPDICT) {
		*c = NULL;
		return 0;
	}

	v = xmmsc_value_dict_lookup (val, key);
	if (v && v->type == XMMSC_VALUE_TYPE_COLL) {
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
 * @param val a #xmmsc_value_t containing a hashtable.
 * @param key Key that should be retrieved
 * @return type of key
 *
 */
xmmsc_value_type_t
xmmsc_value_get_dict_entry_type (xmmsc_value_t *val, const char *key)
{
	xmmsc_value_t *v;
	if (!val || val->error != XMMS_ERROR_NONE) {
		return XMMSC_VALUE_TYPE_NONE;
	}

	if (val->type != XMMSC_VALUE_TYPE_DICT &&
	    val->type != XMMSC_VALUE_TYPE_PROPDICT) {
		return XMMSC_VALUE_TYPE_NONE;
	}

	v = xmmsc_value_dict_lookup (val, key);
	if (!v) {
		return XMMSC_VALUE_TYPE_NONE;
	}

	return v->type;
}

int
xmmsc_value_propdict_foreach (xmmsc_value_t *val,
                              xmmsc_propdict_foreach_func func,
                              void *user_data)
{
	x_list_t *n;

	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (val->type != XMMSC_VALUE_TYPE_PROPDICT) {
		x_print_err ("xmms_value_propdict_foreach", "on a normal dict!");
		return 0;
	}

	for (n = val->list; n; n = x_list_next (n)) {
		xmmsc_value_t *source = NULL;
		xmmsc_value_t *key = NULL;
		xmmsc_value_t *val = NULL;
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
 * void function (const void *key, #xmmsc_value_type_t type, const void *value, void *user_data);
 *
 * @param val a #xmmsc_value_t containing a dict.
 * @param func function that is called for each key/value-pair
 * @param user_data extra data passed to func
 * @return 1 upon success otherwise 0
 *
 */
int
xmmsc_value_dict_foreach (xmmsc_value_t *val, xmmsc_dict_foreach_func func, void *user_data)
{
	x_list_t *n;

	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (val->type != XMMSC_VALUE_TYPE_DICT) {
		x_print_err ("xmms_value_dict_foreach", "on a source dict!");
		return 0;
	}

	if (val->type == XMMSC_VALUE_TYPE_DICT) {
		for (n = val->value.dict; n; n = x_list_next (n)) {
			xmmsc_value_t *val = NULL;
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
 * @param val a #xmmsc_value_t
 * @return 1 if value stores a list, 0 otherwise.
 */
int
xmmsc_value_is_list (xmmsc_value_t *val)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	return val->islist;
}

/**
 * Check if current listnode is inside list boundary.
 *
 * When xmmsc_value_list_valid returns 1, there is a list entry
 * available for access with xmmsc_value_get_{type}.
 *
 * @param val a #xmmsc_value_t that is a list.
 * @return 1 if inside, 0 otherwise
 */
int
xmmsc_value_list_valid (xmmsc_value_t *val)
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
 * #xmmsc_value_list_valid should be used to determine if end of list
 * was reached.
 *
 * @param val a #xmmsc_value_t that is a list.
 * @return 1 upon succes, 0 otherwise
 */
int
xmmsc_value_list_next (xmmsc_value_t *val)
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
		xmmsc_value_t *val2 = val->current->data;
		val->value.generic = val2->value.generic;
		val->type = val2->type;
	} else {
		val->value.generic = NULL;
		val->type = XMMSC_VALUE_TYPE_NONE;
	}

	return 1;
}

/**
 * Return to first entry in list.
 *
 * @param val a #xmmsc_value_t that is a list.
 * @return 1 upon succes, 0 otherwise
 */
int
xmmsc_value_list_first (xmmsc_value_t *val)
{
	if (!val || val->error != XMMS_ERROR_NONE) {
		return 0;
	}

	if (!val->islist) {
		return 0;
	}

	val->current = val->list;

	if (val->current) {
		xmmsc_value_t *val = val->current->data;
		val->value.generic = val->value.generic;
		val->type = val->type;
	} else {
		val->value.generic = NULL;
		val->type = XMMSC_VALUE_TYPE_NONE;
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
 * @param val the #xmmsc_value_t that the string comes from
 * @param string the url encoded string
 * @return decoded string, owned by the #xmmsc_value_t
 *
 */
const char *
xmmsc_value_decode_url (xmmsc_value_t *val, const char *string)
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

/** @} */

/** @internal */

static void
xmmsc_value_seterror (xmmsc_value_t *val, const char *errstr)
{
	val->error_str = strdup (errstr);
	val->error = 1;
}

/* Kept as a proxy for external use */
void
xmmsc_result_seterror (xmmsc_result_t *res, const char *errstr)
{
	xmmsc_value_seterror (res->data, errstr);
}

void
xmmsc_result_restartable (xmmsc_result_t *res, uint32_t signalid)
{
	x_return_if_fail (res);

	res->restart_signal = signalid;
}

/**
 * @internal
 */
void
xmmsc_result_run (xmmsc_result_t *res, xmms_ipc_msg_t *msg)
{
	x_list_t *n;
	int cmd;
	xmmsc_result_t *restart_res;
	int need_restart = 0;

	x_return_if_fail (res);
	x_return_if_fail (msg);

	if (!xmmsc_result_parse_msg (res, msg)) {
		xmms_ipc_msg_destroy (msg);
		return;
	}

	cmd = xmms_ipc_msg_get_cmd (msg);

	xmms_ipc_msg_destroy (msg);

	xmmsc_result_ref (res);
/*	xmmsc_value_ref (res->data); */

	/* Run all notifiers and check for positive return values */
	for (n = res->notifiers; n; n = x_list_next (n)) {
		xmmsc_result_callback_t *cb = n->data;
		if (cb && xmmsc_result_callback_exec (cb, res->data)) {
			need_restart = 1;
		}
	}

	if (need_restart && cmd == XMMS_IPC_CMD_SIGNAL) {
		restart_res = xmmsc_result_restart (res);
		xmmsc_result_unref (restart_res);
	} else if (!need_restart && cmd == XMMS_IPC_CMD_BROADCAST) {
		xmmsc_result_disconnect (res);
	}

	if (cmd == XMMS_IPC_CMD_BROADCAST) {
		/* We keep the results alive with broadcasts, but we
		   just unref the value because it went out of scope. */
		xmmsc_value_unref (res->data);
	} else {
		/* Automatically cleanup result if not a broadcast! */
		xmmsc_result_unref (res);
	}

/*	xmmsc_result_unref (res); */
}

/**
 * Allocates new #xmmsc_result_t and refereces it.
 * Should not be used from a client.
 * @internal
 */

xmmsc_result_t *
xmmsc_result_new (xmmsc_connection_t *c, xmmsc_result_type_t type,
                  uint32_t cookie)
{
	xmmsc_result_t *res;

	res = x_new0 (xmmsc_result_t, 1);
	if (!res) {
		x_oom ();
		return NULL;
	}

	res->c = xmmsc_ref (c);

	res->data = xmmsc_value_new ();
	res->type = type;
	res->cookie = cookie;

	/* user must give this back */
	xmmsc_result_ref (res);

	/* Add it to the loop */
	xmmsc_ipc_result_register (c->ipc, res);

	/* For the destroy func */
	res->ipc = c->ipc;

	return res;
}

static xmmsc_value_t *
xmmsc_parse_value (xmms_ipc_msg_t *msg)
{
	xmmsc_value_t *val;
	uint32_t len;

	val = xmmsc_value_new ();

	if (!xmms_ipc_msg_get_int32 (msg, (int32_t *)&val->type)) {
		goto err;
	}

	switch (val->type) {
		case XMMSC_VALUE_TYPE_STRING:
			if (!xmms_ipc_msg_get_string_alloc (msg, &val->value.string, &len)) {
				goto err;
			}
			break;
		case XMMSC_VALUE_TYPE_UINT32:
			if (!xmms_ipc_msg_get_uint32 (msg, &val->value.uint32)) {
				goto err;
			}
			break;
		case XMMSC_VALUE_TYPE_INT32:
			if (!xmms_ipc_msg_get_int32 (msg, &val->value.int32)) {
				goto err;
			}
			break;
		case XMMSC_VALUE_TYPE_DICT:
			val->value.dict = xmmsc_deserialize_dict (msg);
			if (!val->value.dict) {
				goto err;
			}
			break;
		case XMMSC_VALUE_TYPE_COLL:
			xmms_ipc_msg_get_collection_alloc (msg, &val->value.coll);
			if (!val->value.coll) {
				goto err;
			}
			xmmsc_coll_ref (val->value.coll);
			break;
		case XMMSC_VALUE_TYPE_NONE:
			break;
		default:
			goto err;
			break;
	}

	return val;

err:
	x_internal_error ("Message from server did not parse correctly!");
	free (val);
	return NULL;

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
		xmmsc_value_t *val;

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

		xmmsc_value_unref ((xmmsc_value_t *) list->data); /* value */
		list = x_list_delete_link (list, list);
	}
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


static xmmsc_result_callback_t *
xmmsc_result_callback_new (xmmsc_result_notifier_t f, void *udata,
                           xmmsc_user_data_free_func_t free_f)
{
	xmmsc_result_callback_t *cb;

	cb = x_new0 (xmmsc_result_callback_t, 1);
	cb->func = f;
	cb->user_data = udata;
	cb->free_func = free_f;

	return cb;
}

/* Run the registered notifier and return its retval. */
static int
xmmsc_result_callback_exec (xmmsc_result_callback_t *cb, xmmsc_value_t *val)
{
	cb->want_restart = cb->func (val, cb->user_data);

	return cb->want_restart;
}

static void
xmmsc_result_callback_free (xmmsc_result_callback_t *cb)
{
	/* Don't free if it's gonna be restarted. */
	if (!cb->want_restart && cb->free_func) {
		cb->free_func (cb->user_data);
	}

	free (cb);
}
