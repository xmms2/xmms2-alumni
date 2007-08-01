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

#include "xmmsclient/xmmsclient.h"
#include "xmmsclientpriv/xmmsclient.h"
#include "xmmsclientpriv/xmmsclient_ipc.h"
#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_errorcodes.h"
#include "xmmsc/xmmsc_stdint.h"
#include "xmmsc/xmmsc_strlist.h"

xmmsc_result_t *xmmsc_result_restart (xmmsc_result_t *res);
void xmmsc_result_disconnect (xmmsc_result_t *res);
static void xmmsc_result_notifier_remove (xmmsc_result_t *res, x_list_t *node);
static void xmmsc_result_notifier_delete (xmmsc_result_t *res, x_list_t *node);

typedef struct xmmsc_result_callback_St {
	xmmsc_result_notifier_t func;
	void *user_data;
	xmmsc_user_data_free_func_t free_func;
} xmmsc_result_callback_t;

static xmmsc_result_callback_t *xmmsc_result_callback_new (xmmsc_result_notifier_t f, void *udata, xmmsc_user_data_free_func_t free_f);

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

	xmmsv_t *data;
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
 * xmmsv_t *val;
 * uint32_t id;
 * res = xmmsc_playback_get_current_id (connection);
 * xmmsc_result_wait (res);
 * if (!val = xmmsc_result_get_value (res)) {
 *   printf ("error: failed to retrieve value!");
 * }
 * if (xmmsv_iserror (val)) {
 *   printf ("error: %s", xmmsv_get_error (val));
 * }
 * xmmsv_get_uint (val, &id);
 * xmmsc_result_unref (res);
 * printf ("current id is: %d", id);
 * @endcode
 *
 * an async example is a bit more complex...
 * @code
 * static void handler (xmmsv_t *val, void *userdata) {
 *   uint32_t id;
 *   if (xmmsv_iserror (val)) {
 *      printf ("error: %s", xmmsv_get_error (val));
 *   }
 *   xmmsv_get_uint (val, &id);
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
 * When the answer arrives handler will be called. with the resulting #xmmsv_t
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
	x_list_t *n, *next;

	x_return_if_fail (res);

	/* Free memory! */

	xmmsc_ipc_result_unregister (res->ipc, res);

	xmmsc_unref (res->c);

	if (res->data) {
		xmmsv_unref (res->data);
	}

	n = res->notifiers;
	while (n) {
		next = x_list_next (n);
		xmmsc_result_notifier_delete (res, n);
		n = next;
	}

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

		xmmsc_result_notifier_set_full (newres, cb->func,
		                                cb->user_data,
		                                cb->free_func);
	}
	xmmsc_result_restartable (newres, res->restart_signal);

	return newres;
}

static bool
xmmsc_result_parse_msg (xmmsc_result_t *res, xmms_ipc_msg_t *msg)
{
	if (xmms_ipc_msg_get_cmd (msg) == XMMS_IPC_CMD_ERROR) {
		/* If special error msg, extract the error and save in result */
		char *errstr;
		uint32_t len;

		if (!xmms_ipc_msg_get_string_alloc (msg, &errstr, &len)) {
			xmmsc_result_seterror (res, "No errormsg!");
		} else {
			xmmsc_result_seterror (res, errstr);
			free (errstr);
		}

		res->parsed = true;
		return true;
	} else if (xmms_ipc_msg_get_value_alloc (msg, &res->data)) {
		/* Expected message data retrieved! */
		res->parsed = true;
		return true;
	} else {
		/* FIXME: shouldn't parsed be false then? */
		return false;
	}
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
		/* FIXME: xmmsv_unref (res->data) or not allocated ? */
		res->data = xmmsv_new_error (err);
	}
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
xmmsv_t *
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
 * Retrieve a method's information.
 *
 * Caller is responsible for freeing the list and all the elements, or simply
 * call the helper function #xmmsc_service_method_free.
 *
 * @param res The #xmmsc_result_t returned by #xmmsc_service_method_describe.
 * @param method The return method structure.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_result_get_service_method (xmmsc_result_t *res,
                                 xmmsc_service_method_t **method)
{
	char *name = NULL;
	char *desc = NULL;

	if (!res || res->error != XMMS_ERROR_NONE) {
		return 0;
	}

	*method = x_new0 (xmmsc_service_method_t, 1);

	if (!xmmsc_result_get_dict_entry_string (res, "name", &name)) {
		free (*method);
		return 0;
	}
	if (!xmmsc_result_get_dict_entry_string (res, "description", &desc)) {
		free (*method);
		return 0;
	}
	(*method)->name = x_new0 (char, strlen (name) + 1);
	strcpy ((*method)->name, name);
	(*method)->description = x_new0 (char, strlen (desc) + 1);
	strcpy ((*method)->description, desc);

	return 1;
}

/**
 * Retrieve a method's argument types.
 *
 * Caller is responsible for freeing the list and all the elements by calling
 * the helper function #xmmsc_service_method_free.
 *
 * @param res The #xmmsc_result_t returned by #xmmsc_service_method_args_list.
 * @param method The method which will contain the argument list.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_result_get_service_method_arg_types (xmmsc_result_t *res,
                                           xmmsc_service_method_t *method)
{
	char *name = NULL;
	x_list_t *n;
	xmmsc_service_argument_t *arg;

	if (!res || res->error != XMMS_ERROR_NONE || method->arg_list) {
		return 0;
	}

	while (xmmsc_result_list_valid (res)) {
		arg = x_new0 (xmmsc_service_argument_t, 1);

		if (!xmmsc_result_get_dict_entry_string (res, "name", &name)) {
			for (n = method->arg_list; n; n = x_list_next (n)) {
				free (((xmmsc_service_argument_t *)n->data)->name);
				free ((xmmsc_service_argument_t *)n->data);
			}
			free (method->arg_list);
			return 0;
		}
		if (!xmmsc_result_get_dict_entry_uint (res, "type", &arg->type)) {
			for (n = method->arg_list; n; n = x_list_next (n)) {
				free (((xmmsc_service_argument_t *)n->data)->name);
				free ((xmmsc_service_argument_t *)n->data);
			}
			free (method->arg_list);
			return 0;
		}
		if (!xmmsc_result_get_dict_entry_int (res, "optional", &arg->optional)) {
			for (n = method->arg_list; n; n = x_list_next (n)) {
				free (((xmmsc_service_argument_t *)n->data)->name);
				free ((xmmsc_service_argument_t *)n->data);
			}
			free (method->arg_list);
			return 0;
		}
		arg->name = strdup (name);

		method->arg_list = x_list_append (method->arg_list, arg);

		xmmsc_result_list_next (res);
	}

	return 1;
}

/**
 * Retrieve service method request cookie.
 * 
 * @param res The #xmmsc_result_t containing the cookie.
 * @param cookie The return cookie.
 * @return 1 for success, 0 otherwise.
 */
int
xmmsc_result_get_service_cookie (xmmsc_result_t *res, uint32_t *cookie)
{
	if (!res || res->error != XMMS_ERROR_NONE) {
		return 0;
	}

	return xmmsc_result_get_dict_entry_uint (res, "sc_id", cookie);
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

/* Kept as a proxy for external use */
void
xmmsc_result_seterror (xmmsc_result_t *res, const char *errstr)
{
	if (res->data) {
		xmmsv_unref (res->data);
	}
	res->data = xmmsv_new_error (errstr);
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
	x_list_t *n, *next;
	int cmd;
	xmmsc_result_t *restart_res;
	xmmsc_result_callback_t *cb;

	x_return_if_fail (res);
	x_return_if_fail (msg);

	if (!xmmsc_result_parse_msg (res, msg)) {
		xmms_ipc_msg_destroy (msg);
		return;
	}

	cmd = xmms_ipc_msg_get_cmd (msg);

	xmms_ipc_msg_destroy (msg);

	xmmsc_result_ref (res);

	/* Run all notifiers and check for positive return values */
	n = res->notifiers;
	while (n) {
		next = x_list_next (n);
		cb = n->data;

		if (!cb->func (res->data, cb->user_data)) {
			xmmsc_result_notifier_delete (res, n);
		}

		n = next;
	}

	/* If we restart a signal, we must cleanup its callback because
	 * they hold a reference to the result. */
	if (res->notifiers && cmd == XMMS_IPC_CMD_SIGNAL) {
		restart_res = xmmsc_result_restart (res);

		/* notifiers, and their references have been added to restart_res
		 * Before the notifiers would unref the result themselves,
		 * but they cannot do that anymore, so we must */
		n = res->notifiers;
		while (n) {
			next = x_list_next (n);
			xmmsc_result_notifier_remove (res, n);
			n = next;
		}

		xmmsc_result_unref (restart_res);
	}

	if (cmd == XMMS_IPC_CMD_BROADCAST) {
		/* We keep the results alive with broadcasts, but we
		   just renew the value because it went out of scope.
		   (freeing the payload, forget about it) */
		xmmsv_unref (res->data);
		res->data = NULL;
	}

	xmmsc_result_unref (res);
}

/**
 * Allocates new #xmmsc_result_t and references it.
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

	res->data = NULL;
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

/* Dereference a notifier from a result.
 * The #x_list_t node containing the notifier is passed.
 */
static void
xmmsc_result_notifier_remove (xmmsc_result_t *res, x_list_t *node)
{
	free (node->data); /* remove the callback struct, but not the udata */
	res->notifiers = x_list_delete_link (res->notifiers, node);
	xmmsc_result_unref (res); /* each cb has a reference to res */
}

/* Dereference a notifier from a result and delete its udata.
 * The #x_list_t node containing the notifier is passed.
 */
static void
xmmsc_result_notifier_delete (xmmsc_result_t *res, x_list_t *node)
{
	xmmsc_result_callback_t *cb = node->data;

	/* remove the udata */
	if (cb->free_func) {
		cb->free_func (cb->user_data);
	}
	xmmsc_result_notifier_remove (res, node);
}
