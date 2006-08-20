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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "xmmsclient/xmmsclient.h"
#include "xmmsclientpriv/xmmsclient.h"
#include "xmmsclientpriv/xmmsclient_ipc.h"
#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_stringport.h"

/**
 * @defgroup MedialibControl MedialibControl
 * @ingroup XMMSClient
 * @brief This controls the medialib.
 *
 * @{
 */

static xmmsc_result_t *
do_methodcall (xmmsc_connection_t *conn, unsigned int id, const char *arg)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_MEDIALIB, id);
	xmms_ipc_msg_put_string (msg, arg);

	res = xmmsc_send_msg (conn, msg);

	return res;
}

/**
 * This function will make a pretty string about the information in
 * the mediainfo hash supplied to it.
 * @param target A allocated char *
 * @param len Length of target
 * @param fmt A format string to use. You can insert items from the hash by
 * using specialformat "${field}".
 * @param table The x_hash_t that you got from xmmsc_result_get_mediainfo
 * @returns The number of chars written to #target
 */

int
xmmsc_entry_format (char *target, int len, const char *fmt, xmmsc_result_t *res)
{
	const char *pos;

	if (!target) {
		return 0;
	}

	if (!fmt) {
		return 0;
	}

	memset (target, 0, len);

	pos = fmt;
	while (strlen (target) + 1 < len) {
		char *next_key, *key, *result = NULL, *end;
		int keylen;

		next_key = strstr (pos, "${");
		if (!next_key) {
			strncat (target, pos, len - strlen (target) - 1);
			break;
		}

		strncat (target, pos, MIN (next_key - pos, len - strlen (target) - 1));
		keylen = strcspn (next_key + 2, "}");
		key = malloc (keylen + 1);

		if (!key) {
			fprintf (stderr, "Unable to allocate %u bytes of memory, OOM?", keylen);
			break;
		}

		memset (key, 0, keylen + 1);
		strncpy (key, next_key + 2, keylen);

		if (strcmp (key, "seconds") == 0) {
			int duration;

			xmmsc_result_get_dict_entry_int (res, "duration", &duration);

			if (!duration) {
				strncat (target, "00", len - strlen (target) - 1);
			} else {
				char seconds[10];
				/* rounding */
				duration += 500;
				snprintf (seconds, sizeof(seconds), "%02d", (duration/1000)%60);
				strncat (target, seconds, len - strlen (target) - 1);
			}
		} else if (strcmp (key, "minutes") == 0) {
			int duration;

			xmmsc_result_get_dict_entry_int (res, "duration", &duration);

			if (!duration) {
				strncat (target, "00", len - strlen (target) - 1);
			} else {
				char minutes[10];
				/* rounding */
				duration += 500;
				snprintf (minutes, sizeof(minutes), "%02d", duration/60000);
				strncat (target, minutes, len - strlen (target) - 1);
			}
		} else {
			char tmp[12];

			xmmsc_result_value_type_t type = xmmsc_result_get_dict_entry_type (res, key);
			if (type == XMMSC_RESULT_VALUE_TYPE_STRING) {
				xmmsc_result_get_dict_entry_string (res, key, &result);
			} else if (type == XMMSC_RESULT_VALUE_TYPE_UINT32) {
				uint32_t ui;
				xmmsc_result_get_dict_entry_uint (res, key, &ui);
				snprintf (tmp, 12, "%u", ui);
				result = tmp;
			} else if (type == XMMSC_RESULT_VALUE_TYPE_INT32) {
				int32_t i;
				xmmsc_result_get_dict_entry_int (res, key, &i);
				snprintf (tmp, 12, "%d", i);
				result = tmp;
			}
				
			if (result)
				strncat (target, result, len - strlen (target) - 1);
		}

		free (key);
		end = strchr (next_key, '}');

		if (!end) {
			break;
		}

		pos = end + 1;
	}

	return strlen (target);
}

/**
 * Search for a entry (URL) in the medialib db and return its ID number
 * @param conn The #xmmsc_connection_t
 * @param url The URL to search for
 */
xmmsc_result_t *
xmmsc_medialib_get_id (xmmsc_connection_t *conn, const char *url)
{
	x_check_conn (conn, NULL);

	return do_methodcall (conn, XMMS_IPC_CMD_GET_ID, url);
}

/**
 * Remove a entry from the medialib
 * @param conn The #xmmsc_connection_t
 * @param entry The entry id you want to remove
 */
xmmsc_result_t *
xmmsc_medialib_remove_entry (xmmsc_connection_t *conn, uint32_t entry)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_MEDIALIB, XMMS_IPC_CMD_REMOVE_ID);
	xmms_ipc_msg_put_uint32 (msg, entry);

	res = xmmsc_send_msg (conn, msg);

	return res;
}

/**
 * Add a URL to the medialib. If you want to add mutiple files
 * you should call #xmmsc_medialib_path_import
 * @param conn The #xmmsc_connection_t
 * @param url URL to add to the medialib.
 */
xmmsc_result_t *
xmmsc_medialib_add_entry (xmmsc_connection_t *conn, const char *url)
{
	return xmmsc_medialib_add_entry_args (conn, url, 0, NULL);
}

/**
 * Add a URL with arguments to the medialib.
 *
 * xmmsc_medialib-add_antry_args (conn, "file:///data/HVSC/C64Music/Hubbard_Rob/Commando.sid", 1, "subtune=2");
 * 
 * @param conn The #xmmsc_connection_t
 * @param url URL to add to the medialib.
 * @param numargs The number of arguments
 * @param args array of numargs strings used as arguments
 */
xmmsc_result_t *
xmmsc_medialib_add_entry_args (xmmsc_connection_t *conn, const char *url, int numargs, const char **args)
{
	char *enc_url;
	xmmsc_result_t *res;

	x_check_conn (conn, NULL);

	enc_url = xmmsc_medialib_encode_url (url, numargs, args);
	if (!enc_url)
		return NULL;

	res = do_methodcall (conn, XMMS_IPC_CMD_ADD_URL, enc_url);

	free (enc_url);

	return res;
}

/**
 * Import a all files recursivly from the directory passed
 * as argument.
 * @param conn #xmmsc_connection_t
 * @param path A directory to recursive search for mediafiles
 */
xmmsc_result_t *
xmmsc_medialib_path_import (xmmsc_connection_t *conn,
			    const char *path)
{
	xmmsc_result_t *res;
	char *enc_path;

	x_check_conn (conn, NULL);

	enc_path = xmmsc_medialib_encode_url (path, 0, NULL);
	if (!enc_path)
		return NULL;

	res = do_methodcall (conn, XMMS_IPC_CMD_PATH_IMPORT, enc_path);

	free (enc_path);

	return res;
}

/**
 * Rehash the medialib, this will check data in the medialib
 * still is the same as the data in files.
 * @param id The id to rehash. Set it to 0 if you want to rehash
 * the whole medialib.
 */
xmmsc_result_t *
xmmsc_medialib_rehash (xmmsc_connection_t *conn,
                       unsigned int id)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_MEDIALIB, XMMS_IPC_CMD_REHASH);
	xmms_ipc_msg_put_uint32 (msg, id);

	res = xmmsc_send_msg (conn, msg);

	return res;

}

/**
 * Retrieve information about a entry from the medialib.
 */
xmmsc_result_t *
xmmsc_medialib_get_info (xmmsc_connection_t *c, unsigned int id)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (c, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_MEDIALIB, XMMS_IPC_CMD_INFO);
	xmms_ipc_msg_put_uint32 (msg, id);

	res = xmmsc_send_msg (c, msg);

	return res;
}

/** 
 * Request the medialib_entry_added broadcast. This will be called
 * if a new entry is added to the medialib serverside.
 */
xmmsc_result_t * 
xmmsc_broadcast_medialib_entry_added (xmmsc_connection_t *c)
{
	x_check_conn (c, NULL);

	return xmmsc_send_broadcast_msg (c, XMMS_IPC_SIGNAL_MEDIALIB_ENTRY_ADDED);
}

/**
 * Request the medialib_entry_changed broadcast. This will be called
 * if a entry changes on the serverside. The argument will be an medialib
 * id.
 */
xmmsc_result_t *
xmmsc_broadcast_medialib_entry_changed (xmmsc_connection_t *c)
{
	x_check_conn (c, NULL);

	return xmmsc_send_broadcast_msg (c, XMMS_IPC_SIGNAL_MEDIALIB_ENTRY_UPDATE);
}

/**
 * Associate a int value with a medialib entry. Uses default
 * source which is client/<clientname>
 */
xmmsc_result_t *
xmmsc_medialib_entry_property_set_int (xmmsc_connection_t *c, uint32_t id,
                                       const char *key, int32_t value)
{
	xmmsc_result_t *res;
	char tmp[256];

	x_check_conn (c, NULL);

	snprintf (tmp, 256, "client/%s", c->clientname);
	res = xmmsc_medialib_entry_property_set_int_with_source (c, id,
	                                                         tmp, key,
	                                                         value);
	return res;
}

/**
 * Set a custom int field in the medialib associated with a entry,
 * the same as #xmmsc_result_entry_property_set but with specifing
 * your own source.
 */
xmmsc_result_t *
xmmsc_medialib_entry_property_set_int_with_source (xmmsc_connection_t *c, 
                                                   uint32_t id,
                                                   const char *source, 
                                                   const char *key, 
                                                   int32_t value)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (c, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_MEDIALIB,
	                        XMMS_IPC_CMD_PROPERTY_SET_INT);
	xmms_ipc_msg_put_uint32 (msg, id);
	xmms_ipc_msg_put_string (msg, source);
	xmms_ipc_msg_put_string (msg, key);
	xmms_ipc_msg_put_int32 (msg, value);

	res = xmmsc_send_msg (c, msg);

	return res;
}

/**
 * Associate a value with a medialib entry. Uses default
 * source which is client/<clientname>
 */
xmmsc_result_t *
xmmsc_medialib_entry_property_set_str (xmmsc_connection_t *c, uint32_t id,
                                       const char *key, const char *value)
{
	xmmsc_result_t *res;
	char tmp[256];

	x_check_conn (c, NULL);

	snprintf (tmp, 256, "client/%s", c->clientname);
	res = xmmsc_medialib_entry_property_set_str_with_source (c, id,
	                                                         tmp, key,
	                                                         value);
	return res;
}

/**
 * Set a custom field in the medialib associated with a entry,
 * the same as #xmmsc_result_entry_property_set but with specifing
 * your own source.
 */
xmmsc_result_t *
xmmsc_medialib_entry_property_set_str_with_source (xmmsc_connection_t *c, 
                                                   uint32_t id,
                                                   const char *source, 
                                                   const char *key, 
                                                   const char *value)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (c, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_MEDIALIB,
	                        XMMS_IPC_CMD_PROPERTY_SET_STR);
	xmms_ipc_msg_put_uint32 (msg, id);
	xmms_ipc_msg_put_string (msg, source);
	xmms_ipc_msg_put_string (msg, key);
	xmms_ipc_msg_put_string (msg, value);

	res = xmmsc_send_msg (c, msg);

	return res;
}

/** 
 * Remove a custom field in the medialib associated with an entry.
 * Uses default source which is client/<clientname>
 */
xmmsc_result_t *
xmmsc_medialib_entry_property_remove (xmmsc_connection_t *c, uint32_t id,
                                      const char *key)
{
	xmmsc_result_t *res;
	char tmp[256];

	x_check_conn (c, NULL);

	snprintf(tmp, 256, "client/%s", c->clientname);
	res = xmmsc_medialib_entry_property_remove_with_source (c, id, 
	                                                        tmp, key);
	return res;
}

/**
 * Remove a custom field in the medialib associated with an entry.
 * Identical to #xmmsc_result_entry_property_remove except with specifying
 * your own source.
 */
xmmsc_result_t *
xmmsc_medialib_entry_property_remove_with_source (xmmsc_connection_t *c,
                                                  uint32_t id, 
                                                  const char *source,
                                                  const char *key)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (c, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_MEDIALIB,
	                        XMMS_IPC_CMD_PROPERTY_REMOVE);
	xmms_ipc_msg_put_uint32 (msg, id);
	xmms_ipc_msg_put_string (msg, source);
	xmms_ipc_msg_put_string (msg, key);

	res = xmmsc_send_msg (c, msg);

	return res;
}

/** @} */

#define GOODCHAR(a) ((((a) >= 'a') && ((a) <= 'z')) || \
                     (((a) >= 'A') && ((a) <= 'Z')) || \
                     (((a) >= '0') && ((a) <= '9')) || \
                     ((a) == ':') || \
                     ((a) == '/') || \
                     ((a) == '-') || \
                     ((a) == '.') || \
                     ((a) == '_'))


char *
xmmsc_medialib_encode_url (const char *url, int narg, const char **args)
{
	static char hex[16] = "0123456789abcdef";
	int i = 0, j = 0, extra = 0;
	char *res;

	x_api_error_if (!url, "with a NULL url", NULL);

	for (i = 0; i < narg; i++) {
		extra += strlen (args[i]) + 2;
	}

	res = malloc (strlen(url) * 3 + 1 + extra);
	if (!res)
		return NULL;

	for (i = 0; url[i]; i++) {
		unsigned char chr = url[i];
		if (GOODCHAR (chr)) {
			res[j++] = chr;
		} else if (chr == ' ') {
			res[j++] = '+';
		} else {
			res[j++] = '%';
			res[j++] = hex[((chr & 0xf0) >> 4)];
			res[j++] = hex[(chr & 0x0f)];
		}
	}

	for (i = 0; i < narg; i++) {
		int l;
		l = strlen (args[i]);
		res[j] = (i == 0) ? '?' : '&';
		j++;
		memcpy (&res[j], args[i], l);
		j += l;
	}

	res[j] = '\0';

	return res;
}
