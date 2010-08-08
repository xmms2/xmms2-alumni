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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "xmmsclient/xmmsclient.h"
#include "xmmsclientpriv/xmmsclient.h"
#include "xmmsc/xmmsc_idnumbers.h"


/** @defgroup Collections Collections
 * @ingroup XMMSClient
 * @brief All modules related to collection handling.
 * The API to use collections ; please refer to the wiki for more infos on this.
 */


/**
 * @defgroup CollectionControl CollectionControl
 * @ingroup Collections
 * @brief Functions to manage the collections on the server.
 *
 * @{
 */

/**
 * Get the collection structure of a collection saved on the server.
 *
 * @param conn  The connection to the server.
 * @param collname  The name of the saved collection.
 * @param ns  The namespace containing the saved collection.
 */
xmmsc_result_t*
xmmsc_coll_get (xmmsc_connection_t *conn, const char *collname,
                xmmsv_coll_namespace_t ns)
{
	x_check_conn (conn, NULL);
	x_api_error_if (!collname, "with a NULL name", NULL);

	return xmmsc_send_cmd (conn, XMMS_IPC_OBJECT_COLLECTION,
	                       XMMS_IPC_CMD_COLLECTION_GET,
	                       XMMSV_LIST_ENTRY_STR (collname),
	                       XMMSV_LIST_ENTRY_STR (ns),
	                       XMMSV_LIST_END);
}

/**
 * Synchronize collection data to the database.
 *
 * @param conn  The connection to the server.
 */
xmmsc_result_t*
xmmsc_coll_sync (xmmsc_connection_t *conn)
{
	x_check_conn (conn, NULL);

	return xmmsc_send_cmd (conn, XMMS_IPC_OBJECT_COLLECTION,
	                       XMMS_IPC_CMD_COLLECTION_SYNC,
	                       XMMSV_LIST_END);
}

/**
 * List all collections saved on the server in the given namespace.
 *
 * @param conn  The connection to the server.
 * @param ns  The namespace containing the saved collections.
 */
xmmsc_result_t*
xmmsc_coll_list (xmmsc_connection_t *conn, xmmsv_coll_namespace_t ns)
{
	x_check_conn (conn, NULL);

	return xmmsc_send_cmd (conn, XMMS_IPC_OBJECT_COLLECTION,
	                       XMMS_IPC_CMD_COLLECTION_LIST,
	                       XMMSV_LIST_ENTRY_STR (ns),
	                       XMMSV_LIST_END);
}

/**
 * Save a collection structure on the server under the given name, in
 * the given namespace.
 *
 * @param conn  The connection to the server.
 * @param coll  The collection structure to save.
 * @param name  The name under which to save the collection.
 * @param ns  The namespace in which to save the collection.
 */
xmmsc_result_t*
xmmsc_coll_save (xmmsc_connection_t *conn, xmmsv_coll_t *coll,
                 const char* name, xmmsv_coll_namespace_t ns)
{
	x_check_conn (conn, NULL);
	x_api_error_if (!coll, "with a NULL collection", NULL);
	x_api_error_if (!name, "with a NULL name", NULL);

	return xmmsc_send_cmd (conn, XMMS_IPC_OBJECT_COLLECTION,
	                       XMMS_IPC_CMD_COLLECTION_SAVE,
	                       XMMSV_LIST_ENTRY_STR (name),
	                       XMMSV_LIST_ENTRY_STR (ns),
	                       XMMSV_LIST_ENTRY_COLL (coll),
	                       XMMSV_LIST_END);
}

/**
 * Remove a collection from the server.
 *
 * @param conn  The connection to the server.
 * @param name  The name of the collection to remove.
 * @param ns  The namespace from which to remove the collection.
 */
xmmsc_result_t*
xmmsc_coll_remove (xmmsc_connection_t *conn,
                   const char* name, xmmsv_coll_namespace_t ns)
{
	x_check_conn (conn, NULL);
	x_api_error_if (!name, "with a NULL name", NULL);

	return xmmsc_send_cmd (conn, XMMS_IPC_OBJECT_COLLECTION,
	                       XMMS_IPC_CMD_COLLECTION_REMOVE,
	                       XMMSV_LIST_ENTRY_STR (name),
	                       XMMSV_LIST_ENTRY_STR (ns),
	                       XMMSV_LIST_END);
}


/**
 * Find all collections in the given namespace which match the given
 * media.  The names of these collections is returned as a list.
 *
 * @param conn  The connection to the server.
 * @param mediaid  The id of the media to look for.
 * @param ns  The namespace to consider (cannot be ALL).
 */
xmmsc_result_t*
xmmsc_coll_find (xmmsc_connection_t *conn, int mediaid, xmmsv_coll_namespace_t ns)
{
	x_check_conn (conn, NULL);

	return xmmsc_send_cmd (conn, XMMS_IPC_OBJECT_COLLECTION,
	                       XMMS_IPC_CMD_COLLECTION_FIND,
	                       XMMSV_LIST_ENTRY_INT (mediaid),
	                       XMMSV_LIST_ENTRY_STR (ns),
	                       XMMSV_LIST_END);
}

/**
 * Rename a saved collection.
 *
 * @param conn  The connection to the server.
 * @param from_name  The name of the collection to rename.
 * @param to_name  The new name of the collection.
 * @param ns  The namespace containing the collection.
 */
xmmsc_result_t* xmmsc_coll_rename (xmmsc_connection_t *conn,
                                   const char* from_name,
                                   const char* to_name,
                                   xmmsv_coll_namespace_t ns)
{
	x_check_conn (conn, NULL);
	x_api_error_if (!from_name, "with a NULL from_name", NULL);
	x_api_error_if (!to_name, "with a NULL to_name", NULL);

	return xmmsc_send_cmd (conn, XMMS_IPC_OBJECT_COLLECTION,
	                       XMMS_IPC_CMD_COLLECTION_RENAME,
	                       XMMSV_LIST_ENTRY_STR (from_name),
	                       XMMSV_LIST_ENTRY_STR (to_name),
	                       XMMSV_LIST_ENTRY_STR (ns),
	                       XMMSV_LIST_END);
}


/**
 * List the ids of all media matched by the given collection.
 * A list of ordering properties can be specified, as well as offsets
 * to only retrieve part of the result set.
 *
 * @param conn  The connection to the server.
 * @param coll  The collection used to query.
 * @param order  The list of properties to order by, passed as an #xmmsv_t list of strings.
 * @param limit_start  The offset at which to start retrieving results (0 to disable).
 * @param limit_len  The maximum number of entries to retrieve (0 to disable).
 */
xmmsc_result_t*
xmmsc_coll_query_ids (xmmsc_connection_t *conn, xmmsv_coll_t *coll,
                      xmmsv_t *order, int limit_start,
                      int limit_len)
{
	xmmsc_result_t *ret;
	xmmsv_t *get_list = xmmsv_new_list ();
	xmmsv_t *id_dict = xmmsv_new_dict ();
	xmmsv_t *fetch_spec = xmmsv_new_dict ();
	xmmsv_coll_t *coll2, *coll3;

	x_check_conn (conn, NULL);
	x_api_error_if (!coll, "with a NULL collection", NULL);

	/* Creates the fetchspec to use */
	xmmsv_list_append_string (get_list, "id");

	xmmsv_dict_set_string (id_dict, "_type", "metadata");
	xmmsv_dict_set (id_dict, "get", get_list);

	xmmsv_dict_set_string (fetch_spec, "_type", "cluster-list");
	xmmsv_dict_set_string (fetch_spec, "cluster-by", "id");
	xmmsv_dict_set (fetch_spec, "data", id_dict);

	coll2 = xmmsv_coll_add_order_operators (coll, order);

	if (limit_start != 0 || limit_len != 0) {
		char str[12];
		coll3 = xmmsv_coll_new (XMMS_COLLECTION_TYPE_LIMIT);
		xmmsv_coll_add_operand (coll3, coll2);

		if (limit_start != 0) {
			sprintf (str, "%i", limit_start);
			xmmsv_coll_attribute_set (coll3, "start", str);
		}

		if (limit_len != 0) {
			sprintf (str, "%i", limit_len);
			xmmsv_coll_attribute_set (coll3, "length", str);
		}

		xmmsv_coll_unref (coll2);
		coll2 = coll3;
	}

	ret = xmmsc_coll_query (conn, coll2, fetch_spec);

	xmmsv_unref (get_list);
	xmmsv_unref (id_dict);
	xmmsv_unref (fetch_spec);
	xmmsv_coll_unref (coll2);

	return ret;
}

/**
 * Checks that the list only contains string values.
 */
static int
check_string_list (xmmsv_t *list)
{
	xmmsv_t *valstr;
	xmmsv_list_iter_t *it;

	for (xmmsv_get_list_iter (list, &it);
	     xmmsv_list_iter_valid (it);
	     xmmsv_list_iter_next (it)) {
		xmmsv_list_iter_entry (it, &valstr);
		if (xmmsv_get_type (valstr) != XMMSV_TYPE_STRING) {
			return 0;
		}
	}

	return 1;
}

/**
 * List the properties of all media matched by the given collection.
 * A list of ordering properties can be specified, as well as offsets
 * to only retrieve part of the result set. The list of properties to
 * retrieve must be explicitely specified.  It is also possible to
 * group by certain properties.
 *
 * @param conn  The connection to the server.
 * @param coll  The collection used to query.
 * @param order The list of properties to order by, passed as an
 *              #xmmsv_t list of strings.
 * @param limit_start  The offset at which to start retrieving results (0 to disable).
 * @param limit_len  The maximum number of entries to retrieve (0 to disable).
 * @param fetch  The list of properties to retrieve, passed as an
 *               #xmmsv_t list of strings. At least one property is required.
 * @param group  The list of properties to group by, passed as an
 *               #xmmsv_t list of strings.
 */
xmmsc_result_t*
xmmsc_coll_query_infos (xmmsc_connection_t *conn, xmmsv_coll_t *coll,
                        xmmsv_t *order, int limit_start,
                        int limit_len, xmmsv_t *fetch,
                        xmmsv_t *group)
{
	xmmsc_result_t *ret;
	xmmsv_t *fetch_spec, *org_dict;
	int i;
	const char *str;

	/* check that fetch is not empty */
	if (xmmsv_list_get_size (fetch) == 0) {
		x_api_error_if (!coll, "fetch list must not be empty", NULL);
	}

	/* check for invalid property strings */
	if (!check_string_list (order)) {
		x_api_error_if (!coll, "invalid order list!", NULL);
	}
	if (!check_string_list (fetch)) {
		x_api_error_if (!coll, "invalid fetch list", NULL);
	}
	if (!check_string_list (group)) {
		x_api_error_if (!coll, "invalid group list", NULL);
	}

	fetch_spec = xmmsv_new_dict ();
	xmmsv_dict_set_string (fetch_spec, "_type", "cluster-list");
	if (xmmsv_list_get_size (group) > 0) {
		xmmsv_dict_set (fetch_spec, "cluster-by", group);
	} else {
		xmmsv_dict_set_string (fetch_spec, "cluster-by", "id");
	}

	org_dict = xmmsv_new_dict ();
	xmmsv_dict_set_string (org_dict, "_type", "organize");
	xmmsv_dict_set (fetch_spec, "data", org_dict);

	for (i = 0; xmmsv_list_get_string (fetch, i, &str); i++) {
		xmmsv_t *meta = xmmsv_new_dict ();

		if (strcmp (str, "id") == 0) {
			xmmsv_t *get = xmmsv_new_list ();
			xmmsv_list_append_string (get, "id");
			xmmsv_dict_set (meta, "get", get);
			xmmsv_unref (get);
		} else {
			xmmsv_dict_set_string (meta, "keys", str);
		}

		xmmsv_dict_set (org_dict, str, meta);
		xmmsv_unref (meta);
	}

	coll = xmmsv_coll_add_order_operators (coll, order);

	ret = xmmsc_coll_query (conn, coll, fetch_spec);

	xmmsv_coll_unref (coll);
	xmmsv_unref (fetch_spec);
	xmmsv_unref (org_dict);

	return ret;
}

xmmsc_result_t*
xmmsc_coll_query (xmmsc_connection_t *conn, xmmsv_coll_t *coll, xmmsv_t *fetch)
{
	x_check_conn (conn, NULL);
	x_api_error_if (!coll, "with a NULL collection", NULL);
	x_api_error_if (!fetch, "with a NULL fetch list", NULL);

	return xmmsc_send_cmd (conn, XMMS_IPC_OBJECT_COLLECTION,
	                       XMMS_IPC_CMD_QUERY,
	                       XMMSV_LIST_ENTRY_COLL (coll),
	                       XMMSV_LIST_ENTRY (xmmsv_ref (fetch)),
	                       XMMSV_LIST_END);
}

/**
 * Request the collection changed broadcast from the server. Everytime someone
 * manipulates a collection this will be emitted.
 */
xmmsc_result_t*
xmmsc_broadcast_collection_changed (xmmsc_connection_t *c)
{
	x_check_conn (c, NULL);

	return xmmsc_send_broadcast_msg (c, XMMS_IPC_SIGNAL_COLLECTION_CHANGED);
}

/**
 * Create a new collections structure with type idlist
 * from a playlist file.
 *
 * @param conn  The connection to the server.
 * @param path  Path to the playlist file. Must be unencoded.
 */
xmmsc_result_t *
xmmsc_coll_idlist_from_playlist_file (xmmsc_connection_t *conn, const char *path)
{
	xmmsc_result_t *res;
	char *enc_url;

	x_check_conn (conn, NULL);

	enc_url = _xmmsc_medialib_encode_url (path, NULL);

	res = xmmsc_send_cmd (conn, XMMS_IPC_OBJECT_COLLECTION,
	                      XMMS_IPC_CMD_IDLIST_FROM_PLS,
	                      XMMSV_LIST_ENTRY_STR (enc_url),
	                      XMMSV_LIST_END);

	free (enc_url);

	return res;
}


/** @} */
