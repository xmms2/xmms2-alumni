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
 * Find out whether a collection is a medialist
 *
 * @param conn  The connection to the server.
 * @param coll  The collection to investigate.
 */
xmmsc_result_t* xmmsc_coll_is_medialist (xmmsc_connection_t *conn,
                                         xmmsv_coll_t *coll)
{
	x_check_conn (conn, NULL);
	x_api_error_if (!coll, "with a NULL collection", NULL);

	return xmmsc_send_cmd (conn, XMMS_IPC_OBJECT_COLLECTION,
	                       XMMS_IPC_CMD_COLLECTION_IS_MEDIALIST,
	                       XMMSV_LIST_ENTRY_COLL (coll),
	                       XMMSV_LIST_END);
}

/**
 * List metadata of all media in a medialist (in the appropiate order).
 *
 * @param conn  The connection to the server.
 * @param coll  The collection used to query.
 * @param fetch The metadata to query
 */
xmmsc_result_t*
xmmsc_coll_query_medialist (xmmsc_connection_t *conn, xmmsv_coll_t *coll,
                            const char *fetch)
{
	x_check_conn (conn, NULL);
	x_api_error_if (!coll, "with a NULL collection", NULL);
	x_api_error_if (!fetch, "with a NULL fetch", NULL);

	return xmmsc_send_cmd (conn, XMMS_IPC_OBJECT_COLLECTION,
	                       XMMS_IPC_CMD_COLLECTION_IS_MEDIALIST,
	                       XMMSV_LIST_ENTRY_COLL (coll),
						   XMMSV_LIST_ENTRY_STR (fetch),
	                       XMMSV_LIST_END);
}

/**
 * List metadata of all media in a collection, by cluster.
 *
 * @param conn  The connection to the server.
 * @param coll  The collection used to query.
 * @param spec  The metadata to query and how to cluster.
 */
xmmsc_result_t*
xmmsc_coll_query_clustered (xmmsc_connection_t *conn, xmmsv_coll_t *coll,
                            const char *spec)
{
	x_check_conn (conn, NULL);
	x_api_error_if (!coll, "with a NULL collection", NULL);
	x_api_error_if (!spec, "with a NULL spec", NULL);

	return xmmsc_send_cmd (conn, XMMS_IPC_OBJECT_COLLECTION,
	                       XMMS_IPC_CMD_COLLECTION_IS_MEDIALIST,
	                       XMMSV_LIST_ENTRY_COLL (coll),
						   XMMSV_LIST_ENTRY_STR (spec),
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
	x_check_conn (conn, NULL);
	x_api_error_if (!coll, "with a NULL collection", NULL);

	/* default to empty ordering */
	if (!order) {
		order = xmmsv_new_list ();
	} else {
		xmmsv_ref (order);
	}

	return xmmsc_send_cmd (conn, XMMS_IPC_OBJECT_COLLECTION,
	                       XMMS_IPC_CMD_QUERY_IDS,
	                       XMMSV_LIST_ENTRY_COLL (coll),
	                       XMMSV_LIST_ENTRY_INT (limit_start),
	                       XMMSV_LIST_ENTRY_INT (limit_len),
	                       XMMSV_LIST_ENTRY (order),
	                       XMMSV_LIST_END);
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
	x_check_conn (conn, NULL);
	x_api_error_if (!coll, "with a NULL collection", NULL);
	x_api_error_if (!fetch, "with a NULL fetch list", NULL);

	/* default to empty ordering */
	if (!order) {
		order = xmmsv_new_list ();
	} else {
		xmmsv_ref (order);
	}

	/* default to empty grouping */
	if (!group) {
		group = xmmsv_new_list ();
	} else {
		xmmsv_ref (group);
	}

	return xmmsc_send_cmd (conn, XMMS_IPC_OBJECT_COLLECTION,
	                       XMMS_IPC_CMD_QUERY_INFOS,
	                       XMMSV_LIST_ENTRY_COLL (coll),
	                       XMMSV_LIST_ENTRY_INT (limit_start),
	                       XMMSV_LIST_ENTRY_INT (limit_len),
	                       XMMSV_LIST_ENTRY (order),
	                       XMMSV_LIST_ENTRY (xmmsv_ref (fetch)),
	                       XMMSV_LIST_ENTRY (group),
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

/**
 * Return a collection with an order-operator added.
 *
 * @param coll	the original collection
 * @param key	an ordering string, optionally starting with "-" (for descending
 * ordering), followed by a string "id", "random" or a key identifying a
 * property, such as "artist" or "album"
 *
 * @return	coll with order-operators added
 */
xmmsv_coll_t *
xmmsv_coll_add_order_operator (xmmsv_coll_t *coll, const char *key) {
	xmmsv_coll_t *new;

	new = xmmsv_coll_new (XMMS_COLLECTION_TYPE_ORDER);
	xmmsv_coll_add_operand (new, coll);

	if (key[0] == '-') {
		xmmsv_coll_attribute_set (new, "order", "DESC");
		key++;
	} else {
		xmmsv_coll_attribute_set (new, "order", "ASC");
	}

	if (strcmp (key, "id") == 0) {
		xmmsv_coll_attribute_set (new, "type", "id");
	} else if (strcmp (key, "random") == 0) {
		xmmsv_coll_attribute_set (new, "type", "random");
	} else {
		xmmsv_coll_attribute_set (new, "type", "value");
		xmmsv_coll_attribute_set (new, "field", key);
	}

	return new;
}

/**
 * Return a collection with several order-operators added.
 *
 * @param coll	the original collection
 * @param order	list of ordering strings
 *
 * @return	coll with order-operators added
 */
xmmsv_coll_t *
xmmsv_coll_add_order_operators (xmmsv_coll_t *coll, xmmsv_t *order)
{
	xmmsv_coll_t *new, *current;
	xmmsv_list_iter_t *it;
	xmmsv_t *val;
	const char *str;

	/* FIXME: check if order is a list */

	xmmsv_coll_ref (coll);

	if (!order) {
		return coll;
	}

	current = coll;
	xmmsv_get_list_iter (order, &it);
	xmmsv_list_iter_last (it);

	while (xmmsv_list_iter_valid (it)) {
		xmmsv_list_iter_entry (it, &val);
		if (!xmmsv_get_string (val, &str)) {
			/* FIXME: how do we throw clientlib errors? */
		}

		new = xmmsv_coll_add_order_operator (current, str);
		xmmsv_coll_unref (current);

		current = new;
		xmmsv_list_iter_prev (it);
	}

	return current;
}


/** @} */
