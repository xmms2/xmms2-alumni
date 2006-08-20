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
#include "xmmsc/xmmsc_idnumbers.h"


/**
 * @defgroup Collection Collection
 * @ingroup XMMSClient
 * @brief This performs everything related to collections.
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
                xmmsc_coll_namespace_t ns)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_COLLECTION, XMMS_IPC_CMD_COLLECTION_GET);
	xmms_ipc_msg_put_string (msg, collname);
	xmms_ipc_msg_put_string (msg, ns);

	res = xmmsc_send_msg (conn, msg);

	return res;
}

/**
 * List all collections saved on the server in the given namespace.
 *
 * @param conn  The connection to the server.
 * @param ns  The namespace containing the saved collections.
 */
xmmsc_result_t*
xmmsc_coll_list (xmmsc_connection_t *conn, xmmsc_coll_namespace_t ns)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_COLLECTION, XMMS_IPC_CMD_COLLECTION_LIST);
	xmms_ipc_msg_put_string (msg, ns);

	res = xmmsc_send_msg (conn, msg);

	return res;
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
xmmsc_coll_save (xmmsc_connection_t *conn, xmmsc_coll_t *coll,
                 const char* name, xmmsc_coll_namespace_t ns)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_COLLECTION, XMMS_IPC_CMD_COLLECTION_SAVE);
	xmms_ipc_msg_put_string (msg, name);
	xmms_ipc_msg_put_string (msg, ns);
	xmms_ipc_msg_put_collection (msg, coll);

	res = xmmsc_send_msg (conn, msg);

	return res;
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
                   const char* name, xmmsc_coll_namespace_t ns)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_COLLECTION, XMMS_IPC_CMD_COLLECTION_REMOVE);
	xmms_ipc_msg_put_string (msg, name);
	xmms_ipc_msg_put_string (msg, ns);

	res = xmmsc_send_msg (conn, msg);

	return res;
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
xmmsc_coll_find (xmmsc_connection_t *conn, unsigned int mediaid, xmmsc_coll_namespace_t ns)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_COLLECTION, XMMS_IPC_CMD_COLLECTION_FIND);
	xmms_ipc_msg_put_uint32 (msg, mediaid);
	xmms_ipc_msg_put_string (msg, ns);

	res = xmmsc_send_msg (conn, msg);

	return res;
}

/**
 * Rename a saved collection.
 *
 * @param conn  The connection to the server.
 * @param from_name  The name of the collection to rename.
 * @param to_name  The new name of the collection.
 * @param ns  The namespace containing the collection.
 */
xmmsc_result_t* xmmsc_coll_rename (xmmsc_connection_t *conn, char* from_name,
                                   char* to_name, xmmsc_coll_namespace_t ns)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_COLLECTION, XMMS_IPC_CMD_COLLECTION_RENAME);
	xmms_ipc_msg_put_string (msg, from_name);
	xmms_ipc_msg_put_string (msg, to_name);
	xmms_ipc_msg_put_string (msg, ns);

	res = xmmsc_send_msg (conn, msg);

	return res;
}

 
/**
 * List the ids of all media matched by the given collection.
 * A list of ordering properties can be specified, as well as offsets
 * to only retrieve part of the result set.
 *
 * @param conn  The connection to the server.
 * @param coll  The collection used to query.
 * @param order  The list of properties to order by (NULL to disable).
 * @param limit_start  The offset at which to start retrieving results (0 to disable).
 * @param limit_len  The maximum number of entries to retrieve (0 to disable).
 */
xmmsc_result_t*
xmmsc_coll_query_ids (xmmsc_connection_t *conn, xmmsc_coll_t *coll, 
                      const char **order, unsigned int limit_start,
                      unsigned int limit_len)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_COLLECTION, XMMS_IPC_CMD_QUERY_IDS);
	xmms_ipc_msg_put_collection (msg, coll);
	xmms_ipc_msg_put_uint32 (msg, limit_start);
	xmms_ipc_msg_put_uint32 (msg, limit_len);
	xmms_ipc_msg_put_string_list (msg, order);

	res = xmmsc_send_msg (conn, msg);

	return res;
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
 * @param order  The list of properties to order by (NULL to disable).
 * @param limit_start  The offset at which to start retrieving results (0 to disable).
 * @param limit_len  The maximum number of entries to retrieve (0 to disable).
 * @param fetch  The list of properties to retrieve (NULL to only get the id).
 * @param group  The list of properties to group by (NULL to disable).
 */
xmmsc_result_t*
xmmsc_coll_query_infos (xmmsc_connection_t *conn, xmmsc_coll_t *coll,
                        const char **order, unsigned int limit_start,
                        unsigned int limit_len, const char **fetch,
                        const char **group)
{
	xmmsc_result_t *res;
	xmms_ipc_msg_t *msg;

	x_check_conn (conn, NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_COLLECTION, XMMS_IPC_CMD_QUERY_INFOS);
	xmms_ipc_msg_put_collection (msg, coll);
	xmms_ipc_msg_put_uint32 (msg, limit_start);
	xmms_ipc_msg_put_uint32 (msg, limit_len);
	xmms_ipc_msg_put_string_list (msg, order);
	xmms_ipc_msg_put_string_list (msg, fetch);
	xmms_ipc_msg_put_string_list (msg, group);

	res = xmmsc_send_msg (conn, msg);

	return res;
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

/** @} */
