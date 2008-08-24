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

#include <glib.h>
#include <string.h>

#include "xmms/xmms_log.h"
#include "xmmspriv/xmms_ipc.h"
#include "xmmspriv/xmms_ipc_pending.h"
#include "xmmsc/xmmsc_ipc_msg.h"


/**
  * @defgroup IPCPending IPCPending
  * @ingroup XMMSServer
  * @brief IPC pending pool functions for XMMS2 Daemon
  * @{
  */

struct xmms_ipc_pending_pool_St {
	GList *entries;
	guint next_free_id;
};

struct xmms_ipc_pending_entry_St {
	xmms_ipc_pending_id_t pid;
	uint32_t object;
	xmms_ipc_client_t *client;
	uint32_t cookie;
	xmmsv_t *data;
};

typedef struct xmms_ipc_pending_pool_St xmms_ipc_pending_pool_t;
typedef struct xmms_ipc_pending_entry_St xmms_ipc_pending_entry_t;


static GMutex *ipc_pending_pool_lock;
static xmms_ipc_pending_pool_t *ipc_pending_pool = NULL;

static xmms_ipc_pending_pool_t *xmms_ipc_pending_pool_new ();
static void xmms_ipc_pending_pool_free (xmms_ipc_pending_pool_t *pool);
static void xmms_ipc_pending_entry_free (xmms_ipc_pending_entry_t *entry);
static xmms_ipc_pending_entry_t *xmms_ipc_pending_find (xmms_ipc_pending_id_t pid);


void
xmms_ipc_pending_pool_init ()
{
	ipc_pending_pool = xmms_ipc_pending_pool_new ();
	ipc_pending_pool_lock = g_mutex_new ();
}

void
xmms_ipc_pending_pool_destroy ()
{
	xmms_ipc_pending_pool_free (ipc_pending_pool);
	g_mutex_free (ipc_pending_pool_lock);
}

static xmms_ipc_pending_pool_t *
xmms_ipc_pending_pool_new ()
{
	xmms_ipc_pending_pool_t *pool;

	pool = g_new0 (xmms_ipc_pending_pool_t, 1);
	pool->next_free_id = 1; /* 0 is an invalid pending id*/

	return pool;
}

static void
xmms_ipc_pending_pool_free (xmms_ipc_pending_pool_t *pool)
{
	xmms_ipc_pending_entry_t *entry;

	/* Free all entries */
	while (pool->entries) {
		entry = (xmms_ipc_pending_entry_t *) pool->entries->data;
		xmms_ipc_pending_entry_free (entry);
		pool->entries = g_list_delete_link (pool->entries, pool->entries);
	}

	g_free (pool);
}

static void
xmms_ipc_pending_entry_free (xmms_ipc_pending_entry_t *entry)
{
	xmmsv_unref (entry->data);
	g_free (entry);
}

static xmms_ipc_pending_entry_t *
xmms_ipc_pending_find (xmms_ipc_pending_id_t pid)
{
	GList *it;
	xmms_ipc_pending_entry_t *entry;

	for (it = ipc_pending_pool->entries; it; it = it->next) {
		entry = (xmms_ipc_pending_entry_t *) it->data;
		if (entry->pid == pid) {
			return entry;
		}
	}

	return NULL;
}

static void
xmms_ipc_pending_remove (xmms_ipc_pending_entry_t *entry)
{
	ipc_pending_pool->entries = g_list_remove (ipc_pending_pool->entries,
	                                           entry);
	xmms_ipc_pending_entry_free (entry);
}

xmms_ipc_pending_id_t
xmms_ipc_pending_register (xmmsv_t *data)
{
	xmms_ipc_pending_id_t pid;
	xmms_ipc_pending_entry_t *entry;

	g_mutex_lock (ipc_pending_pool_lock);
	pid = ipc_pending_pool->next_free_id;

	entry = g_new0 (xmms_ipc_pending_entry_t, 1);
	entry->pid = pid;
	entry->data = xmmsv_ref (data);

	ipc_pending_pool->entries = g_list_append (ipc_pending_pool->entries,
	                                           entry);
	ipc_pending_pool->next_free_id++;
	g_mutex_unlock (ipc_pending_pool_lock);

	return pid;
}

void
xmms_ipc_pending_save_ipc (xmms_ipc_pending_id_t pid, uint32_t object_id,
                           xmms_ipc_client_t *client, uint32_t cookie)
{
	xmms_ipc_pending_entry_t *entry;

	g_mutex_lock (ipc_pending_pool_lock);
	entry = xmms_ipc_pending_find (pid);
	if (!entry) {
		/* FIXME: not found! */
		g_mutex_unlock (ipc_pending_pool_lock);
		XMMS_DBG("Failed to find the pending IPC entry!");
		return;
	}

	entry->object = object_id;
	entry->client = client;
	entry->cookie = cookie;
	g_mutex_unlock (ipc_pending_pool_lock);
}


void
xmms_ipc_pending_send (xmms_ipc_pending_id_t pid, xmmsv_t *retval)
{
	xmms_ipc_pending_entry_t *entry;
	xmms_ipc_msg_t *retmsg;

	g_mutex_lock (ipc_pending_pool_lock);
	entry = xmms_ipc_pending_find (pid);
	if (!entry) {
		/* FIXME: not found! */
		g_mutex_unlock (ipc_pending_pool_lock);
		XMMS_DBG("Failed to find the pending IPC entry!");
		return;
	}

	/* Send retval to pending client */
	retmsg = xmms_ipc_msg_new (entry->object, XMMS_IPC_CMD_REPLY);
	xmms_ipc_msg_put_value (retmsg, retval);
	xmms_ipc_msg_set_cookie (retmsg, entry->cookie);
	xmms_ipc_client_msg_write (entry->client, retmsg);

	xmmsv_unref (retval);

	/* Remove entry from the pending pool */
	xmms_ipc_pending_remove (entry);
	g_mutex_unlock (ipc_pending_pool_lock);
}

xmmsv_t *
xmms_ipc_pending_get_data (xmms_ipc_pending_id_t pid)
{
	xmms_ipc_pending_entry_t *entry;

	entry = xmms_ipc_pending_find (pid);
	if (!entry) {
		/* FIXME: not found! */
		XMMS_DBG("Failed to find the pending IPC entry!");
		return NULL;
	}

	return entry->data;
}

/** @} */

