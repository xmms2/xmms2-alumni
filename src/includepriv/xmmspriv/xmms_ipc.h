#ifndef __XMMS_PRIV_IPC_H__
#define __XMMS_PRIV_IPC_H__

#include "xmms/xmms_ipc.h"
#include "xmmsc/xmmsc_ipc_msg.h"

typedef enum {
	XMMS_IPC_CLIENT_STATUS_NEW,
} xmms_ipc_client_status_t;

typedef struct xmms_ipc_St xmms_ipc_t;

xmms_ipc_t *xmms_ipc_init (void);
void xmms_ipc_shutdown (void);
void on_config_ipcsocket_change (xmms_object_t *object, gconstpointer data, gpointer udata);
gboolean xmms_ipc_setup_server (const gchar *path);
gboolean xmms_ipc_setup_with_gmain (xmms_ipc_t *ipc);

gboolean xmms_ipc_has_pending (guint signalid);

typedef struct xmms_ipc_client_St xmms_ipc_client_t;

gboolean xmms_ipc_client_msg_write (xmms_ipc_client_t *client, xmms_ipc_msg_t *msg);

GList *xmms_ipc_get_cmds();
void xmms_ipc_add_cmds(gpointer *cmds, guint type);

#endif
