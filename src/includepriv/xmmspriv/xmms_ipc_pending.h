#ifndef __XMMS_PRIV_IPC_PENDING_H__
#define __XMMS_PRIV_IPC_PENDING_H__

#include "xmms/xmms_ipc.h"
#include "xmms/xmms_ipc_pending.h"
#include "xmmsc/xmmsc_ipc_msg.h"

void xmms_ipc_pending_pool_init ();
void xmms_ipc_pending_pool_destroy ();
xmms_ipc_pending_id_t xmms_ipc_pending_register (xmmsv_t *data);
void xmms_ipc_pending_save_ipc (xmms_ipc_pending_id_t pid, uint32_t object_id, xmms_ipc_client_t *client, uint32_t cookie);
void xmms_ipc_pending_send (xmms_ipc_pending_id_t pid, xmmsv_t *retval);
xmmsv_t *xmms_ipc_pending_get_data (xmms_ipc_pending_id_t pid);

#endif
