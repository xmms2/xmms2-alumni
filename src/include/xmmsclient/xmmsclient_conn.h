#ifndef __XMMS_CLIENT_H__
#define __XMMS_CLIENT_H__

#include "xmmsc/xmmsc_stdint.h"
#include "xmmsc/xmmsc_ipc_msg.h"
#include "xmmsc/xmmsc_ipc_transport.h"
#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_coll.h"

typedef void (*xmmsc_request_callback_t) (/* data, and what else? */);

struct xmmsc_connection_t_ {
	char *clientname;
	char *error;
	xmms_ipc_transport_t *trans;
	unsigned int id = 0;
};

struct xmmsc_request_t_ {
	xmms_ipc_msg_t *msg;

	/* Other properties */

	//Callback function
	//How often to request
};

typedef struct xmmsc_connection_t_ xmmsc_connection_t;
typedef struct xmmsc_request_t_ xmmsc_request_t;
typedef unsigned int xmmsc_error_t;

xmmsc_connection_t *xmmsc_init (const char *clientname);
int xmmsc_connect (xmmsc_connection_t *c, const char *ipcpath);
unsigned int xmmsc_next_id (xmmsc_connection_t *c);
int xmmsc_ipc_msg_write(xmmsc_connection_t *c, xmms_ipc_msg_t *msg);

xmmsc_request_t *xmmsc_request_new (xmms_ipc_msg_t *msg);
void xmmsc_request_set_callback(xmmsc_request_t *req, xmmsc_request_callback_t *cb);
int xmmsc_request_send (xmmsc_request_t *req);

#endif
