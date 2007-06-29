#ifndef __XMMS_CLIENT_H__
#define __XMMS_CLIENT_H__

#include "xmmsc/xmmsc_stdint.h"
#include "xmmsc/xmmsc_ipc_msg.h"
#include "xmmsc/xmmsc_ipc_transport.h"
#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_coll.h"

typedef void (*xmmsc_request_callback_t) (void * /* data, and what else? */);

typedef struct xmmsc_connection_t_ xmmsc_connection_t;

typedef enum {
	XMMSC_REQUEST_INTERVAL_NOW,
	XMMSC_REQUEST_INTERVAL_EVERY_CHANGE,
	XMMSC_REQUEST_END
} xmmsc_request_interval_t;

typedef struct xmmsc_request_t_ {
	xmmsc_connection_t *c;
	xmms_ipc_msg_t *msg;

	/* Other properties */

	xmmsc_request_callback_t cb;
	xmmsc_request_interval_t interval;

	unsigned int type;

} xmmsc_request_t;

typedef unsigned int xmmsc_error_t;

xmmsc_connection_t *xmmsc_init (const char *clientname);
int xmmsc_connect (xmmsc_connection_t *c, const char *ipcpath);
unsigned int xmmsc_next_id (xmmsc_connection_t *c);
xmms_ipc_transport_t *xmmsc_connection_get_transport (xmmsc_connection_t *c);

int xmmsc_ipc_msg_write (xmmsc_connection_t *c, xmms_ipc_msg_t *msg);

xmmsc_request_t *xmmsc_request_new (xmmsc_connection_t *c, xmms_ipc_msg_t *msg);
void xmmsc_request_set_callback (xmmsc_request_t *req, xmmsc_request_callback_t cb);
int xmmsc_request_send (xmmsc_request_t *req);
void xmmsc_request_now (xmmsc_request_t *req);

xmmsc_request_t *xmmsc_object_property_get (xmmsc_connection_t *c, int object,
                 int property);

#endif
