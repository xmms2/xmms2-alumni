#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <sys/types.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#include <sys/types.h>

#include "xmmsclient/xmmsclient.h"

xmmsc_request_t *
xmmsc_request_new (xmmsc_connection_t *c, xmms_ipc_msg_t *msg)
{
	xmmsc_request_t *req;

	if (!c)
		fprintf (stderr, "BYE\n\n");

	if (!(req = x_new0 (xmmsc_request_t, 1))) {
		return NULL;
	}

	req->c = c;
	req->msg = msg;
	req->cb = NULL;
	req->interval = -1;

	xmms_ipc_msg_set_cookie (msg, xmmsc_next_id (c));

	return req;
}

void
xmmsc_request_set_callback(xmmsc_request_t *req, xmmsc_request_callback_t cb)
{
	req->cb = cb;
}

int
xmmsc_request_send (xmmsc_request_t *req)
{
	int retval;
	bool received = false;
	bool disc = false;
	xmms_ipc_msg_t *msg;
	int cookie;
	unsigned int data;
	xmms_ipc_transport_t *trans;

	/* TODO Setup the stuff to call the callback(if necessary)
	 * Add the interval stuff to the msg */

	retval = xmmsc_ipc_msg_write (req->c, req->msg);

	/* Wait for the reply now */
	if (req->interval == XMMSC_REQUEST_INTERVAL_NOW) {
		msg = xmms_ipc_msg_alloc ();
		trans = xmmsc_connection_get_transport (req->c);
		/* TODO Generalize this, this is just for testing */
		while (!received) {

			received = xmms_ipc_msg_read_transport (msg,trans,
					&disc);

			if (disc) {
				return false;
			}
		}
		cookie = xmms_ipc_msg_get_cookie (msg);

		if (cookie == xmms_ipc_msg_get_cookie (req->msg)) {
			xmms_ipc_msg_get_uint32 (msg, &data);
			if (req->cb)
				req->cb (&data);
		}

	}

	return retval;
}

xmmsc_request_t *
xmmsc_object_property_get (xmmsc_connection_t *c, int object, int property)
{
	xmms_ipc_msg_t *msg;
	int obj, prop;

	/* Probably looks like a switch or cascading if's to determine what
	 * server-side CMDS are needed to fulfull the property getting */

	msg = xmms_ipc_msg_new (object, property);

	/* If we need to add args now do it */

	return xmmsc_request_new (c, msg);
}

