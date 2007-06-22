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

	if (!(c = x_new0 (xmmsc_request_t, 1))) {
		return NULL;
	}

	req->c = c;
	req->msg = msg;
	req->cb = NULL;

	return req;
}

void
xmmsc_request_set_callback(xmmsc_request_t *req, xmmsc_request_callback_t *cb)
{
	x_api_error_if (!req, "with NULL request", NULL);

	req->cb = cb;
}

int
xmmsc_request_send (xmmsc_request_t *req)
{
	/* TODO Setup the stuff to call the callback(if necessary) */
	return xmmsc_ipc_msg_write (req->c, req->msg);
}

xmmsc_request_t *
xmmsc_object_property_get (xmmsc_connection_t *c, int object, int property)
{
	xmms_ipc_msg_t *msg;
	int obj, prop;

	/* Probably looks like a switch or cascading if's to determine what
	 * server-side CMDS are needed to fulfull the property getting */

	msg = xmms_ipc_msg_new (obj, prop);
	xmms_ipc_msg_set_cookie (msg, xmmsc_next_id (c));

	/* If we need to add args now do it */

	return xmmsc_request_new (c, msg);
}

