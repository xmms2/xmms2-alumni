#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <sys/types.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#include <sys/types.h>

#include "xmmsclientpriv/xmmsclient_queue.h"
#include "xmmspriv/xmms_list.h"

#include "xmmsclient/xmmsclient.h"
#include "xmmsc/xmmsc_ipc_msg.h"
#include "xmmsc/xmmsc_ipc_transport.h"
#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_stdint.h"
#include "xmmsc/xmmsc_stringport.h"
#include "xmmsc/xmmsc_util.h"

struct xmmsc_connection_t_ {
	char *clientname;
	char *error;
	xmms_ipc_transport_t *trans;
	unsigned int id;
	x_list_t *req_list;
};

unsigned int
xmmsc_next_id (xmmsc_connection_t *c)
{
	return c->id++;
}

void
xmmsc_hello_callback (void *data)
{
	unsigned int *val = (unsigned int *)data;

	if (!(*val)) {
		/* ERROR OUT */
	}
}


xmmsc_connection_t *
xmmsc_init (const char *clientname)
{
        xmmsc_connection_t *c;
        int i = 0;
        char j;

        x_api_error_if (!clientname, "with NULL clientname", NULL);

        if (!(c = x_new0 (xmmsc_connection_t, 1))) {
                return NULL;
        }

	c->id = 0;

        while (clientname[i]) {
                j = clientname[i];
                if (!isalnum (j) && j != '_' && j != '-') {
                        /* snyggt! */
                        free (c);
                        x_api_error_if (true, "clientname contains invalid chars, just alphanumeric chars are allowed!", NULL);
                }
                i++;
        }

        if (!(c->clientname = strdup (clientname))) {
                free (c);
                return NULL;
        }

        return c;
}

xmms_ipc_transport_t *
xmmsc_connection_get_transport (xmmsc_connection_t *c)
{
	return c->trans;
}

void
hello_cb (void *data)
{
	int *d = (int *)data;

	printf("retval:%d\n",*d);
}

int
xmmsc_connect (xmmsc_connection_t *c, const char *ipcpath)
{
        xmms_ipc_msg_t *msg;
        xmmsc_request_t *req;
	uint32_t i;
        int ret;

        char path[PATH_MAX];

        x_api_error_if (!c, "with a NULL connection", false);

        if (!ipcpath) {
                if (!xmms_default_ipcpath_get (path, PATH_MAX)) {
                        return false;
                }
        } else {
                snprintf (path, sizeof (path), "%s", ipcpath);
        }

	c->trans = xmms_ipc_client_init (path);
	if (!c->trans) {
                c->error = strdup ("xmms2d is not running.");
		return false;
	}

	/* Make this a request */

	/* XXX This is pretty ad-hoc ATM, will be fixed before integrating in
	 *  real xmmsclient */

	msg = xmms_ipc_msg_new (XMMSC_IPC_OBJECT_MAIN, XMMSC_MAIN_METHOD_HELLO);
        xmms_ipc_msg_put_uint32 (msg, 1);
        xmms_ipc_msg_put_uint32 (msg, XMMSC_VERSION);
        xmms_ipc_msg_put_string (msg, c->clientname);
        req = xmmsc_request_new (c, msg);
	xmmsc_request_now (req);

	xmmsc_request_set_callback (req, hello_cb);

	xmmsc_request_send (req);

        return ret;
}

