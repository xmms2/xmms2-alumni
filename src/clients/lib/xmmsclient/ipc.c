#include "xmmsclient/xmmsclient.h"

int
xmmsc_ipc_msg_write(xmmsc_connection_t *c, xmms_ipc_msg_t *msg)
{
	bool disconnected;
	bool sent = false;

	while (!sent) {
		sent = xmms_ipc_msg_write_transport (msg,
				xmmsc_connection_get_transport (c), &disconnected);
		if (disconnected)
			return false; //Should disconnect properly
	}

	return true;
}
