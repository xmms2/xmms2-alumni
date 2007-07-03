#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "tap.h"
#include "xmmsc/xmmsc_ipc_transport.h"

char *msg = "korv";
char *ipc_path = "unix:///tmp/xmms2.socket";

int
main (int argc, char **argv) {
	xmms_ipc_transport_t *transport;
	int fd, status;
	pid_t pid;

	PLAN_TESTS(3);

	transport = xmms_ipc_server_init (ipc_path);
	OK(transport != NULL, "connected");

	fd = xmms_ipc_transport_fd_get (transport);

	pid = fork ();

	if (!pid) {
		xmms_ipc_transport_t *client_transport;

		client_transport = xmms_ipc_client_init (ipc_path);
		if (!client_transport) {
			exit(EXIT_FAILURE);
		}

		xmms_ipc_transport_write (client_transport, msg, strlen (msg));

		exit(EXIT_SUCCESS);
	}

	fd_set set;

	FD_ZERO (&set);
	FD_SET (fd, &set);

	if (select (fd + 1, &set, NULL, NULL, NULL) > 0) {
		char buffer[sizeof (msg) + 1];

		xmms_ipc_transport_t *client = xmms_ipc_server_accept (transport);
		OK(client != NULL, "connection accepted");

		xmms_ipc_transport_read (client, buffer, strlen (msg));
		IS(buffer, msg, "got correct message");
	}

	waitpid (-1, &status, 0);

	return EXIT_STATUS;
}
