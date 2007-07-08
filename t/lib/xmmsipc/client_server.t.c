#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "tap.h"
#include "xmmsc/xmmsc_ipc_transport.h"

char *msg = "korv";
char *unix_ipc_path = "unix:///tmp/xmms2.socket";
char *tcp_ipc_path = "tcp://localhost:12345";

void
test_korv (char *ipc_path) {
	xmms_ipc_transport_t *transport;
	int fd, status;
	pid_t pid;

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
		char *buffer = malloc (sizeof (char) * strlen (msg) + 1);

		xmms_ipc_transport_t *client = xmms_ipc_server_accept (transport);
		OK(client != NULL, "connection accepted");

		xmms_ipc_transport_read (client, buffer, strlen (msg));
		IS(buffer, msg, "got correct message");

		free (buffer);
	}

	waitpid (-1, &status, 0);
}

int
main (int argc, char **argv) {
	PLAN_TESTS(6);

	test_korv(unix_ipc_path);
	test_korv(tcp_ipc_path);

	return EXIT_STATUS;
}
