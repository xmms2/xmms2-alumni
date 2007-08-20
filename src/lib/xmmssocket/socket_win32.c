#include "xmmsc/xmmsc_sockets.h"

int xmms_sockets_initialize () {
	WSADATA wsaData;
	int res = WSAStartup (MAKEWORD (2,2), &wsaData);
	if (res != NO_ERROR) {
		return 0;
	}
	return 1;
}

/**
 * Tries to set socket to non-blocking mode.
 * @param socket Socket to make non-blocking.
 * On success, returns 1.
 * On failure, closes socket and returns 0.
 */
int xmms_socket_set_nonblock (xmms_socket_t socket) {
	unsigned long yes = 1;
	int err = ioctlsocket (socket, FIONBIO, &yes);
	if (err == SOCKET_ERROR) {
		closesocket (socket);
		return 0;
	}
	return 1;

}

int xmms_socket_errno () {
	return WSAGetLastError ();
}
