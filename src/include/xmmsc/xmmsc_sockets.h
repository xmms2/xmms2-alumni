#ifndef XMMSC_SOCKETS_H
#define XMMSC_SOCKETS_H

#include <xmmsc/xmmsc_stdbool.h>

/* Windows */

#ifdef __WIN32__
/* Only support Windows XP and newer */
#define _WIN32_WINNT 0x0501
#include <Winsock2.h>
#include <Ws2tcpip.h>
typedef SOCKET xmms_socket_t;
typedef int socklen_t;
#define XMMS_INVALID_SOCKET INVALID_SOCKET
#define XMMS_EINTR WSAEINTR
#define XMMS_EAGAIN WSAEWOULDBLOCK
#define XMMS_EINPROGRESS WSAEINPROGRESS


/* UNIX */
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#define SOCKET_ERROR (-1)
#define XMMS_EINTR EINTR
#define XMMS_EINPROGRESS EINPROGRESS
#ifdef __hpux
/* on HP-UX EAGAIN != EWOULDBLOCK */
#define XMMS_EAGAIN EAGAIN
#else
#define XMMS_EAGAIN EWOULDBLOCK
#endif
typedef int xmms_socket_t;
#define XMMS_INVALID_SOCKET -1
#endif

int xmms_sockets_initialize();
int xmms_socket_set_nonblock(xmms_socket_t socket);
int xmms_socket_errno();
bool xmms_socket_error_recoverable();

#endif
