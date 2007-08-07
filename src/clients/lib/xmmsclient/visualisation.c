/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2007 XMMS2 Team
 *
 *  PLUGINS ARE NOT CONSIDERED TO BE DERIVED WORK !!!
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include <sys/shm.h>
#include <sys/sem.h>
#include <sys/stat.h>

#include "xmmsclient/xmmsclient.h"
#include "xmmsclientpriv/xmmsclient.h"

#include "xmmsc/xmmsc_ipc_transport.h"
#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_visualisation.h"

/**
 * @defgroup Visualisation Visualisation
 * @ingroup XMMSClient
 * @brief This manages the visualisation transfer
 *
 * @{
 */

struct xmmsc_visualisation_St {
	union {
		xmmsc_vis_unixshm_t shm;
		xmmsc_vis_udp_t udp;
	} transport;
	xmmsc_vis_transport_t type;
	/** server side identifier */
	int32_t id;
};

xmmsc_visualisation_t *
get_dataset(xmmsc_connection_t *c, int vv) {
	if (vv < 0 || vv >= c->visc)
		return NULL;

	return c->visv[vv];
}

/**
 * Querys the visualisation version
 */

xmmsc_result_t *
xmmsc_visualisation_version (xmmsc_connection_t *c) {
	x_check_conn (c, NULL);

	return xmmsc_send_msg_no_arg (c, XMMS_IPC_OBJECT_VISUALISATION, XMMS_IPC_CMD_VISUALISATION_QUERY_VERSION);
}

/**
 * Initializes a new visualisation dataset
 */

int
xmmsc_visualisation_init (xmmsc_connection_t *c) {
	xmmsc_result_t *res;

	x_check_conn (c, 0);

	c->visc++;
	c->visv = realloc (c->visv, sizeof (xmmsc_visualisation_t*) * c->visc);
	if (!c->visv) {
		x_oom ();
		c->visc = 0;
	}
	if (c->visc > 0) {
		int vv = c->visc-1;
		if (!(c->visv[vv] = x_new0 (xmmsc_visualisation_t, 1))) {
			x_oom ();
		} else {
			res = xmmsc_send_msg_no_arg (c, XMMS_IPC_OBJECT_VISUALISATION, XMMS_IPC_CMD_VISUALISATION_REGISTER);
			xmmsc_result_wait (res);
			if (xmmsc_result_iserror (res)) {
				c->error = strdup("Couldn't register to the server!");
				return -1;
			}
			xmmsc_result_get_int (res, &c->visv[vv]->id);
			c->visv[vv]->type = VIS_NONE;
			xmmsc_result_unref (res);
		}
	}
	return c->visc-1;
}

int
setup_udp (xmmsc_visualisation_t *v, xmmsc_connection_t *c, int32_t port) {
	int socknum;
	struct addrinfo hints;
	struct addrinfo *result, *rp;
	int i, cnt;
	double lag;
	struct timeval time;
	// first is id, next four describe time
	int32_t buf[1 + 2 + 2];
	double diff = 0.0;
	int diffc = 0;
	char *host;
	char portstr[10];
	sprintf (portstr, "%d", port);

	memset (&hints, 0, sizeof (hints));
	hints.ai_family	= AF_UNSPEC;
	hints.ai_socktype = SOCK_DGRAM;
	hints.ai_flags = 0;
	hints.ai_protocol = 0;

	host = xmms_ipc_hostname (c->path);
	if (!host) {
		host = strdup ("localhost");
	}

	if (getaddrinfo (host, portstr, &hints, &result) != 0)
	{
		c->error = strdup("Couldn't setup socket!");
		return 0;
	}
	free (host);

	for (rp = result; rp != NULL; rp = rp->ai_next) {
		if ((socknum = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol)) == -1) {
			continue;
		}
		if (connect (socknum, rp->ai_addr, rp->ai_addrlen) != -1) {
			break;
		} else {
			close (socknum);
		}
	}
	if (rp == NULL) {
		c->error = strdup("Could not connect!");
		return 0;
	}
	freeaddrinfo (result);

	gettimeofday (&time, NULL);
	buf[0] = htonl (v->id);
	ts2net (&buf[1], &time);
	for (i = 0; i < 10; ++i) {
		send (socknum, buf, sizeof (buf), 0);
	}
	do { /* TODO: Handle lost packages! */
		cnt = recv (socknum, buf, sizeof (buf), 0);
		if (cnt == sizeof (buf)) {
			gettimeofday (&time, NULL);
			lag = (ts2tv (&time) - net2tv (&buf[1])) / 2.0;
			diffc++;
			diff += net2tv (&buf[3]) - lag;
			/* debug output
			printf("server diff: %f \t old timestamp: %f, new timestamp %f\n", net2tv (&buf[3]), net2tv (&buf[1]), ts2tv (&time));
			 end of debug */
		}
	} while (cnt > 0 && diffc < 10);
	diff /= (double)diffc;
	v->transport.udp.socket = socknum;
	v->transport.udp.timediff = diff;
	return 1;
}

/**
 * Initializes a new visualisation connection
 */

xmmsc_result_t *
xmmsc_visualisation_start (xmmsc_connection_t *c, int vv) {
	xmms_ipc_msg_t *msg;
	xmmsc_result_t *res;
	xmmsc_visualisation_t *v;

	int32_t shmid;

	/* we can't transmit 64 bit int yet, but it could be that shmget() gives one */
	x_api_error_if (sizeof(int) != sizeof(int32_t), "on yet unsupported 64 bit machine", NULL);

	x_api_error_if (!(v = get_dataset(c, vv)), "with unregistered/unconnected visualisation dataset", NULL);

	x_api_error_if (!(v->type == VIS_NONE), "with already transmitting visualisation dataset", NULL);

	x_check_conn (c, NULL);

	/* prepare unixshm + semaphores */
	  /* following means everyone on the system could inject wrong vis data ;) */
	shmid = shmget (IPC_PRIVATE, sizeof (xmmsc_vispacket_t) * XMMS_VISPACKET_SHMCOUNT, S_IRWXU + S_IRWXG + S_IRWXO);
	if (shmid == -1) {
		c->error = strdup("Couldn't create the shared memory!");
		return NULL;
	}

	/* send packet */
	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALISATION, XMMS_IPC_CMD_VISUALISATION_INIT_SHM);
	xmms_ipc_msg_put_int32 (msg, v->id);
	xmms_ipc_msg_put_int32 (msg, shmid);
	res = xmmsc_send_msg (c, msg);

	/* find out if it worked */
	xmmsc_result_wait (res);
	if (!xmmsc_result_iserror (res)) {
		v->type = VIS_UNIXSHM;
		v->transport.shm.buffer = shmat(shmid, NULL, SHM_RDONLY);
		v->transport.shm.size = XMMS_VISPACKET_SHMCOUNT;
		v->transport.shm.pos = 0;
		xmmsc_result_get_int (res, &v->transport.shm.semid);
		/* In either case, mark the shared memory segment to be destroyed.
		   The segment will only actually be destroyed after the last process detaches it. */
		shmctl(shmid, IPC_RMID, NULL);
	} else {
		/* see above */
		shmctl(shmid, IPC_RMID, NULL);

		/* try udp here */
		/* send packet */
		xmmsc_result_unref (res);
		msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALISATION, XMMS_IPC_CMD_VISUALISATION_INIT_UDP);
		xmms_ipc_msg_put_int32 (msg, v->id);
		res = xmmsc_send_msg (c, msg);
		xmmsc_result_wait (res);
		if (!xmmsc_result_iserror (res)) {
			int port;
			v->type = VIS_UDP;
			xmmsc_result_get_int (res, &port);
			if (!setup_udp (v, c, port)) {
				// error occured
				return NULL;
			}
		} else {
			msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALISATION, XMMS_IPC_CMD_VISUALISATION_SHUTDOWN);
			xmms_ipc_msg_put_int32 (msg, v->id);
			xmmsc_send_msg (c, msg);
		}
	}
	return res;
}

/**
 * Deliver one property
 */
xmmsc_result_t *
xmmsc_visualisation_property_set (xmmsc_connection_t *c, int vv, const char* key, const char* value)
{
	xmms_ipc_msg_t *msg;
	xmmsc_visualisation_t *v;

	x_check_conn (c, NULL);
	v = get_dataset(c, vv);
	x_api_error_if (!(v = get_dataset(c, vv)), "with unregistered visualisation dataset", NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALISATION, XMMS_IPC_CMD_VISUALISATION_PROPERTY);
	xmms_ipc_msg_put_int32 (msg, v->id);
	//xmms_ipc_msg_put_string (msg, key);
	//xmms_ipc_msg_put_string (msg, value);
	return xmmsc_send_msg (c, msg);
}

/**
 * Deliver some properties
 */
xmmsc_result_t *
xmmsc_visualisation_properties_set (xmmsc_connection_t *c, int vv, const char* prop[])
{
	xmms_ipc_msg_t *msg;
	xmmsc_visualisation_t *v;

	x_check_conn (c, NULL);
	x_api_error_if (!(v = get_dataset(c, vv)), "with unregistered visualisation dataset", NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALISATION, XMMS_IPC_CMD_VISUALISATION_PROPERTIES);
	xmms_ipc_msg_put_int32 (msg, v->id);
	xmms_ipc_msg_put_string_list (msg, prop);
	return xmmsc_send_msg (c, msg);
}

/**
 * Says goodbye and cleans up
 */

xmmsc_result_t *
xmmsc_visualisation_shutdown (xmmsc_connection_t *c, int vv)
{
	xmms_ipc_msg_t *msg;
	xmmsc_result_t *res;
	xmmsc_visualisation_t *v;

	x_check_conn (c, NULL);
	x_api_error_if (!(v = get_dataset(c, vv)), "with unregistered visualisation dataset", NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALISATION, XMMS_IPC_CMD_VISUALISATION_SHUTDOWN);
	xmms_ipc_msg_put_int32 (msg, v->id);
	res = xmmsc_send_msg (c, msg);

	/* detach from shm, close socket.. */
	if (v->type == VIS_UNIXSHM) {
		shmdt (v->transport.shm.buffer);
	}

	free(v);
	c->visv[vv] = NULL;
	return res;
}

/**
 * Decrements the client's semaphor (to read the next available chunk)
 */
int
decrement_client (xmmsc_vis_unixshm_t *t) {
	/* alter semaphore 1 by -1, no flags */
	struct sembuf op = { 1, -1, 0 };

	/*{
		int a = semctl(t->semid, 0, GETVAL, 0);
		int b = semctl(t->semid, 1, GETVAL, 0);
		printf("DECR | %d, %d | pos %d\n", a, b, t->pos);
	}*/

	while (semop (t->semid, &op, 1) == -1) {
		switch (errno) {
		case EINVAL:
		case EIDRM:
			return 0;
		case EINTR:
			break;
		default: perror ("Unexpected semaphore problem");
		}
	}
	return 1;
}

/**
 * Increments the server's semaphor (after a chunk was read)
 */
void
increment_server (xmmsc_vis_unixshm_t *t) {
	/* alter semaphore 0 by 1, no flags */
	struct sembuf op = { 0, +1, 0 };

	/*{
		int a = semctl(t->semid, 0, GETVAL, 0);
		int b = semctl(t->semid, 1, GETVAL, 0);
		printf("INCR | %d, %d | pos %d\n", a, b, t->pos);
	}*/

	if (semop (t->semid, &op, 1) == -1) {
		/* there should not occur any error */
	}
}

int
package_read_start (xmmsc_visualisation_t *v, int blocking, xmmsc_vispacket_t **dest) {
	if (v->type == VIS_UNIXSHM) {
		xmmsc_vis_unixshm_t *t = &v->transport.shm;
		if (!blocking) {
			/* test first */
			int v = semctl(t->semid, 1, GETVAL, 0);
			if (v == -1) {
				return -1;
			}
			if (v == 0) {
				return 1;
			}
		}
		if (!decrement_client (t)) {
			return -1;
		}
		*dest = &t->buffer[t->pos];
		return 0;
	}
	if (v->type == VIS_UDP) {
		int cnt;
		xmmsc_vispacket_t *buf;
		xmmsc_vis_udp_t *t = &v->transport.udp;
		*dest = malloc (sizeof (xmmsc_vispacket_t));
		buf = *dest;

		if (blocking) {
			cnt = recv (t->socket, buf, sizeof (xmmsc_vispacket_t), 0);
		} else {
			cnt = recv (t->socket, buf, sizeof (xmmsc_vispacket_t), MSG_DONTWAIT);
			if (cnt == -1 && errno == EAGAIN) {
				return 1;
			}
		}
		/* TODO: make this usable */
		if (cnt == sizeof (xmmsc_vispacket_t)) {
			/* this is nasty */
			double interim = net2tv (buf->timestamp);
			interim -= t->timediff;
			tv2net (buf->timestamp, interim);
			return 0;
		} else if (cnt > -1) {
			return 1;
		} else {
			return -1;
		}
	}
	return -1;
}

void
package_read_finish (xmmsc_visualisation_t *v, int blocking, xmmsc_vispacket_t *dest) {
	if (v->type == VIS_UNIXSHM) {
		xmmsc_vis_unixshm_t *t = &v->transport.shm;
		t->pos = (t->pos + 1) % t->size;
		increment_server (t);
	} else if (v->type == VIS_UDP) {
		free (dest);
	}
}

/**
 * Fetches the next available data chunk
 */

int
xmmsc_visualisation_chunk_get (xmmsc_connection_t *c, int vv, void *data, int drawtime, int blocking) {
	xmmsc_visualisation_t *v;
	xmmsc_vispacket_t *src;
	struct timeval time;
	double diff;
	struct timespec sleeptime;
	int old;
	int ret;

	x_check_conn (c, 0);
	x_api_error_if (!(v = get_dataset(c, vv)), "with unregistered visualisation dataset", 0);

	while (1) {
		ret = package_read_start (v, blocking, &src);
		if (ret != 0) {
			return ret;
		}

		if (drawtime >= 0) {
			gettimeofday (&time, NULL);
			diff = net2tv (src->timestamp) - ts2tv (&time);
			if (diff >= 0) {
				double dontcare;
				old = 0;
				/* nanosleep has a garantueed granularity of 10 ms.
				   to not sleep too long, we sleep 10 ms less than intended */
				diff -= (drawtime + 10) * 0.001;
				sleeptime.tv_sec = diff;
				sleeptime.tv_nsec = modf (diff, &dontcare) * 1000000000;
				while (nanosleep (&sleeptime, &sleeptime) == -1) {
					if (errno != EINTR) {
						break;
					}
				};
			} else {
				old = 1;
			}
		}
		if (!old) {
			/* TODO: make this usable ;-) */
			short *source = (short*)src->data;
			short *dest = (short*)data;
			dest[0] = ntohs (source[0]);
			dest[1] = ntohs (source[1]);
		}
		package_read_finish (v, blocking, src);
		if (!old) {
			return 0;
		}
	}
}

/** @} */

