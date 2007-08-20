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
#include <time.h>

#include "xmmsclient/xmmsclient.h"
#include "xmmsclientpriv/xmmsclient.h"

#include "xmmsc/xmmsc_ipc_transport.h"
#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_visualization.h"

/**
 * @defgroup Visualization Visualization
 * @ingroup XMMSClient
 * @brief This manages the visualization transfer
 *
 * @{
 */

struct xmmsc_visualization_St {
	union {
		xmmsc_vis_unixshm_t shm;
		xmmsc_vis_udp_t udp;
	} transport;
	xmmsc_vis_transport_t type;
	/** server side identifier */
	int32_t id;
};

xmmsc_visualization_t *
get_dataset(xmmsc_connection_t *c, int vv) {
	if (vv < 0 || vv >= c->visc)
		return NULL;

	return c->visv[vv];
}

/**
 * Querys the visualization version
 */

xmmsc_result_t *
xmmsc_visualization_version (xmmsc_connection_t *c) {
	x_check_conn (c, NULL);

	return xmmsc_send_msg_no_arg (c, XMMS_IPC_OBJECT_VISUALIZATION, XMMS_IPC_CMD_VISUALIZATION_QUERY_VERSION);
}

/**
 * Initializes a new visualization dataset
 */

int
xmmsc_visualization_init (xmmsc_connection_t *c) {
	xmmsc_result_t *res;

	x_check_conn (c, 0);

	c->visc++;
	c->visv = realloc (c->visv, sizeof (xmmsc_visualization_t*) * c->visc);
	if (!c->visv) {
		x_oom ();
		c->visc = 0;
	}
	if (c->visc > 0) {
		int vv = c->visc-1;
		if (!(c->visv[vv] = x_new0 (xmmsc_visualization_t, 1))) {
			x_oom ();
		} else {
			res = xmmsc_send_msg_no_arg (c, XMMS_IPC_OBJECT_VISUALIZATION, XMMS_IPC_CMD_VISUALIZATION_REGISTER);
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

double
udp_timediff (int32_t id, int socket) {
	int i;
	double lag;
	struct timeval time;
	xmmsc_vis_udp_data_t buf;
	xmmsc_vis_udp_timing_t *content = (xmmsc_vis_udp_timing_t*)&buf;
	double diff = 0.0;
	int diffc = 0;

	gettimeofday (&time, NULL);
	content->type = 'T';
	content->id = htonl (id);
	tv2net (content->clientstamp, &time);
	/* TODO: handle lost packages! */
	for (i = 0; i < 10; ++i) {
		send (socket, content, sizeof (xmmsc_vis_udp_timing_t), 0);
	}
	do {
		if (recv (socket, &buf, sizeof (buf), 0) > 0 && buf.type == 'T') {
			gettimeofday (&time, NULL);
			lag = (tv2ts (&time) - net2ts (content->clientstamp)) / 2.0;
			diffc++;
			diff += net2ts (content->serverstamp) - lag;
			/* debug output
			printf("server diff: %f \t old timestamp: %f, new timestamp %f\n",
			       net2ts (content->serverstamp), net2ts (content->clientstamp), tv2ts (&time));
			 end of debug */
		}
	} while (diffc < 10);

	return diff / (double)diffc;
}

int
setup_udp (xmmsc_visualization_t *v, xmmsc_connection_t *c, int32_t port) {
	struct addrinfo hints;
	struct addrinfo *result, *rp;
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
		if ((v->transport.udp.socket[0] = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol)) == -1) {
			continue;
		}
		if (connect (v->transport.udp.socket[0], rp->ai_addr, rp->ai_addrlen) != -1) {
			/* init fallback socket for timing stuff */
			v->transport.udp.socket[1] = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
			connect (v->transport.udp.socket[1], rp->ai_addr, rp->ai_addrlen);
			break;
		} else {
			close (v->transport.udp.socket[0]);
		}
	}
	if (rp == NULL) {
		c->error = strdup("Could not connect!");
		return 0;
	}
	freeaddrinfo (result);
	/* TODO: do this properly! */
	xmmsc_vis_udp_timing_t content;
	content.type = 'H';
	content.id = htonl (v->id);
	send (v->transport.udp.socket[0], &content, sizeof (xmmsc_vis_udp_timing_t), 0);
	v->transport.udp.timediff = udp_timediff (v->id, v->transport.udp.socket[1]);
//	printf ("diff: %f\n", v->transport.udp.timediff);
	return 1;
}

/**
 * Initializes a new visualization connection
 */

xmmsc_result_t *
xmmsc_visualization_start (xmmsc_connection_t *c, int vv) {
	xmms_ipc_msg_t *msg;
	xmmsc_result_t *res;
	xmmsc_visualization_t *v;

	int32_t shmid;

	/* we can't transmit 64 bit int yet, but it could be that shmget() gives one */
	x_api_error_if (sizeof(int) != sizeof(int32_t), "on yet unsupported 64 bit machine", NULL);

	x_api_error_if (!(v = get_dataset(c, vv)), "with unregistered/unconnected visualization dataset", NULL);

	x_api_error_if (!(v->type == VIS_NONE), "with already transmitting visualization dataset", NULL);

	x_check_conn (c, NULL);

	/* prepare unixshm + semaphores */
	  /* following means everyone on the system could inject wrong vis data ;) */
	shmid = shmget (IPC_PRIVATE, sizeof (xmmsc_vischunk_t) * XMMS_VISPACKET_SHMCOUNT, S_IRWXU + S_IRWXG + S_IRWXO);
	if (shmid == -1) {
		c->error = strdup("Couldn't create the shared memory!");
		return NULL;
	}

	/* send packet */
	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALIZATION, XMMS_IPC_CMD_VISUALIZATION_INIT_SHM);
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
		msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALIZATION, XMMS_IPC_CMD_VISUALIZATION_INIT_UDP);
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
			msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALIZATION, XMMS_IPC_CMD_VISUALIZATION_SHUTDOWN);
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
xmmsc_visualization_property_set (xmmsc_connection_t *c, int vv, const char* key, const char* value)
{
	xmms_ipc_msg_t *msg;
	xmmsc_visualization_t *v;

	x_check_conn (c, NULL);
	v = get_dataset(c, vv);
	x_api_error_if (!(v = get_dataset(c, vv)), "with unregistered visualization dataset", NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALIZATION, XMMS_IPC_CMD_VISUALIZATION_PROPERTY);
	xmms_ipc_msg_put_int32 (msg, v->id);
	//xmms_ipc_msg_put_string (msg, key);
	//xmms_ipc_msg_put_string (msg, value);
	return xmmsc_send_msg (c, msg);
}

/**
 * Deliver some properties
 */
xmmsc_result_t *
xmmsc_visualization_properties_set (xmmsc_connection_t *c, int vv, const char* prop[])
{
	xmms_ipc_msg_t *msg;
	xmmsc_visualization_t *v;

	x_check_conn (c, NULL);
	x_api_error_if (!(v = get_dataset(c, vv)), "with unregistered visualization dataset", NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALIZATION, XMMS_IPC_CMD_VISUALIZATION_PROPERTIES);
	xmms_ipc_msg_put_int32 (msg, v->id);
	xmms_ipc_msg_put_string_list (msg, prop);
	return xmmsc_send_msg (c, msg);
}

/**
 * Says goodbye and cleans up
 */

xmmsc_result_t *
xmmsc_visualization_shutdown (xmmsc_connection_t *c, int vv)
{
	xmms_ipc_msg_t *msg;
	xmmsc_result_t *res;
	xmmsc_visualization_t *v;

	x_check_conn (c, NULL);
	x_api_error_if (!(v = get_dataset(c, vv)), "with unregistered visualization dataset", NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALIZATION, XMMS_IPC_CMD_VISUALIZATION_SHUTDOWN);
	xmms_ipc_msg_put_int32 (msg, v->id);
	res = xmmsc_send_msg (c, msg);

	/* detach from shm, close socket.. */
	if (v->type == VIS_UNIXSHM) {
		shmdt (v->transport.shm.buffer);
	}
	if (v->type == VIS_UDP) {
		close (v->transport.udp.socket[0]);
		close (v->transport.udp.socket[1]);
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
			return 2;
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
package_read_start (xmmsc_visualization_t *v, int blocking, xmmsc_vischunk_t **dest) {
	if (v->type == VIS_UNIXSHM) {
		xmmsc_vis_unixshm_t *t = &v->transport.shm;
		if (!blocking) {
			/* test first */
			int v = semctl(t->semid, 1, GETVAL, 0);
			if (v == -1) {
				return -1;
			}
			if (v == 0) {
				return 0;
			}
		}
		int decr = decrement_client (t);
		if (!decr) {
			return -1;
		}
		if (decr == 2) {
			return 0;
		}
		*dest = &t->buffer[t->pos];
		return 1;
	}
	if (v->type == VIS_UDP) {
		int cnt;
		xmmsc_vis_udp_t *t = &v->transport.udp;
		xmmsc_vis_udp_data_t *packet = malloc (sizeof (xmmsc_vis_udp_data_t));
		*dest = &packet->data;

		if (blocking) {
			blocking = MSG_DONTWAIT;
		}
		cnt = recv (t->socket[0], packet, sizeof (xmmsc_vis_udp_data_t), blocking);
		if (cnt == -1 && (errno == EAGAIN || errno == EINTR)) {
			free (packet);
			return 0;
		}
		if (cnt > 0 && packet->type == 'V') {
			if (packet->grace < 100) {
				if (t->grace != 0) {
					puts ("resync");
					t->grace = 0;
					/* use second socket here, so vis packets don't get lost */
					t->timediff = udp_timediff (v->id, t->socket[1]);
				}
			} else {
				t->grace = packet->grace;
			}
			/* this is nasty */
			double interim = net2ts (packet->data.timestamp);
			interim -= t->timediff;
			ts2net (packet->data.timestamp, interim);
			return 1;
		} else if (cnt > -1) {
			free (packet);
			return 0;
		} else {
			return -1;
		}
	}
	return -1;
}

void
package_read_finish (xmmsc_visualization_t *v, int blocking, xmmsc_vischunk_t *dest) {
	if (v->type == VIS_UNIXSHM) {
		xmmsc_vis_unixshm_t *t = &v->transport.shm;
		t->pos = (t->pos + 1) % t->size;
		increment_server (t);
	} else if (v->type == VIS_UDP) {
		xmmsc_vis_udp_data_t *packet = 0;
		int offset = ((int)&packet->data - (int)packet);
		packet = (xmmsc_vis_udp_data_t*)((char*)dest - offset);
		free (packet);
	}
}

/**
 * Fetches the next available data chunk
 */

int
xmmsc_visualization_chunk_get (xmmsc_connection_t *c, int vv, short *buffer, int drawtime, int blocking) {
	xmmsc_visualization_t *v;
	xmmsc_vischunk_t *src;
	struct timeval time;
	double diff;
	struct timespec sleeptime;
	int old;
	int i, ret, size;

	x_check_conn (c, 0);
	x_api_error_if (!(v = get_dataset(c, vv)), "with unregistered visualization dataset", 0);

	while (1) {
		ret = package_read_start (v, blocking, &src);
		if (ret < 1) {
			return ret;
		}

		if (drawtime >= 0) {
			gettimeofday (&time, NULL);
			diff = net2ts (src->timestamp) - tv2ts (&time);
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
		} else {
			old = 0;
		}
		if (!old) {
			size = ntohs (src->size);
			for (i = 0; i < size; ++i) {
				buffer[i] = (int16_t)ntohs (src->data[i]);
			}
		}
		package_read_finish (v, blocking, src);
		if (!old) {
			return size;
		}
	}
}

/** @} */

