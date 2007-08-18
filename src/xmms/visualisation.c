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




/** @file
 *
 */

/* never call a fetch without a guaranteed release following! */
#define x_fetch_client(id) \
	g_mutex_lock (vis->clientlock); \
	c = get_client (id); \
	if (!c) { \
		xmms_error_set (err, XMMS_ERROR_INVAL, "invalid server-side identifier provided"); \
		g_mutex_unlock (vis->clientlock); \
		return -1; \
	} while (0);
#define x_release_client() \
	g_mutex_unlock (vis->clientlock);


#include <math.h>
#include <glib.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <sys/shm.h>
#include <sys/sem.h>
#include <sys/stat.h>

#include <unistd.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "xmmspriv/xmms_visualisation.h"
#include "xmmspriv/xmms_ipc.h"
#include "xmmspriv/xmms_sample.h"
#include "xmms/xmms_object.h"
#include "xmmspriv/xmms_log.h"

#include "xmmsc/xmmsc_visualisation.h"

#ifdef _SEM_SEMUN_UNDEFINED
	union semun {
	   int              val;    /* Value for SETVAL */
	   struct semid_ds *buf;    /* Buffer for IPC_STAT, IPC_SET */
	   unsigned short  *array;  /* Array for GETALL, SETALL */
	   struct seminfo  *__buf;  /* Buffer for IPC_INFO (Linux specific) */
	};
#endif

/** @defgroup Visualisation Visualisation
  * @ingroup XMMSServer
  * @brief Visulation draws a FFT and feeds it to the client.
  * @{
  */

/**
 * The structures for a vis client
 */

typedef struct {
	union {
		xmmsc_vis_unixshm_t shm;
		xmmsc_vis_udp_t udp;
	} transport;
	xmmsc_vis_transport_t type;
	unsigned short format;
	xmmsc_vis_properties_t prop;
} xmms_vis_client_t;

/**
 * The structures for the vis module
 */

struct xmms_visualisation_St {
	xmms_object_t object;
	xmms_output_t *output;
	int socket;
	GIOChannel *socketio;

	GMutex *clientlock;
	int32_t clientc;
	xmms_vis_client_t **clientv;
};

/* TODO: handle singleton state properly */
static xmms_visualisation_t *vis;

XMMS_CMD_DEFINE (query_version, xmms_visualisation_version, xmms_visualisation_t *, UINT32, NONE, NONE);
XMMS_CMD_DEFINE (registercl, xmms_visualisation_register_client, xmms_visualisation_t *, INT32, NONE, NONE);
XMMS_CMD_DEFINE (init_shm, xmms_visualisation_init_shm, xmms_visualisation_t *, INT32, INT32, INT32);
XMMS_CMD_DEFINE (init_udp, xmms_visualisation_init_udp, xmms_visualisation_t *, INT32, INT32, NONE);
XMMS_CMD_DEFINE3 (property_set, xmms_visualisation_property_set, xmms_visualisation_t *, INT32, INT32, STRING, STRING);
XMMS_CMD_DEFINE (properties_set, xmms_visualisation_properties_set, xmms_visualisation_t *, INT32, INT32, STRINGLIST);
XMMS_CMD_DEFINE (shutdown, xmms_visualisation_shutdown_client, xmms_visualisation_t *, NONE, INT32, NONE);

/* create an uninitialised vis client. don't use this method without mutex! */
int32_t
create_client() {
	int32_t id;

	for (id = 0; id < vis->clientc; ++id) {
		if (!vis->clientv[id]) {
			break;
		}
	}

	if (id == vis->clientc) {
		vis->clientc++;
	}

	vis->clientv = g_renew (xmms_vis_client_t*, vis->clientv, vis->clientc);
	if (!vis->clientv || (!(vis->clientv[id] = g_new (xmms_vis_client_t, 1)))) {
		vis->clientc = 0;
		id = -1;
	}

	xmms_log_info ("Attached visualisation client %d", id);
	return id;
}

xmms_vis_client_t *
get_client (int32_t id) {
	if (id < 0 || id >= vis->clientc) {
		return NULL;
	}

	return vis->clientv[id];
}

/* delete a vis client. don't use this method without mutex! */
void
delete_client (int32_t id) {
	xmms_vis_client_t *c;

	if (id < 0 || id >= vis->clientc) {
		return;
	}

	c = vis->clientv[id];
	if (c == NULL) {
		return;
	}

	if (c->type == VIS_UNIXSHM) {
		shmdt (c->transport.shm.buffer);
		semctl (c->transport.shm.semid, 0, IPC_RMID, 0);
	} else {
		// TODO: UDP issue killer packet?
	}

	g_free (c);
	vis->clientv[id] = NULL;

	xmms_log_info ("Removed visualisation client %d", id);
}

/**
 * Initialize the Vis module.
 */
void
xmms_visualisation_init ()
{
	vis = xmms_object_new (xmms_visualisation_t, xmms_visualisation_destroy);
	vis->clientlock = g_mutex_new ();
	vis->clientc = 0;

	xmms_ipc_object_register (XMMS_IPC_OBJECT_VISUALISATION, XMMS_OBJECT (vis));
	xmms_object_cmd_add (XMMS_OBJECT (vis),
					 XMMS_IPC_CMD_VISUALISATION_QUERY_VERSION,
					 XMMS_CMD_FUNC (query_version));
	xmms_object_cmd_add (XMMS_OBJECT (vis),
					 XMMS_IPC_CMD_VISUALISATION_REGISTER,
					 XMMS_CMD_FUNC (registercl));
	xmms_object_cmd_add (XMMS_OBJECT (vis),
					 XMMS_IPC_CMD_VISUALISATION_INIT_SHM,
					 XMMS_CMD_FUNC (init_shm));
	xmms_object_cmd_add (XMMS_OBJECT (vis),
					 XMMS_IPC_CMD_VISUALISATION_INIT_UDP,
					 XMMS_CMD_FUNC (init_udp));
	xmms_object_cmd_add (XMMS_OBJECT (vis),
					 XMMS_IPC_CMD_VISUALISATION_PROPERTY,
					 XMMS_CMD_FUNC (property_set));
	xmms_object_cmd_add (XMMS_OBJECT (vis),
					 XMMS_IPC_CMD_VISUALISATION_PROPERTIES,
					 XMMS_CMD_FUNC (properties_set));
	xmms_object_cmd_add (XMMS_OBJECT (vis),
					 XMMS_IPC_CMD_VISUALISATION_SHUTDOWN,
					 XMMS_CMD_FUNC (shutdown));

	vis->socket = -1;

	//xmms_plugin_load (&xmms_builtin_visualisation, NULL);
}

/**
 * Free all resoures used by visualisation module.
 * TODO: Fill this in properly, unregister etc!
 */

void
xmms_visualisation_destroy (xmms_object_t *object)
{
	xmms_visualisation_t *vis = (xmms_visualisation_t *)object;

	/* TODO: assure that the xform is already dead! */
	g_mutex_free (vis->clientlock);
	xmms_log_debug ("starting cleanup of %d vis clients", vis->clientc);
	for (; vis->clientc > 0; --vis->clientc) {
		delete_client (vis->clientc - 1);
	}

	if (vis->socket != -1) {
		// it seems there is no way to remove the watch?
		g_io_channel_shutdown (vis->socketio, FALSE, NULL);
		close (vis->socket);
	}
	xmms_ipc_object_unregister (XMMS_IPC_OBJECT_VISUALISATION);
}

/**
 * Allocate the visualisation.
 */
xmms_visualisation_t *
xmms_visualisation_new ()
{
	xmms_object_ref (vis);
	return vis;
}

void
xmms_visualisation_register_output (xmms_output_t *output)
{
	xmms_visualisation_t *vis = xmms_visualisation_new();
	vis->output = output;
}

uint32_t
xmms_visualisation_version (xmms_visualisation_t *vis, xmms_error_t *err) {
	/* if there is a way to disable visualisation support on the server side,
	   we could return 0 here, or we could return an error? */

	return XMMS_VISPACKET_VERSION;
}

void
properties_init (xmmsc_vis_properties_t *p) {
	p->type = VIS_PCM;
	p->stereo = 1;
	p->timeframe = 0.015;
}

gboolean
property_set (xmmsc_vis_properties_t *p, gchar* key, gchar* data) {

	if (!g_strcasecmp (key, "type")) {
		if (!g_strcasecmp (data, "pcm")) {
			p->type = VIS_PCM;
		} else if (!g_strcasecmp (data, "fft")) {
			p->type = VIS_FFT;
		} else if (!g_strcasecmp (data, "peak")) {
			p->type = VIS_PEAK;
		} else {
			return FALSE;
		}
	} else if (!g_strcasecmp (key, "stereo")) {
		p->stereo = (atoi (data) > 0);
	} else if (!g_strcasecmp (key, "timeframe")) {
		p->timeframe = g_strtod (data, NULL);
		if (p->timeframe == 0.0) {
			return FALSE;
		}
		/* TODO: blah */
	} else {
		return FALSE;
	}
	return TRUE;
}

int32_t
xmms_visualisation_register_client (xmms_visualisation_t *vis, xmms_error_t *err) {
	int32_t id;
	xmms_vis_client_t *c;

	g_mutex_lock (vis->clientlock);
	id = create_client ();
	if (id < 0) {
		xmms_error_set (err, XMMS_ERROR_OOM, "could not allocate dataset");
	} else {
		/* do necessary initialisations here */
		c = get_client (id);
		c->type = VIS_NONE;
		c->format = 0;
		properties_init(&c->prop);
	}
	g_mutex_unlock (vis->clientlock);
	return id;
}


int32_t
xmms_visualisation_property_set (xmms_visualisation_t *vis, int32_t id, gchar* key, gchar* value, xmms_error_t *err)
{
	xmms_vis_client_t *c;

	x_fetch_client (id);

	if (!property_set (&c->prop, key, value)) {
		xmms_error_set (err, XMMS_ERROR_INVAL, "property could not be set!");
	}
	/* TODO: propagate new format to xform! ORLY? */

	x_release_client ();

	/* the format identifier (between client and server) changes. so the client can recognize the first packet
	   which is built using the new format according to the newly set property */
	return (++c->format);
}

int32_t
xmms_visualisation_properties_set (xmms_visualisation_t *vis, int32_t id, GList* prop, xmms_error_t *err)
{
	GList* n;
	xmms_vis_client_t *c;

	x_fetch_client (id);

	for (n = prop; n; n = n->next) {
		if (!property_set (&c->prop, (gchar*)n->data, (gchar*)n->next->data)) {
			xmms_error_set (err, XMMS_ERROR_INVAL, "property could not be set!");
		}
		n = n->next;
	}
	/* TODO: propagate new format to xform! */

	x_release_client ();

	return (++c->format);
}

int32_t
xmms_visualisation_init_shm (xmms_visualisation_t *vis, int32_t id, int32_t shmid, xmms_error_t *err)
{
	struct shmid_ds shm_desc;
	int32_t semid;
	void *buffer;
	int size;
	xmms_vis_client_t *c;
	union semun semopts;

	x_fetch_client (id);

	/* MR. DEBUG */
	/*	xmms_error_set (err, XMMS_ERROR_NO_SAUSAGE, "lame, more lame, shm!");
		x_release_client ();
		return -1; */


	/* test the shm */
	buffer = shmat(shmid, NULL, 0);
	if (buffer == (void*)-1) {
		xmms_error_set (err, XMMS_ERROR_NO_SAUSAGE, "couldn't attach to shared memory");
		x_release_client ();
		return -1;
	}
	shmctl (shmid, IPC_STAT, &shm_desc);
	size = shm_desc.shm_segsz / sizeof (xmmsc_vischunk_t);

	/* setup the semaphore set */
	semid = semget (IPC_PRIVATE, 2, S_IRWXU + S_IRWXG + S_IRWXO);
	if (semid == -1) {
		xmms_error_set (err, XMMS_ERROR_NO_SAUSAGE, "couldn't create semaphore set");
		x_release_client ();
		return -1;
	}
	/* initially set semaphores - nothing to read, buffersize to write
	   first semaphore is the server semaphore */
	semopts.val = size;
	semctl(semid, 0, SETVAL, semopts);
	semopts.val = 0;
	semctl(semid, 1, SETVAL, semopts);

	/* set up client structure */
	c->type = VIS_UNIXSHM;
	c->transport.shm.semid = semid;
	c->transport.shm.shmid = shmid;
	c->transport.shm.buffer = buffer;
	/* at the beginning, all slots are free */
	c->transport.shm.size = size;
	c->transport.shm.pos = 0;

	x_release_client ();
	//~ printf("SIZE: %d\tT: %d\t S: %d\t TF: %lf \n", c->transport.shm.size, c->prop.type, c->prop.stereo, c->prop.timeframe);
	xmms_log_info ("Visualisation client %d initialised using Unix SHM", id);
	return semid;
}

gboolean
udpwatcher (GIOChannel *src, GIOCondition cond, xmms_visualisation_t *vis)
{
	struct sockaddr_storage from;
	socklen_t sl = sizeof (from);
	xmmsc_vis_udp_data_t buf;

	if (recvfrom (vis->socket, &buf, sizeof (buf), 0, (struct sockaddr *)&from, &sl) > 0) {
		if (buf.type == 'H') {
			xmms_vis_client_t *c;
			xmmsc_vis_udp_timing_t *content = (xmmsc_vis_udp_timing_t*)&buf;
			int32_t id = ntohl (content->id);

			/* debug code starts
			char adrb[INET6_ADDRSTRLEN];
			struct sockaddr_in6 *a = (struct sockaddr_in6 *)&from;
			printf ("Client address: %s:%d, %d\n", inet_ntop (AF_INET6, &a->sin6_addr,
					adrb, INET6_ADDRSTRLEN), a->sin6_port, id);
			 debug code ends */
			g_mutex_lock (vis->clientlock);
			c = get_client (id);
			if (!c || c->type != VIS_UDP) {
				g_mutex_unlock (vis->clientlock);
				return TRUE;
			}
			/* save client address according to id */
			memcpy (&c->transport.udp.addr, &from, sizeof (from));
			c->transport.udp.socket[0] = 1;
			c->transport.udp.grace = 500;
			g_mutex_unlock (vis->clientlock);
		} else if (buf.type == 'T') {
			struct timeval time;
			xmms_vis_client_t *c;
			xmmsc_vis_udp_timing_t *content = (xmmsc_vis_udp_timing_t*)&buf;
			int32_t id = ntohl (content->id);

			g_mutex_lock (vis->clientlock);
			c = get_client (id);
			if (!c || c->type != VIS_UDP) {
				g_mutex_unlock (vis->clientlock);
				return TRUE;
			}
			c->transport.udp.grace = 500;
			g_mutex_unlock (vis->clientlock);

			/* give pong */
			gettimeofday (&time, NULL);
			tv2net (content->serverstamp, ts2tv (&time) - net2tv (content->clientstamp));
			sendto (vis->socket, content, sizeof (xmmsc_vis_udp_timing_t), 0, (struct sockaddr *)&from, sl);

			/* new debug:
			printf ("Timings: local %f, remote %f, diff %f\n", ts2tv (&time), net2tv (&buf[1]), net2tv (&buf[1]) - ts2tv (&time));
			 ends */
		} else {
			xmms_log_error ("Received invalid UDP package!");
		}
	}
	return TRUE;
}

int32_t
xmms_visualisation_init_udp (xmms_visualisation_t *vis, int32_t id, xmms_error_t *err)
{
	// TODO: we need the currently used port, not only the default one! */
	int32_t port = XMMS_DEFAULT_TCP_PORT;
	xmms_vis_client_t *c;

	// setup socket if needed
	if (vis->socket == -1) {
		struct addrinfo hints;
		struct addrinfo *result, *rp;
		int s;

		memset (&hints, 0, sizeof (hints));
		hints.ai_family	= AF_UNSPEC;
		hints.ai_socktype = SOCK_DGRAM;
		hints.ai_flags = AI_PASSIVE;
		hints.ai_protocol = 0;

		if ((s = getaddrinfo (NULL, G_STRINGIFY (XMMS_DEFAULT_TCP_PORT), &hints, &result)) != 0)
		{
			xmms_log_fatal ("Could not setup socket! getaddrinfo: %s", gai_strerror (s));
			xmms_error_set (err, XMMS_ERROR_NO_SAUSAGE, "Could not setup socket!");
			return -1;
		}

		for (rp = result; rp != NULL; rp = rp->ai_next) {
			if ((vis->socket = socket (rp->ai_family, rp->ai_socktype, rp->ai_protocol)) == -1) {
				continue;
			}
			if (bind (vis->socket, rp->ai_addr, rp->ai_addrlen) != -1) {
				break;
			} else {
				close (vis->socket);
			}
		}
		if (rp == NULL) {
			xmms_log_fatal ("Could not bind socket!");
			xmms_error_set (err, XMMS_ERROR_NO_SAUSAGE, "Could not bind socket!");
			freeaddrinfo (result);
			return -1;
		}
		freeaddrinfo (result);

		// register into mainloop:
		vis->socketio = g_io_channel_unix_new (vis->socket);
		g_io_channel_set_encoding (vis->socketio, NULL, NULL);
		g_io_channel_set_buffered (vis->socketio, FALSE);
		g_io_add_watch (vis->socketio, G_IO_IN, (GIOFunc) udpwatcher, vis);
	}

	/* set up client structure */
	x_fetch_client (id);
	c->type = VIS_UDP;
	memset (&c->transport.udp.addr, 0, sizeof (c->transport.udp.addr));
	c->transport.udp.socket[0] = 0;
	x_release_client ();

	xmms_log_info ("Visualisation client %d initialised using UDP", id);
	// return socketport
	return port;
}

void
xmms_visualisation_shutdown_client (xmms_visualisation_t *vis, int32_t id, xmms_error_t *err)
{
	g_mutex_lock (vis->clientlock);
	delete_client (id);
	g_mutex_unlock (vis->clientlock);
}


/**
 * Decrements the server's semaphor (to write the next chunk)
 */
gboolean
decrement_server (xmmsc_vis_unixshm_t *t) {
	/* alter semaphore 0 by -1, don't block */
	struct sembuf op = { 0, -1, IPC_NOWAIT };

	/*{
		int a = semctl(t->semid, 0, GETVAL, 0);
		int b = semctl(t->semid, 1, GETVAL, 0);
		printf("DECR | %d, %d | pos %d\n", a, b, t->pos);
	}*/

	while (semop (t->semid, &op, 1) == -1) {
		switch (errno) {
		case EINTR:
			break;
		case EAGAIN:
			return FALSE;
		default:
			perror("Skipping visualisation package");
			return FALSE;
		}
	}
	return TRUE;
}

/**
 * Increments the client's semaphor (after a chunk was written)
 */
void
increment_client (xmmsc_vis_unixshm_t *t) {
	/* alter semaphore 1 by 1, no flags */
	struct sembuf op = { 1, +1, 0 };

	/*{
		int a = semctl(t->semid, 0, GETVAL, 0);
		int b = semctl(t->semid, 1, GETVAL, 0);
		printf("INCR | %d, %d | pos %d\n", a, b, t->pos);
	}*/

	if (semop (t->semid, &op, 1) == -1) {
		/* there should not occur any error */
		perror("");
	}
}

gboolean
package_write_start (int32_t id, xmms_vis_client_t* c, xmmsc_vischunk_t **dest) {
	if (c->type == VIS_UNIXSHM) {
		struct shmid_ds shm_desc;
		xmmsc_vis_unixshm_t *t = &c->transport.shm;

		/* first check if the client is still there */
		shmctl (t->shmid, IPC_STAT, &shm_desc);
		if (shm_desc.shm_nattch == 1) {
			delete_client(id);
			return FALSE;
		}

		if (!decrement_server (t)) {
			return FALSE;
		}

		*dest = &t->buffer[t->pos];
		return TRUE;
	}
	if (c->type == VIS_UDP) {
		/* first check if the client is still there */
		if (c->transport.udp.grace == 0) {
			delete_client(id);
			return FALSE;
		}

		if (c->transport.udp.socket == 0) {
			return FALSE;
		}
		xmmsc_vis_udp_data_t *packet = g_new (xmmsc_vis_udp_data_t, 1);
		packet->type = 'V';
		packet->grace = --c->transport.udp.grace;
		*dest = &packet->data;
		return TRUE;
	}
	return FALSE;
}

void
package_write_finish (int32_t id, xmms_vis_client_t* c, xmmsc_vischunk_t *dest) {
	if (c->type == VIS_UNIXSHM) {
		xmmsc_vis_unixshm_t *t = &c->transport.shm;
		t->pos = (t->pos + 1) % t->size;
		increment_client (t);
	}
	if (c->type == VIS_UDP) {
		socklen_t sl = sizeof (c->transport.udp.addr);
		/* debug code starts
		char adrb[INET6_ADDRSTRLEN];
		struct sockaddr_in6 *a = (struct sockaddr_in6 *)&c->transport.udp.addr;
		printf ("Destination: %s:%d, %d\n", inet_ntop (AF_INET6, &a->sin6_addr,
		        adrb, INET6_ADDRSTRLEN), a->sin6_port, id);
		 debug code ends */
		/* nasty trick that won't get through code review */
		int offset[2];
		xmmsc_vis_udp_data_t *packet = 0;
		offset[0] = ((int)&packet->data - (int)packet);
		packet = (xmmsc_vis_udp_data_t*)((char*)dest - offset[0]);
		offset[1] = ((int)&packet->data.data - (int)&packet->data);
		sendto (vis->socket, packet, offset[0] + offset[1] + dest->size * sizeof(int16_t), 0, (struct sockaddr *)&c->transport.udp.addr, sl);
		g_free (packet);
	}
}

/* you know ... */
short
fill_buffer (int16_t *dest, xmmsc_vis_properties_t* prop, int channels, int size, short *src) {
	int i, j;
	if (prop->type == VIS_PEAK) {
		short l = 0, r = 0;
		for (i = 0; i < size; i += channels) {
			if (src[i] > 0 && src[i] > l) {
				l = src[i];
			}
			if (src[i] < 0 && -src[i] > l) {
				l = -src[i];
			}
			if (channels > 1) {
				if (src[i+1] > 0 && src[i+1] > r) {
					r = src[i+1];
				}
				if (src[i+1] < 0 && -src[i+1] > r) {
					r = -src[i+1];
				}
			}
		}
		if (channels == 1) {
			r = l;
		}
		if (prop->stereo) {
			dest[0] = htons (l);
			dest[1] = htons (r);
			size = 2;
		} else {
			dest[0] = htons ((l + r) / 2);
			size = 1;
		}
	}
	if (prop->type == VIS_PCM) {
		for (i = 0, j = 0; i < size; i += channels, j++) {
			dest[j] = htons (src[i]);
			if (prop->stereo) {
				if (channels > 1) {
					dest[j*2] = htons (src[i+1]);
				} else {
					dest[j*2] = htons (src[i]);
				}
			}
		}
		size /= channels;
		if (prop->stereo) {
			size *= 2;
		}
	}
	return size;
}


/* TODO: sick in various ways */
void
xmms_visualisation_send_data (xmms_visualisation_t *vis, int channels, int size, short *buf) {
	int i;
	xmmsc_vischunk_t *dest;
	struct timeval time;
	guint32 latency = xmms_output_latency (vis->output);

	gettimeofday (&time, NULL);
	g_mutex_lock (vis->clientlock);
	for (i = 0; i < vis->clientc; ++i) {
		if (vis->clientv[i]) {
			if (!package_write_start (i, vis->clientv[i], &dest)) {
				continue;
			}

			tv2net (dest->timestamp, ts2tv (&time) + latency * 0.001);
			dest->format = htons (vis->clientv[i]->format);

			dest->size = htons (fill_buffer (dest->data, &vis->clientv[i]->prop, channels, size, buf));

			package_write_finish (i, vis->clientv[i], dest);
		}
	}
	g_mutex_unlock (vis->clientlock);
}

/** @} */
