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

#include <sys/types.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <sys/stat.h>

#include "xmmsclient/xmmsclient.h"
#include "xmmsclientpriv/xmmsclient.h"

#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_visualisation.h"

#ifdef _SEM_SEMUN_UNDEFINED
	union semun {
	   int              val;    /* Value for SETVAL */
	   struct semid_ds *buf;    /* Buffer for IPC_STAT, IPC_SET */
	   unsigned short  *array;  /* Array for GETALL, SETALL */
	   struct seminfo  *__buf;  /* Buffer for IPC_INFO (Linux specific) */
	};
#endif

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

/**
 * Initializes a new visualisation connection
 */

xmmsc_result_t *
xmmsc_visualisation_start (xmmsc_connection_t *c, int vv) {
	xmms_ipc_msg_t *msg;
	xmmsc_result_t *res;
	xmmsc_visualisation_t *v;
	union semun semopts;

	int32_t shmid, semid;

	/* we can't transmit 64 bit int yet, but it could be that shmget() gives one */
	x_api_error_if (sizeof(int) != sizeof(int32_t), "on yet unsupported 64 bit machine", NULL);

	x_api_error_if (!(v = get_dataset(c, vv)), "with unregistered/unconnected visualisation dataset", NULL);

	x_api_error_if (!(v->type == VIS_NONE), "with already transmitting visualisation dataset", NULL);

	x_check_conn (c, NULL);

	/* prepare unixshm + semaphores */
	  /* following means everyone on the system could inject wrong vis data ;) */
	shmid = shmget (IPC_PRIVATE, sizeof (xmmsc_vispacket_t) * XMMS_VISPACKET_SHMCOUNT, S_IRWXU + S_IRWXG + S_IRWXO);
	semid = semget (IPC_PRIVATE, 2, S_IRWXU + S_IRWXG + S_IRWXO);
	if (shmid == -1 || semid == -1) {
		c->error = strdup("Couldn't create the shared memory and semaphore set!");
		return NULL;
	}

	/* initially set semaphores - nothing to read, buffersize to write
	   first semaphore is the server semaphore */
	semopts.val = XMMS_VISPACKET_SHMCOUNT;
	semctl(semid, 0, SETVAL, semopts);
	semopts.val = 0;
	semctl(semid, 1, SETVAL, semopts);

	/* send packet */
	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALISATION, XMMS_IPC_CMD_VISUALISATION_INIT_SHM);
	xmms_ipc_msg_put_int32 (msg, v->id);
	xmms_ipc_msg_put_int32 (msg, shmid);
	xmms_ipc_msg_put_int32 (msg, semid);
	res = xmmsc_send_msg (c, msg);

	/* find out if it worked */
	xmmsc_result_wait (res);
	if (!xmmsc_result_iserror (res)) {
		v->type = VIS_UNIXSHM;
		v->transport.shm.buffer = shmat(shmid, NULL, SHM_RDONLY);
		v->transport.shm.semid = semid;
		v->transport.shm.size = XMMS_VISPACKET_SHMCOUNT;
		v->transport.shm.pos = 0;
	} else {
		/* try udp here */
	}

	/* In either case, mark the shared memory segment to be destroyed.
	   The segment will only actually be destroyed after the last process detaches it. */
	shmctl(shmid, IPC_RMID, NULL);

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
		shmdt(v->transport.shm.buffer);
		semctl(v->transport.shm.semid, 0, IPC_RMID, 0);
	}

	free(v);
	c->visv[vv] = NULL;
	return res;
}

/**
 * Decrements the client's semaphor (to read the next available chunk)
 */
void
decrement_client (xmmsc_vis_unixshm_t *t) {
	/* alter semaphore 1 by -1, no flags */
	struct sembuf op = { 1, -1, 0 };

	{
		int a = semctl(t->semid, 0, GETVAL, 0);
		int b = semctl(t->semid, 1, GETVAL, 0);
		//printf("DECR | %d, %d | pos %d\n", a, b, t->pos);
	}

	if (semop (t->semid, &op, 1) == -1) {
		/* TODO: restart after signals, etc. */
		perror("");
	}
}

/**
 * Increments the server's semaphor (after a chunk was read)
 */
void
increment_server (xmmsc_vis_unixshm_t *t) {
	/* alter semaphore 0 by 1, no flags */
	struct sembuf op = { 0, +1, 0 };

	{
		int a = semctl(t->semid, 0, GETVAL, 0);
		int b = semctl(t->semid, 1, GETVAL, 0);
		//printf("INCR | %d, %d | pos %d\n", a, b, t->pos);
	}

	if (semop (t->semid, &op, 1) == -1) {
		/* there should not occur any error */
		perror("");
	}
}

/**
 * Fetches the next available data chunk (blocking)
 */

int
xmmsc_visualisation_chunk_get (xmmsc_connection_t *c, int vv, void *data) {
	xmmsc_visualisation_t *v;
	xmmsc_vispacket_t *src;

	x_check_conn (c, 0);
	x_api_error_if (!(v = get_dataset(c, vv)), "with unregistered visualisation dataset", 0);

	if (v->type == VIS_UNIXSHM) {
		xmmsc_vis_unixshm_t *t = &v->transport.shm;
		decrement_client (t);
		src = &t->buffer[t->pos];
		//printf("timestamp: %d, format: %hd\n", src->timestamp, src->format);
		/* TODO: make this usable ;-) */
		memcpy(data, src->data, 2*sizeof(short));
		t->pos = (t->pos + 1) % t->size;
		increment_server (t);
		return 1;
	} else {
		return 0;
	}
}

/** @} */

