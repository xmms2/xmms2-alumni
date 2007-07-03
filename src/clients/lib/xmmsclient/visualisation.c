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

#include <sys/shm.h>
#include <sys/sem.h>
#include <sys/stat.h>

#include "xmmsclient/xmmsclient.h"
#include "xmmsclientpriv/xmmsclient.h"

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
	uint32_t id;
};

xmmsc_visualisation_t *
create_dataset(xmmsc_connection_t *c, int vv) {
	if (vv < 0 || vv >= c->visc)
		return NULL;

	if (!(c->visv[vv] = x_new0 (xmmsc_visualisation_t, 1))) {
        x_oom ();
    }
	return c->visv[vv];
}

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
	x_check_conn (c, 0);

	c->visc++;
	c->visv = realloc (c->visv, sizeof (xmmsc_visualisation_t*) * c->visc);
	if (!c->visv) {
		x_oom ();
		c->visc = 0;
	}
	if (c->visc > 0)
		c->visv[c->visc-1] = NULL;
	return c->visc - 1;
}

/**
 * Initializes a new visualisation connection
 */

xmmsc_result_t *
xmmsc_visualisation_connect (xmmsc_connection_t *c, int vv) {
	xmms_ipc_msg_t *msg;
	xmmsc_result_t *res;
	xmmsc_visualisation_t *v;

	int32_t shmid, semid;

	/* we can't transmit 64 bit int yet, but it could be that shmget() gives one */
	x_api_error_if (sizeof(int) != sizeof(int32_t), "on yet unsupported 64 bit machine", NULL);

	x_check_conn (c, NULL);

	/* prepare unixshm + semaphores */
	  /* following means everyone on the system could inject wrong vis data ;) */
	shmid = shmget (IPC_PRIVATE, sizeof (xmmsc_vispacket_t), S_IRWXU + S_IRWXG + S_IRWXO);
	semid = semget (IPC_PRIVATE, 2, S_IRWXU + S_IRWXG + S_IRWXO);
	if (shmid == -1 || semid == -1) {
		c->error = strdup("Couldn't create the shared memory and semaphore set!");
		return NULL;
	}

	/* send packet */
	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALISATION, XMMS_IPC_CMD_VISUALISATION_INIT_SHM);
	xmms_ipc_msg_put_int32 (msg, shmid);
	xmms_ipc_msg_put_int32 (msg, semid);
	res = xmmsc_send_msg (c, msg);

	/* find out if it worked */
	xmmsc_result_wait (res);
	if (!xmmsc_result_iserror (res)) {
		x_api_error_if (!(v = create_dataset(c, vv)), "with unregistered visualisation dataset", NULL);

		xmmsc_result_get_uint (res, &v->id);
		v->type = VIS_UNIXSHM;
		v->transport.shm.buffer = shmat(shmid, NULL, SHM_RDONLY);
		v->transport.shm.semid = semid;
	} else {
		/* try udp here */
	}

	/* In either case, mark the shared memory segment to be destroyed.
	   The segment will only actually be destroyed after the last process detaches it. */
	shmctl(shmid, IPC_RMID, NULL);

	return res;
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
	x_api_error_if (!(v = get_dataset(c, vv)), "with unregistered/unconnected visualisation dataset", NULL);

	msg = xmms_ipc_msg_new (XMMS_IPC_OBJECT_VISUALISATION, XMMS_IPC_CMD_VISUALISATION_SHUTDOWN);
	xmms_ipc_msg_put_uint32 (msg, v->id);
	res = xmmsc_send_msg (c, msg);

	/* detach from shm, close socket.. */
	if (v->type == VIS_UNIXSHM) {
		shmdt(v->transport.shm.buffer);
		/* this is for x-platform compat. but it sucks at it: union semun su; */
		semctl(v->transport.shm.semid, 0, IPC_RMID/*, su*/);
	}

	free(v);
	c->visv[vv] = NULL;
	return res;
}

/** @} */

