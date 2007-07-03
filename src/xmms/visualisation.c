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

#include <math.h>
#include <glib.h>
#include <string.h>
#include <stdio.h>

#include <sys/shm.h>
#include <sys/sem.h>

#include "xmmspriv/xmms_visualisation.h"
#include "xmmspriv/xmms_ipc.h"
#include "xmmspriv/xmms_sample.h"
#include "xmms/xmms_object.h"

#include "xmmsc/xmmsc_visualisation.h"

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
} xmms_vis_client_t;

/**
 * The structures for the vis module
 */

struct xmms_visualisation_St {
	xmms_object_t object;

	int32_t clientc;
	xmms_vis_client_t **clientv;
};

static xmms_visualisation_t *vis;

static void xmms_visualisation_destroy (xmms_object_t *object);

XMMS_CMD_DEFINE (query_version, xmms_visualisation_version, xmms_visualisation_t *, UINT32, NONE, NONE);
XMMS_CMD_DEFINE (init_shm, xmms_visualisation_init_shm, xmms_visualisation_t *, UINT32, INT32, INT32);
XMMS_CMD_DEFINE (shutdown, xmms_visualisation_shutdown, xmms_visualisation_t *, NONE, UINT32, NONE);

/**
 * Initialize the Vis module.
 */
void
xmms_visualisation_init ()
{
	vis = xmms_object_new (xmms_visualisation_t, xmms_visualisation_destroy);
	vis->clientc = 0;

	xmms_ipc_object_register (XMMS_IPC_OBJECT_VISUALISATION, XMMS_OBJECT (vis));
	xmms_object_cmd_add (XMMS_OBJECT (vis),
					 XMMS_IPC_CMD_VISUALISATION_QUERY_VERSION,
					 XMMS_CMD_FUNC (query_version));
	xmms_object_cmd_add (XMMS_OBJECT (vis),
					 XMMS_IPC_CMD_VISUALISATION_INIT_SHM,
					 XMMS_CMD_FUNC (init_shm));
	xmms_object_cmd_add (XMMS_OBJECT (vis),
					 XMMS_IPC_CMD_VISUALISATION_SHUTDOWN,
					 XMMS_CMD_FUNC (shutdown));
}

/**
 * Free all resoures used by visualisation module.
 */

static void
xmms_visualisation_destroy (xmms_object_t *object)
{
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
		return -1;
	}

	return id;
}

xmms_vis_client_t *
get_client(int32_t id) {
	if (id < 0 || id >= vis->clientc) {
		return NULL;
	}

	return vis->clientv[id];
}

uint32_t
xmms_visualisation_version (xmms_visualisation_t *vis, xmms_error_t *err) {
	/* if there is a way to disable visualisation support on the server side,
	   we could return 0 here, or we could return an error? */

	return XMMS_VISPACKET_VERSION;
}

int32_t
xmms_visualisation_init_shm (xmms_visualisation_t *vis, int32_t shmid, int32_t semid, xmms_error_t *err)
{
	int32_t clientid;
	xmms_vis_client_t *c;
	void *buffer;

	/* test the shm */
	buffer = shmat(shmid, NULL, SHM_RDONLY);
	if (buffer == (void*)-1) {
		xmms_error_set (err, XMMS_ERROR_NO_SAUSAGE, "couldn't attach to shared memory");
	}

	/* set up client structure */
	clientid = create_client();
	if (clientid < 0) {
		xmms_error_set (err, XMMS_ERROR_OOM, "could not allocate dataset");
	}
	c = get_client(clientid);
	c->type = VIS_UNIXSHM;
	c->transport.shm.buffer = buffer;
	c->transport.shm.semid = semid;

	return clientid;
}

void
xmms_visualisation_shutdown (xmms_visualisation_t *vis, int32_t id, xmms_error_t *err)
{
	xmms_vis_client_t *c = get_client(id);
	if (!c) {
		xmms_error_set (err, XMMS_ERROR_INVAL, "invalid server-side identifier provided");
	}

	if (c->type == VIS_UNIXSHM) {
		shmdt (c->transport.shm.buffer);
	}

	g_free (c);
}

//~ static void output_spectrum (xmms_visualisation_t *vis, guint32 pos)
//~ {
	//~ xmms_object_cmd_value_t *data;
	//~ GList *node = vis->list;
	//~ int i;

	//~ data = (xmms_object_cmd_value_t *)node->data;
	//~ data->value.uint32 = xmms_sample_samples_to_ms (vis->format, pos);

	//~ node = g_list_next (node);
	//~ for (i = 0; i < FFT_LEN / 2; i++) {
		//~ gfloat tmp = vis->spec[i];
		//~ data = (xmms_object_cmd_value_t *)node->data;

		//~ if (tmp >= 1.0)
			//~ data->value.uint32 = INT_MAX;
		//~ else if (tmp < 0.0)
			//~ data->value.uint32 = 0;
		//~ else
			//~ data->value.uint32 = (guint)(tmp * INT_MAX);

		//~ node = g_list_next (node);
	//~ }

	//~ xmms_object_emit_f (XMMS_OBJECT (vis),
	                    //~ XMMS_IPC_SIGNAL_VISUALISATION_DATA,
	                    //~ XMMS_OBJECT_CMD_ARG_LIST,
	                    //~ vis->list);

//~ }

//~ /**
 //~ * Calcualte the FFT on the decoded data buffer.
 //~ */
//~ void
//~ xmms_visualisation_calc (xmms_visualisation_t *vis, xmms_sample_t *buf, int len, guint32 pos)
//~ {
	//~ gint t;

	//~ g_return_if_fail (vis);

	//~ if (vis->format->format != XMMS_SAMPLE_FORMAT_S16)
		//~ return;

	//~ if (xmms_ipc_has_pending (XMMS_IPC_SIGNAL_VISUALISATION_DATA)) {
		//~ vis->needed = 20;
	//~ } else if (vis->needed) {
		//~ vis->needed--;
	//~ }

	//~ if (!vis->needed)
		//~ return;

	//~ if (vis->fft_data) {
		//~ pos -= vis->fft_data / 4;

		//~ t = MIN (len, (FFT_LEN*4)-vis->fft_data);
		//~ memcpy (vis->fft_buf + vis->fft_data, buf, t);
		//~ vis->fft_data += t;
		//~ len -= t;
		//~ buf += t;
		//~ if (vis->fft_data == FFT_LEN*4) {
			//~ fft ((gint16 *)vis->fft_buf, vis->spec);
			//~ output_spectrum (vis, pos);
			//~ vis->fft_data = 0;
		//~ }
	//~ }

	//~ while (len > FFT_LEN*4) {
		//~ fft ((gint16 *)buf, vis->spec);
		//~ output_spectrum (vis, pos);

		//~ buf += FFT_LEN*4;
		//~ len -= FFT_LEN*4;
		//~ pos += FFT_LEN;
	//~ }

	//~ if (len) {
		//~ g_return_if_fail (!vis->fft_data);
		//~ memcpy (vis->fft_buf, buf, len);
		//~ vis->fft_data = len;
	//~ }
//~ }

//~ /**
 //~ * Tell the visualisation what audio format we use
 //~ */
//~ void
//~ xmms_visualisation_format_set (xmms_visualisation_t *vis, xmms_audio_format_t *fmt)
//~ {
	//~ vis->format = fmt;
//~ }

//~ static void
//~ fft (gint16 *samples, gfloat *spec)
//~ {
	//~ gint nv2, k, l, j = 0, i;
	//~ gfloat t_r, t_i;
	//~ gfloat buf[FFT_LEN][2];

	//~ for (i = 0; i < FFT_LEN; i++){
		//~ buf[i][0]  = (float) samples[j++];
		//~ buf[i][0] += (float) samples[j++];
		//~ buf[i][0] /= (float) (1 << 17);
		//~ buf[i][0] *= window[i];
		//~ buf[i][1] = 0.0f;
	//~ }

	//~ /* reorder... */  /* this is crappy! Go rewrite it using real bitreversing */
	//~ nv2 = FFT_LEN / 2;
	//~ j = 1;

	//~ for (i = 1; i < FFT_LEN; i++) {
		//~ if (i < j) {
			//~ t_r = buf[i - 1][0];
			//~ t_i = buf[i - 1][1];
			//~ buf[i - 1][0] = buf[j - 1][0];
			//~ buf[i - 1][1] = buf[j - 1][1];
			//~ buf[j - 1][0] = t_r;
			//~ buf[j - 1][1] = t_i;
		//~ }

		//~ k = nv2;

		//~ while (k < j) {
			//~ j -= k;
			//~ k /= 2;
		//~ }

		//~ j += k;
	//~ }

	//~ /* do fft */
	//~ for (l = 1; l <= FFT_BITS; l++) {
		//~ gint le = 1 << l;
		//~ gint le1 = le / 2;
		//~ gfloat u_r = 1.0;
		//~ gfloat u_i = 0.0;
		//~ gfloat w_r =  cos (M_PI / (float) le1);
		//~ gfloat w_i = -sin (M_PI / (float) le1);

		//~ for (j = 1; j <= le1; j++) {
			//~ for (i = j; i <= FFT_LEN; i += le) {
				//~ gint ip = i + le1;

				//~ t_r = buf[ip - 1][0] * u_r - u_i * buf[ip - 1][1];
				//~ t_i = buf[ip - 1][1] * u_r + u_i * buf[ip - 1][0];

				//~ buf[ip - 1][0] = buf[i - 1][0] - t_r;
				//~ buf[ip - 1][1] = buf[i - 1][1] - t_i;

				//~ buf[i - 1][0] =  buf[i - 1][0] + t_r;
				//~ buf[i - 1][1] =  buf[i - 1][1] + t_i;
			//~ }

			//~ t_r = u_r * w_r - w_i * u_i;
			//~ u_i = w_r * u_i + w_i * u_r;
			//~ u_r = t_r;
		//~ }
	//~ }

	//~ /* output abs-value instead */
	//~ for (i = 0; i < nv2; i++) {
		//~ spec[i] = hypot (buf[i][0], buf[i][1]);
	//~ }

	//~ /* correct the scale */
	//~ spec[0] /= 2;
	//~ spec[nv2 - 1] /= 2;

//~ }

/** @} */
