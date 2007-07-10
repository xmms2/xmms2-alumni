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

#define x_check_client(ret) \
	if (!c) { \
		xmms_error_set (err, XMMS_ERROR_INVAL, "invalid server-side identifier provided"); \
		return ret; \
	}


#include <math.h>
#include <glib.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

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
	unsigned short format;
	xmmsc_vis_properties_t prop;
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
XMMS_CMD_DEFINE (registercl, xmms_visualisation_register_client, xmms_visualisation_t *, INT32, NONE, NONE);
XMMS_CMD_DEFINE3 (init_shm, xmms_visualisation_init_shm, xmms_visualisation_t *, NONE, INT32, INT32, INT32);
XMMS_CMD_DEFINE3 (property_set, xmms_visualisation_property_set, xmms_visualisation_t *, INT32, INT32, STRING, STRING);
XMMS_CMD_DEFINE (properties_set, xmms_visualisation_properties_set, xmms_visualisation_t *, INT32, INT32, STRINGLIST);
XMMS_CMD_DEFINE (shutdown, xmms_visualisation_shutdown, xmms_visualisation_t *, NONE, INT32, NONE);

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
					 XMMS_IPC_CMD_VISUALISATION_REGISTER,
					 XMMS_CMD_FUNC (registercl));
	xmms_object_cmd_add (XMMS_OBJECT (vis),
					 XMMS_IPC_CMD_VISUALISATION_INIT_SHM,
					 XMMS_CMD_FUNC (init_shm));
	xmms_object_cmd_add (XMMS_OBJECT (vis),
					 XMMS_IPC_CMD_VISUALISATION_PROPERTY,
					 XMMS_CMD_FUNC (property_set));
	xmms_object_cmd_add (XMMS_OBJECT (vis),
					 XMMS_IPC_CMD_VISUALISATION_PROPERTIES,
					 XMMS_CMD_FUNC (properties_set));
	xmms_object_cmd_add (XMMS_OBJECT (vis),
					 XMMS_IPC_CMD_VISUALISATION_SHUTDOWN,
					 XMMS_CMD_FUNC (shutdown));

	//xmms_plugin_load (&xmms_builtin_visualisation, NULL);
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
get_client (int32_t id) {
	if (id < 0 || id >= vis->clientc) {
		return NULL;
	}

	return vis->clientv[id];
}

void
delete_client (int32_t id) {
	if (id < 0 || id >= vis->clientc) {
		return;
	}
	g_free (vis->clientv[id]);
	vis->clientv[id] = NULL;
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
		/* TODO: restart timer */
	} else {
		return FALSE;
	}
	return TRUE;
}

int32_t
xmms_visualisation_register_client (xmms_visualisation_t *vis, xmms_error_t *err) {
	int32_t id;
	xmms_vis_client_t *c;

	id = create_client();
	if (id < 0) {
		xmms_error_set (err, XMMS_ERROR_OOM, "could not allocate dataset");
		return -1;
	}

	/* do necessary initialisations here */
	c = get_client(id);
	c->format = 0;
	c->type = VIS_NONE;
	properties_init(&c->prop);

	return id;
}


int32_t
xmms_visualisation_property_set (xmms_visualisation_t *vis, int32_t id, gchar* key, gchar* value, xmms_error_t *err)
{
	xmms_vis_client_t *c = get_client(id);

	x_check_client (-1);

	if (!property_set (&c->prop, key, value)) {
		xmms_error_set (err, XMMS_ERROR_INVAL, "property could not be set!");
	}
	/* the format identifier (between client and server) changes. so the client can recognize the first packet
	   which is built using the new format according to the newly set property */
	return (++c->format);
}

int32_t
xmms_visualisation_properties_set (xmms_visualisation_t *vis, int32_t id, GList* prop, xmms_error_t *err)
{
	GList* n;
	xmms_vis_client_t *c = get_client(id);

	x_check_client (-1);

	for (n = prop; n; n = n->next) {
		if (!property_set (&c->prop, (gchar*)n->data, (gchar*)n->next->data)) {
			xmms_error_set (err, XMMS_ERROR_INVAL, "property could not be set!");
		}
		n = n->next;
	}
	return (++c->format);
}

void
xmms_visualisation_init_shm (xmms_visualisation_t *vis, int32_t id, int32_t shmid, int32_t semid, xmms_error_t *err)
{
	void *buffer;
	xmms_vis_client_t *c = get_client(id);

	x_check_client ();

	/* test the shm */
	buffer = shmat(shmid, NULL, SHM_RDONLY);
	if (buffer == (void*)-1) {
		xmms_error_set (err, XMMS_ERROR_NO_SAUSAGE, "couldn't attach to shared memory");
		return;
	}

	/* set up client structure */
	c->type = VIS_UNIXSHM;
	c->transport.shm.buffer = buffer;
	c->transport.shm.semid = semid;

	/* TODO: start delivery timer, etc. */
	printf("T: %d\t S: %d\t TF: %lf \n", c->prop.type, c->prop.stereo, c->prop.timeframe);
}

void
xmms_visualisation_shutdown (xmms_visualisation_t *vis, int32_t id, xmms_error_t *err)
{
	xmms_vis_client_t *c = get_client (id);

	x_check_client ();

	if (c->type == VIS_UNIXSHM) {
		shmdt (c->transport.shm.buffer);
	}

	delete_client (id);
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
