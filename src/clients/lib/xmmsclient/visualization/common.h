#include "xmmsclient/xmmsclient.h"
#include "xmmsclientpriv/xmmsclient.h"
#include "xmmsc/xmmsc_visualization.h"
#include "xmmsc/xmmsc_stdbool.h"

#ifndef __VISUALIZATIONCLIENT_COMMON_H__
#define __VISUALIZATIONCLIENT_COMMON_H__

struct xmmsc_visualization_St {
	union {
		xmmsc_vis_unixshm_t shm;
		xmmsc_vis_udp_t udp;
	} transport;
	xmmsc_vis_transport_t type;
	/** server side identifier */
	int32_t id;
	/** client side array index */
	int idx;
};

xmmsc_visualization_t *get_dataset(xmmsc_connection_t *c, int vv);


/* provided by unixshm.c / dummy.c */
xmmsc_result_t *setup_shm_prepare (xmmsc_connection_t *c, int32_t vv);
bool setup_shm_handle (xmmsc_result_t *res);
void cleanup_shm (xmmsc_vis_unixshm_t *t);
int read_start_shm (xmmsc_vis_unixshm_t *t, unsigned int blocking, xmmsc_vischunk_t **dest);
void read_finish_shm (xmmsc_vis_unixshm_t *t, xmmsc_vischunk_t *dest);

/* provided by udp.c */
xmmsc_result_t *setup_udp_prepare (xmmsc_connection_t *c, int32_t vv);
bool setup_udp_handle (xmmsc_result_t *res);
void cleanup_udp (xmmsc_vis_udp_t *t);
int read_start_udp (xmmsc_vis_udp_t *t, unsigned int blocking, xmmsc_vischunk_t **dest, int32_t id);
void read_finish_udp (xmmsc_vis_udp_t *t, xmmsc_vischunk_t *dest);

#endif
