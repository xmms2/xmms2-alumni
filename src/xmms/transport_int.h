#ifndef __XMMS_TRANSPORT_INT_H__
#define __XMMS_TRANSPORT_INT_H__

/*
 * Macros
 */

#include "playlist.h"

#define xmms_transport_lock(t) g_mutex_lock ((t)->mutex)
#define xmms_transport_unlock(t) g_mutex_unlock ((t)->mutex)

/*
 * Private function prototypes -- do NOT use in plugins.
 */

xmms_transport_t *xmms_transport_new (void);
gboolean xmms_transport_open (xmms_transport_t *transport, 
			      xmms_playlist_entry_t *entry);
const gchar *xmms_transport_mimetype_get (xmms_transport_t *transport);
const gchar *xmms_transport_mimetype_get_wait (xmms_transport_t *transport);
void xmms_transport_start (xmms_transport_t *transport);

/*
 * Private defines -- do NOT rely on these in plugins
 */
#define XMMS_TRANSPORT_RINGBUF_SIZE 32768

#endif
