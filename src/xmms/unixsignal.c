/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2006 XMMS2 Team
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
 * Takes care of unix-signals.
 */


#include "xmmspriv/xmms_unixsignal.h"
#include "xmms/xmms_log.h"
#include "xmms/xmms_object.h"

#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <string.h>
#include <glib.h>
#include <unistd.h>


static gpointer
sigwaiter (gpointer data)
{
	xmms_object_t *obj = (xmms_object_t *) data;
	xmms_object_cmd_arg_t arg;
	sigset_t signals;
	int caught;

	memset (&signals, 0, sizeof (sigset_t));
	sigaddset (&signals, SIGINT);
	sigaddset (&signals, SIGTERM);

	while (1337) {
		sigwait (&signals, &caught);

		switch (caught){
			case SIGINT:
				xmms_log_info ("Got SIGINT!");

				xmms_object_cmd_arg_init (&arg);
				memset (&arg, 0, sizeof (arg));
				xmms_error_reset (&arg.error);
				xmms_object_cmd_call (obj, XMMS_IPC_CMD_QUIT, &arg);
				break;
			case SIGTERM:
				xmms_log_info ("Got SIGTERM! Bye!");
				exit (0);
				break;
		}
	}
}

void
xmms_signal_init (xmms_object_t *obj)
{
	g_thread_create (sigwaiter, obj, FALSE, NULL);
}
