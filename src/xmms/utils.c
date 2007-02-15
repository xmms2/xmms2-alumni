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
 * Miscellaneous internal utility functions specific to the daemon.
 */

#include <stdlib.h>
#include <glib.h>
#include <stdarg.h>

#include "xmmsc/xmmsc_util.h"
#include "xmmspriv/xmms_utils.h"
#include "xmmsc/xmmsc_strlist.h"

/**
 * Build path to file in xmms2 configuration directory.
 * @param first The first file or directory name in the path.
 * @param ... Additional file/directory names.
 * @return Absolute path to a file or directory.
 */
char *
xmms_build_path (char *first, ...)
{
	va_list ap;
	gchar confdir[PATH_MAX];
	gchar *ret, **vargv, **argv;

	g_return_val_if_fail (first, NULL);

	xmms_userconfdir_get (confdir, PATH_MAX);
	
	va_start (ap, first);
	vargv = xmms_valist_to_strlist (first, ap);
	va_end (ap);
	
	argv = xmms_strlist_prepend_copy (vargv, confdir);
	
	ret = g_build_pathv (G_DIR_SEPARATOR_S, argv);
	xmms_strlist_destroy (vargv);
	xmms_strlist_destroy (argv);
	return ret;
}
