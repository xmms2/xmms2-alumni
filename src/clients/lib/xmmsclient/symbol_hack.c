/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2011 XMMS2 Team
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

/* This file is a hack around bug #2449.
 * See: http://bugs.xmms2.org/view.php?id=2449
 */

#include "xmmsclient/xmmsclient.h"

void hack (void);

void hack (void)
{
	xmmsv_t *v;
	const char *c;

	v = xmmsv_sc_argument_new ("name", "docstring", XMMSV_TYPE_INT32, NULL);
	c = xmmsv_sc_argument_get_docstring (v);

	xmmsv_unref (v);
	return;
}
