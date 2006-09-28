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

#include <xmms/xmms_defs.h>
#include <xmmsclient/xmmsclient.h>

#include <ruby.h>

void Init_Client ();

static VALUE
m_userconfdir_get (VALUE self)
{
	const char *p;
	char path[PATH_MAX];

	p = xmmsc_userconfdir_get (path, PATH_MAX);

	return p ? rb_str_new2 (p) : Qnil;
}

void
Init_xmmsclient (void)
{
	VALUE mXmms = rb_define_module ("Xmms");

	rb_define_module_function (mXmms, "userconfdir", m_userconfdir_get, 0);

	rb_define_const (mXmms, "VERSION", rb_str_new2 (XMMS_VERSION));

	Init_Client (mXmms); /* initializes Result, too */
}
