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

#ifndef __MISC_H__
#define __MISC_H__

#ifdef __cplusplus
{
#endif

#include <xmmsclient/xmmsclient.h>
#include <jni.h>

/*
 * swig should wrap that two functions to make them java-usable
 */
extern xmmsc_result_t* getResultFromPointer (jlong val);
extern jlong getPointerToConnection (xmmsc_connection_t *c);

#ifdef __cplusplus
}
#endif

#endif
