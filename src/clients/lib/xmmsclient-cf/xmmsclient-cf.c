/** @file xmmsclient-cf.c
 *  Mac OS X CoreFoundation run loop integration
 *
 *  Copyright (C) 2005-2009 XMMS2 Team
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

#include <CoreFoundation/CoreFoundation.h>
#include "xmmsclient/xmmsclient.h"

void
xmmsc_io_cf_toggle_socket_flags (int toggle, void *userdata)
{
	CFSocketRef sockRef = (CFSocketRef) userdata;

	if (toggle)
		CFSocketEnableCallBacks (sockRef, kCFSocketWriteCallBack);
	else
		CFSocketDisableCallBacks (sockRef, kCFSocketWriteCallBack);
}

static void
xmmsc_io_cf_event_callback (CFSocketRef s,
                            CFSocketCallBackType type,
                            CFDataRef address,
                            const void *data,
                            void *info)
{
	CFSocketContext context;
	context.version = 0;
	CFSocketGetContext (s, &context);

	if (type == kCFSocketCloseOnInvalidate) {
		xmmsc_io_disconnect (context.info);
	}
	else if (type == kCFSocketWriteCallBack) {
		xmmsc_io_out_handle (context.info);
	}
	else if (type == kCFSocketReadCallBack) {
		xmmsc_io_in_handle (context.info);
	}
}


unsigned int
xmmsc_setup_with_cf (xmmsc_connection_t *c)
{
	CFRunLoopRef runloop = CFRunLoopGetCurrent();
	CFRunLoopSourceRef runLoopSourceRef;
	CFSocketContext context;
	CFSocketRef sockRef;
	CFOptionFlags flags;

	context.version = 0;
	context.info = c;
	context.retain = NULL;
	context.release = NULL;
	context.copyDescription = NULL;

	flags = kCFSocketReadCallBack | kCFSocketWriteCallBack;

	sockRef = CFSocketCreateWithNative (kCFAllocatorDefault,
	                                    xmmsc_io_fd_get (c),
	                                    flags,
	                                    &xmmsc_io_cf_event_callback,
	                                    &context);

	if (!sockRef)
		return 0;


	runLoopSourceRef = CFSocketCreateRunLoopSource (kCFAllocatorDefault,
	                                                sockRef, 4);

	CFRunLoopAddSource (runloop, runLoopSourceRef, kCFRunLoopDefaultMode);


	xmmsc_io_need_out_callback_set (c, xmmsc_io_cf_toggle_socket_flags, sockRef);
	
	return 1;
}
