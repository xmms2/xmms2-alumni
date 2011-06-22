/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2009 XMMS2 Team
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

#ifndef __CHANNELMASK_H__
#define __CHANNELMASK_H__

#include <glib.h>
#include "xmms/xmms_streamtype.h"

G_BEGIN_DECLS

typedef enum {
	XMMS_CHANNEL_MASK_MONO                  = 0x000000,
	XMMS_CHANNEL_MASK_FRONT_LEFT            = 0x000001,
	XMMS_CHANNEL_MASK_FRONT_RIGHT           = 0x000002,
	XMMS_CHANNEL_MASK_FRONT_CENTER          = 0x000004,
	XMMS_CHANNEL_MASK_LFE                   = 0x000008,
	XMMS_CHANNEL_MASK_BACK_LEFT             = 0x000010,
	XMMS_CHANNEL_MASK_BACK_RIGHT            = 0x000020,
	XMMS_CHANNEL_MASK_FRONT_LEFT_OF_CENTER  = 0x000040,
	XMMS_CHANNEL_MASK_FRONT_RIGHT_OF_CENTER = 0x000080,
	XMMS_CHANNEL_MASK_BACK_CENTER           = 0x000100,
	XMMS_CHANNEL_MASK_SIDE_LEFT             = 0x000200,
	XMMS_CHANNEL_MASK_SIDE_RIGHT            = 0x000400,
	XMMS_CHANNEL_MASK_TOP_CENTER            = 0x000800,
	XMMS_CHANNEL_MASK_TOP_FRONT_LEFT        = 0x001000,
	XMMS_CHANNEL_MASK_TOP_FRONT_CENTER      = 0x002000,
	XMMS_CHANNEL_MASK_TOP_FRONT_RIGHT       = 0x004000,
	XMMS_CHANNEL_MASK_TOP_BACK_LEFT         = 0x008000,
	XMMS_CHANNEL_MASK_TOP_BACK_CENTER       = 0x010000,
	XMMS_CHANNEL_MASK_TOP_BACK_RIGHT        = 0x020000,
	/* add new entries here, WAVE compatibility preferred */
	XMMS_CHANNEL_MASK_UNKNOWN               = 0x80000000,

	XMMS_CHANNEL_MASK_DEFAULT_1CH           = XMMS_CHANNEL_MASK_MONO,
	XMMS_CHANNEL_MASK_DEFAULT_2CH           = XMMS_CHANNEL_MASK_FRONT_LEFT|
	                                          XMMS_CHANNEL_MASK_FRONT_RIGHT,
	XMMS_CHANNEL_MASK_DEFAULT_3CH           = XMMS_CHANNEL_MASK_FRONT_LEFT|
	                                          XMMS_CHANNEL_MASK_FRONT_RIGHT|
	                                          XMMS_CHANNEL_MASK_FRONT_CENTER,
	XMMS_CHANNEL_MASK_DEFAULT_4CH           = XMMS_CHANNEL_MASK_FRONT_LEFT|
	                                          XMMS_CHANNEL_MASK_FRONT_RIGHT|
	                                          XMMS_CHANNEL_MASK_BACK_LEFT|
	                                          XMMS_CHANNEL_MASK_BACK_RIGHT,
	XMMS_CHANNEL_MASK_DEFAULT_5CH           = XMMS_CHANNEL_MASK_FRONT_LEFT|
	                                          XMMS_CHANNEL_MASK_FRONT_RIGHT|
	                                          XMMS_CHANNEL_MASK_FRONT_CENTER|
	                                          XMMS_CHANNEL_MASK_BACK_LEFT|
	                                          XMMS_CHANNEL_MASK_BACK_RIGHT,
	XMMS_CHANNEL_MASK_DEFAULT_6CH           = XMMS_CHANNEL_MASK_FRONT_LEFT|
	                                          XMMS_CHANNEL_MASK_FRONT_RIGHT|
	                                          XMMS_CHANNEL_MASK_FRONT_CENTER|
	                                          XMMS_CHANNEL_MASK_LFE|
	                                          XMMS_CHANNEL_MASK_BACK_LEFT|
	                                          XMMS_CHANNEL_MASK_BACK_RIGHT,
} xmms_channel_mask_t;


#define XMMS_CHANNEL_MASK_DEFAULT_MONO (XMMS_CHANNEL_MASK_UNDEFINED)
#define XMMS_CHANNEL_MASK_DEFAULT_STEREO (XMMS_CHANNEL_MASK_FRONT_LEFT|XMMS_CHANNEL_MASK_FRONT_RIGHT)
#define XMMS_CHANNEL_MASK_DEFAULT_3CH (XMMS_CHANNEL_MASK_FRONT_LEFT|XMMS_CHANNEL_MASK_FRONT_RIGHT|XMMS_CHANNEL_MASK_FRONT_CENTER)
#define XMMS_CHANNEL_MASK_DEFAULT_4CH (XMMS_CHANNEL_MASK_FRONT_LEFT|XMMS_CHANNEL_MASK_FRONT_RIGHT|XMMS_CHANNEL_MASK_BACK_LEFT|XMMS_CHANNEL_MASK_BACK_RIGHT)
#define XMMS_CHANNEL_MASK_DEFAULT


static inline xmms_channel_mask_t
xmms_channel_mask_get_default (gint channels)
{
	xmms_channel_mask_t mask;
	switch (channels) {
	case 1:
		mask = XMMS_CHANNEL_MASK_DEFAULT_1CH;
		break;
	case 2:
		mask = XMMS_CHANNEL_MASK_DEFAULT_2CH;
		break;
	case 3:
		mask = XMMS_CHANNEL_MASK_DEFAULT_3CH;
		break;
	case 4:
		mask = XMMS_CHANNEL_MASK_DEFAULT_4CH;
		break;
	case 5:
		mask = XMMS_CHANNEL_MASK_DEFAULT_5CH;
		break;
	case 6:
		mask = XMMS_CHANNEL_MASK_DEFAULT_6CH;
		break;
	default:
		mask = XMMS_CHANNEL_MASK_UNKNOWN;
		break;
	}
	return mask;
}

G_END_DECLS

#endif
