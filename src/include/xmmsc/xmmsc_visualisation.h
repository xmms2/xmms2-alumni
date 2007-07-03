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

/** @defgroup Visualisation Common
  * @brief Common structs for the visualisation client and server
  * @{
  */

#ifndef __XMMS_VIS_COMMON_H__
#define __XMMS_VIS_COMMON_H__


#ifdef __cplusplus
extern "C" {
#endif

/* Note that features should only be added to the packet data, _not_
   removed. The packet's format should stay downwardly compatible.
   The client only tests if the server's version is equal or greater
   than the client's version! */
#define XMMS_VISPACKET_VERSION 1

/**
 * Package format for vis data, encapsulated by unixshm or udp transport
 */

typedef struct {
	int timestamp;
	unsigned short graceleft;
	unsigned short format;
	char randomdata[24];
} xmmsc_vispacket_t;

/**
 * Possible vis transports
 */

typedef enum {
	VIS_UNIXSHM,
	VIS_UDP
} xmmsc_vis_transport_t;

/**
 * data describing a unixshm transport
 */

typedef struct {
	int semid;
	xmmsc_vispacket_t *buffer;
} xmmsc_vis_unixshm_t;

/**
 * data describing a udp transport
 */

typedef struct {
	// socket, etc.
} xmmsc_vis_udp_t;


#ifdef __cplusplus
}
#endif

#endif

/** @} */
