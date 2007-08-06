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

#include <sys/time.h>
#include <sys/types.h>

/* Note that features should only be added to the packet data, _not_
   removed. The packet's format should stay downwardly compatible.
   The client only tests if the server's version is equal or greater
   than the client's version! */
#define XMMS_VISPACKET_VERSION 1

/* How many packages are in the shm queue?
	* one package the server can operate on
	* one packate the client can operate on
	* to avoid needing to be in sync, one spare packet
    * XXX packets to compensate the latency (TODO: find a good value) */
#define XMMS_VISPACKET_SHMCOUNT 300

/**
 * Package format for vis data, encapsulated by unixshm or udp transport
 */

typedef struct {
	int32_t timestamp[2];
	uint16_t format;
	/* 512 is what libvisual wants for pcm data (could also be 256) */
	char data[512*sizeof(uint16_t)*2];
} xmmsc_vischunk_t;

/**
 * UDP package format to deliver a vis chunk
 */

typedef struct {
	char type; /* = 'V' */;
	uint16_t grace;
	xmmsc_vischunk_t data;
	/* DON'T put anything after data */
} xmmsc_vis_udp_data_t;

/**
 * UDP package format to synchronize time
 */

typedef struct {
	char type; /* = 'T' */
	int32_t id;
	int32_t clientstamp[2];
	int32_t serverstamp[2];
} xmmsc_vis_udp_timing_t;

/**
 * Possible data modes
 */

typedef enum {
	VIS_PCM,
	VIS_FFT
} xmmsc_vis_data_t;

/**
 * Properties of the delivered vis data. The client doesn't use this struct
 * to date, but perhaps could in future
 */

typedef struct {
	/* type of data */
	xmmsc_vis_data_t type;
	/* wether to send both channels seperate, or mixed */
	int stereo;
	/* TODO frequency of package delivery in hz */
	double freq;
	double timeframe;
	/* pcm amount of data wanted */
	int pcm_samplecount;
	/* pcm bitsize wanted */
/*	TODO xmms_sample_format_t pcm_sampleformat;*/
} xmmsc_vis_properties_t;

/**
 * Possible vis transports
 */

typedef enum {
	VIS_UNIXSHM,
	VIS_UDP,
	VIS_NONE
} xmmsc_vis_transport_t;

/**
 * data describing a unixshm transport
 */

typedef struct {
	int semid;
	int shmid;
	xmmsc_vischunk_t *buffer;
	int pos, size;
} xmmsc_vis_unixshm_t;

/**
 * data describing a udp transport
 */

typedef struct {
	// client address, used by server
	struct sockaddr_storage addr;
	// file descriptors, used by the client
	int socket[2];
	// watch adjustment, used by the client
	double timediff;
	// grace value (lifetime of the client without pong)
	int grace;
} xmmsc_vis_udp_t;

inline double
ts2tv (struct timeval *t)
{
	return t->tv_sec + t->tv_usec / 1000000.0;
}

inline double
net2tv (int32_t* s)
{
	return (int32_t)(ntohl (s[0])) + (int32_t)(ntohl (s[1])) / 1000000.0;
}

inline void
tv2net (int32_t* d, double t)
{
	double s, u;
	u = modf (t, &s);
	d[0] = htonl ((int32_t)s);
	d[1] = htonl ((int32_t)(u * 1000000.0));
}

inline void
ts2net (int32_t* d, struct timeval *t)
{
	d[0] = htonl ((int32_t)t->tv_sec);
	d[1] = htonl ((int32_t)t->tv_usec);
}

#ifdef __cplusplus
}
#endif

#endif

/** @} */
