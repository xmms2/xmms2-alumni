/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2008 XMMS2 Team
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

#include <glib.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <errno.h>

#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_ipc_transport.h"
#include "xmmsc/xmmsc_ipc_msg.h"

#include "xmms/xmms_log.h"

#include "xmmspriv/xmms_ringbuf.h"
#include "xmmspriv/xmms_ipc.h"
#include "xmmspriv/xmms_playlist.h"
#include "xmmspriv/xmms_config.h"
#include "xmmspriv/xmms_bindata.h"
#include "xmmspriv/xmms_utils.h"

struct xmms_bindata_St {
	xmms_object_t obj;
	const gchar *bindir;
};

static xmms_bindata_t *global_bindata;

static void xmms_bindata_destroy (xmms_object_t *obj);

typedef unsigned char md5_byte_t; /* 8-bit byte */
typedef unsigned int md5_word_t; /* 32-bit word */

/* Define the state of the MD5 Algorithm. */
typedef struct md5_state_s {
    md5_word_t count[2]; /* message length in bits, lsw first */
    md5_word_t abcd[4];  /* digest buffer */
    md5_byte_t buf[64];  /* accumulate block */
} md5_state_t;

/* Initialize the algorithm. */
static void md5_init (md5_state_t *pms);
static void md5_append (md5_state_t *pms, const md5_byte_t *data, int nbytes);
static void md5_finish (md5_state_t *pms, md5_byte_t digest[16]);

static gchar *xmms_bindata_add (xmms_bindata_t *bindata, GString *data, xmms_error_t *err);
static GString *xmms_bindata_retrieve (xmms_bindata_t *bindata, gchar *hash, xmms_error_t *err);
static void xmms_bindata_remove (xmms_bindata_t *bindata, gchar *hash, xmms_error_t *);
static GList *xmms_bindata_list (xmms_bindata_t *bindata, xmms_error_t *err);
static gboolean _xmms_bindata_add (xmms_bindata_t *bindata, const guchar *data, gsize len, gchar hash[33], xmms_error_t *err);

XMMS_CMD_DEFINE (get_data, xmms_bindata_retrieve, xmms_bindata_t *, BIN, STRING, NONE);
XMMS_CMD_DEFINE (add_data, xmms_bindata_add, xmms_bindata_t *, STRING, BIN, NONE);
XMMS_CMD_DEFINE (remove_data, xmms_bindata_remove, xmms_bindata_t *, NONE, STRING, NONE);
XMMS_CMD_DEFINE (list_data, xmms_bindata_list, xmms_bindata_t *, LIST, NONE, NONE);

xmms_bindata_t *
xmms_bindata_init ()
{
	gchar *tmp;
	xmms_bindata_t *obj;
	xmms_config_property_t *cv;

	obj = xmms_object_new (xmms_bindata_t, xmms_bindata_destroy);

	xmms_object_cmd_add (XMMS_OBJECT (obj),
	                     XMMS_IPC_CMD_ADD_DATA,
	                     XMMS_CMD_FUNC (add_data));

	xmms_object_cmd_add (XMMS_OBJECT (obj),
	                     XMMS_IPC_CMD_REMOVE_DATA,
	                     XMMS_CMD_FUNC (remove_data));

	xmms_object_cmd_add (XMMS_OBJECT (obj),
	                     XMMS_IPC_CMD_GET_DATA,
	                     XMMS_CMD_FUNC (get_data));

	xmms_object_cmd_add (XMMS_OBJECT (obj),
	                     XMMS_IPC_CMD_LIST_DATA,
	                     XMMS_CMD_FUNC (list_data));

	xmms_ipc_object_register (XMMS_IPC_OBJECT_BINDATA, XMMS_OBJECT (obj));

	tmp = XMMS_BUILD_PATH ("bindata");
	cv = xmms_config_property_register ("bindata.path", tmp, NULL, NULL);
	g_free (tmp);

	obj->bindir = xmms_config_property_get_string (cv);

	if (!g_file_test (obj->bindir, G_FILE_TEST_IS_DIR)) {
		if (g_mkdir_with_parents (obj->bindir, 0755) == -1) {
			xmms_log_error ("Couldn't create bindir %s", obj->bindir);
		}
	}

	global_bindata = obj;

	return obj;
}

static void
xmms_bindata_destroy (xmms_object_t *obj)
{
	xmms_ipc_object_unregister (XMMS_IPC_OBJECT_BINDATA);
}

gchar *
xmms_bindata_calculate_md5 (const guchar *data, guint size, gchar ret[33])
{
	md5_state_t state;
	md5_byte_t digest[16];
	int di;
	static gchar hex[] = {
		'0', '1', '2', '3', '4', '5', '6', '7',
		'8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
	};

	md5_init (&state);
	md5_append (&state, (const md5_byte_t *)data, size);
	md5_finish (&state, digest);

	for (di = 0; di < 16; ++di) {
		ret[di * 2] = hex[digest[di] >> 4];
		ret[di * 2 + 1] = hex[digest[di] & 0x0f];
	}
	ret[32] = 0;
	return ret;
}

/** Add binary data from a plugin */
gboolean
xmms_bindata_plugin_add (const guchar *data, gsize size, gchar hash[33])
{
	xmms_error_t err;
	return _xmms_bindata_add (global_bindata, data, size, hash, &err);
}

static gboolean
_xmms_bindata_add (xmms_bindata_t *bindata, const guchar *data, gsize len, gchar hash[33], xmms_error_t *err)
{
	const guchar *ptr;
	gsize left;
	gchar *path;
	FILE *fp;

	xmms_bindata_calculate_md5 (data, len, hash);

	path = XMMS_BUILD_PATH ("bindata", hash);

	if (g_file_test (path, G_FILE_TEST_IS_REGULAR)) {
		XMMS_DBG ("file %s is already in bindata dir", hash);
		g_free (path);
		return TRUE;
	}

	XMMS_DBG ("Creating %s", path);
	fp = fopen (path, "wb");
	if (!fp) {
		xmms_log_error ("Couldn't create %s", path);
		xmms_error_set (err, XMMS_ERROR_GENERIC, "Couldn't create file on server!");
		g_free (path);
		return FALSE;
	}

	/* write the data to the file */
	ptr = data;
	left = len;

	while (left > 0) {
		size_t w;

		w = fwrite (ptr, 1, left, fp);
		if (!w && ferror (fp)) {
			fclose (fp);
			unlink (path);

			xmms_log_error ("Couldn't write data");
			xmms_error_set (err, XMMS_ERROR_GENERIC,
			                "Couldn't write data!");
			g_free (path);
			return FALSE;
		}

		left -= w;
		ptr += w;
	}

	fclose (fp);
	g_free (path);

	return TRUE;
}

char *
xmms_bindata_add (xmms_bindata_t *bindata, GString *data, xmms_error_t *err)
{
	gchar hash[33];
	if (_xmms_bindata_add (bindata, (guchar *)data->str, data->len, hash, err))
		return g_strdup (hash);
	return NULL;
}

static GString *
xmms_bindata_retrieve (xmms_bindata_t *bindata, gchar *hash, xmms_error_t *err)
{
	gchar *path;
	GString *str;
	FILE *fp;

	path = XMMS_BUILD_PATH ("bindata", hash);

	fp = fopen (path, "rb");
	if (!fp) {
		xmms_log_error ("Requesting '%s' which is not on the server", hash);
		xmms_error_set (err, XMMS_ERROR_NOENT, "File not found!");
		g_free (path);
		return NULL;
	}

	g_free (path);

	str = g_string_new (NULL);
	while (!feof (fp)) {
		gchar buf[1024];
		gint l;

		l = fread (buf, 1, 1024, fp);
		if (ferror (fp)) {
			g_string_free (str, TRUE);
			xmms_log_error ("Error reading bindata '%s'", hash);
			xmms_error_set (err, XMMS_ERROR_GENERIC, "Error reading file");
			return NULL;
		}
		g_string_append_len (str, buf, l);
	}

	fclose (fp);

	return str;
}

static void
xmms_bindata_remove (xmms_bindata_t *bindata, gchar *hash, xmms_error_t *err)
{
	gchar *path;
	path = XMMS_BUILD_PATH ("bindata", hash);
	if (unlink (path) == -1) {
		xmms_error_set (err, XMMS_ERROR_GENERIC, "Couldn't remove file");
	}
	g_free (path);
	return;
}

static GList *
xmms_bindata_list (xmms_bindata_t *bindata, xmms_error_t *err)
{
	GList *entries = NULL;
	gchar *path;
	const gchar *file;
	GDir *dir;

	path = XMMS_BUILD_PATH ("bindata");
	dir = g_dir_open (path, 0, NULL);
	g_free (path);

	if (!dir) {
		xmms_error_set (err, XMMS_ERROR_GENERIC,
		                "Couldn't open bindata directory");
		return NULL;
	}

	while ((file = g_dir_read_name (dir))) {
		entries = g_list_prepend (entries, xmmsv_new_string (file));
	}

	g_dir_close (dir);

	return entries;
}

/*
  Copyright (C) 1999, 2000, 2002 Aladdin Enterprises.  All rights reserved.

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  L. Peter Deutsch
  ghost@aladdin.com

 */
/* $Id: md5.c,v 1.6 2002/04/13 19:20:28 lpd Exp $ */
/*
  Independent implementation of MD5 (RFC 1321).

  This code implements the MD5 Algorithm defined in RFC 1321, whose
  text is available at
	http://www.ietf.org/rfc/rfc1321.txt
  The code is derived from the text of the RFC, including the test suite
  (section A.5) but excluding the rest of Appendix A.  It does not include
  any code or documentation that is identified in the RFC as being
  copyrighted.

  The original and principal author of md5.c is L. Peter Deutsch
  <ghost@aladdin.com>.  Other authors are noted in the change history
  that follows (in reverse chronological order):

  2002-04-13 lpd Clarified derivation from RFC 1321; now handles byte order
	either statically or dynamically; added missing #include <string.h>
	in library.
  2002-03-11 lpd Corrected argument list for main(), and added int return
	type, in test program and T value program.
  2002-02-21 lpd Added missing #include <stdio.h> in test program.
  2000-07-03 lpd Patched to eliminate warnings about "constant is
	unsigned in ANSI C, signed in traditional"; made test program
	self-checking.
  1999-11-04 lpd Edited comments slightly for automatic TOC extraction.
  1999-10-18 lpd Fixed typo in header comment (ansi2knr rather than md5).
  1999-05-03 lpd Original version.
 */

/*
 * This package supports both compile-time and run-time determination of CPU
 * byte order.  If ARCH_IS_BIG_ENDIAN is defined as 0, the code will be
 * compiled to run only on little-endian CPUs; if ARCH_IS_BIG_ENDIAN is
 * defined as non-zero, the code will be compiled to run only on big-endian
 * CPUs; if ARCH_IS_BIG_ENDIAN is not defined, the code will be compiled to
 * run on either big- or little-endian CPUs, but will run slightly less
 * efficiently on either one than if ARCH_IS_BIG_ENDIAN is defined.
 */

#undef BYTE_ORDER /* 1 = big-endian, -1 = little-endian, 0 = unknown */
#ifdef ARCH_IS_BIG_ENDIAN
#  define BYTE_ORDER (ARCH_IS_BIG_ENDIAN ? 1 : -1)
#else
#  define BYTE_ORDER 0
#endif

#define T_MASK ((md5_word_t)~0)
#define T1 /* 0xd76aa478 */ (T_MASK ^ 0x28955b87)
#define T2 /* 0xe8c7b756 */ (T_MASK ^ 0x173848a9)
#define T3    0x242070db
#define T4 /* 0xc1bdceee */ (T_MASK ^ 0x3e423111)
#define T5 /* 0xf57c0faf */ (T_MASK ^ 0x0a83f050)
#define T6    0x4787c62a
#define T7 /* 0xa8304613 */ (T_MASK ^ 0x57cfb9ec)
#define T8 /* 0xfd469501 */ (T_MASK ^ 0x02b96afe)
#define T9    0x698098d8
#define T10 /* 0x8b44f7af */ (T_MASK ^ 0x74bb0850)
#define T11 /* 0xffff5bb1 */ (T_MASK ^ 0x0000a44e)
#define T12 /* 0x895cd7be */ (T_MASK ^ 0x76a32841)
#define T13    0x6b901122
#define T14 /* 0xfd987193 */ (T_MASK ^ 0x02678e6c)
#define T15 /* 0xa679438e */ (T_MASK ^ 0x5986bc71)
#define T16    0x49b40821
#define T17 /* 0xf61e2562 */ (T_MASK ^ 0x09e1da9d)
#define T18 /* 0xc040b340 */ (T_MASK ^ 0x3fbf4cbf)
#define T19    0x265e5a51
#define T20 /* 0xe9b6c7aa */ (T_MASK ^ 0x16493855)
#define T21 /* 0xd62f105d */ (T_MASK ^ 0x29d0efa2)
#define T22    0x02441453
#define T23 /* 0xd8a1e681 */ (T_MASK ^ 0x275e197e)
#define T24 /* 0xe7d3fbc8 */ (T_MASK ^ 0x182c0437)
#define T25    0x21e1cde6
#define T26 /* 0xc33707d6 */ (T_MASK ^ 0x3cc8f829)
#define T27 /* 0xf4d50d87 */ (T_MASK ^ 0x0b2af278)
#define T28    0x455a14ed
#define T29 /* 0xa9e3e905 */ (T_MASK ^ 0x561c16fa)
#define T30 /* 0xfcefa3f8 */ (T_MASK ^ 0x03105c07)
#define T31    0x676f02d9
#define T32 /* 0x8d2a4c8a */ (T_MASK ^ 0x72d5b375)
#define T33 /* 0xfffa3942 */ (T_MASK ^ 0x0005c6bd)
#define T34 /* 0x8771f681 */ (T_MASK ^ 0x788e097e)
#define T35    0x6d9d6122
#define T36 /* 0xfde5380c */ (T_MASK ^ 0x021ac7f3)
#define T37 /* 0xa4beea44 */ (T_MASK ^ 0x5b4115bb)
#define T38    0x4bdecfa9
#define T39 /* 0xf6bb4b60 */ (T_MASK ^ 0x0944b49f)
#define T40 /* 0xbebfbc70 */ (T_MASK ^ 0x4140438f)
#define T41    0x289b7ec6
#define T42 /* 0xeaa127fa */ (T_MASK ^ 0x155ed805)
#define T43 /* 0xd4ef3085 */ (T_MASK ^ 0x2b10cf7a)
#define T44    0x04881d05
#define T45 /* 0xd9d4d039 */ (T_MASK ^ 0x262b2fc6)
#define T46 /* 0xe6db99e5 */ (T_MASK ^ 0x1924661a)
#define T47    0x1fa27cf8
#define T48 /* 0xc4ac5665 */ (T_MASK ^ 0x3b53a99a)
#define T49 /* 0xf4292244 */ (T_MASK ^ 0x0bd6ddbb)
#define T50    0x432aff97
#define T51 /* 0xab9423a7 */ (T_MASK ^ 0x546bdc58)
#define T52 /* 0xfc93a039 */ (T_MASK ^ 0x036c5fc6)
#define T53    0x655b59c3
#define T54 /* 0x8f0ccc92 */ (T_MASK ^ 0x70f3336d)
#define T55 /* 0xffeff47d */ (T_MASK ^ 0x00100b82)
#define T56 /* 0x85845dd1 */ (T_MASK ^ 0x7a7ba22e)
#define T57    0x6fa87e4f
#define T58 /* 0xfe2ce6e0 */ (T_MASK ^ 0x01d3191f)
#define T59 /* 0xa3014314 */ (T_MASK ^ 0x5cfebceb)
#define T60    0x4e0811a1
#define T61 /* 0xf7537e82 */ (T_MASK ^ 0x08ac817d)
#define T62 /* 0xbd3af235 */ (T_MASK ^ 0x42c50dca)
#define T63    0x2ad7d2bb
#define T64 /* 0xeb86d391 */ (T_MASK ^ 0x14792c6e)


static void
md5_process (md5_state_t *pms, const md5_byte_t *data /*[64]*/)
{
	md5_word_t
	a = pms->abcd[0], b = pms->abcd[1],
	c = pms->abcd[2], d = pms->abcd[3];
	md5_word_t t;
#if BYTE_ORDER > 0
	/* Define storage only for big-endian CPUs. */
	md5_word_t X[16];
#else
	/* Define storage for little-endian or both types of CPUs. */
	md5_word_t xbuf[16];
	const md5_word_t *X;
#endif

	{
#if BYTE_ORDER == 0
	/*
	 * Determine dynamically whether this is a big-endian or
	 * little-endian machine, since we can use a more efficient
	 * algorithm on the latter.
	 */
	static const int w = 1;

	if (*((const md5_byte_t *)&w)) /* dynamic little-endian */
#endif
#if BYTE_ORDER <= 0 /* little-endian */
	{
		/*
		 * On little-endian machines, we can process properly aligned
		 * data without copying it.
		 */
		if (!((data - (const md5_byte_t *)0) & 3)) {
			/* data are properly aligned */
			X = (const md5_word_t *)data;
		} else {
			/* not aligned */
			memcpy (xbuf, data, 64);
			X = xbuf;
		}
	}
#endif
#if BYTE_ORDER == 0
	else /* dynamic big-endian */
#endif
#if BYTE_ORDER >= 0 /* big-endian */
	{
		/*
		 * On big-endian machines, we must arrange the bytes in the
		 * right order.
		 */
		const md5_byte_t *xp = data;
		int i;

#  if BYTE_ORDER == 0
		X = xbuf;/* (dynamic only) */
#  else
#    define xbuf X /* (static only) */
#  endif
		for (i = 0; i < 16; ++i, xp += 4)
		xbuf[i] = xp[0] + (xp[1] << 8) + (xp[2] << 16) + (xp[3] << 24);
	}
#endif
	}

#define ROTATE_LEFT(x, n) (((x) << (n)) | ((x) >> (32 - (n))))

	/* Round 1. */
	/* Let [abcd k s i] denote the operation
	   a = b + ((a + F(b,c,d) + X[k] + T[i]) <<< s). */
#define F(x, y, z) (((x) & (y)) | (~(x) & (z)))
#define SET(a, b, c, d, k, s, Ti)\
  t = a + F (b,c,d) + X[k] + Ti;\
  a = ROTATE_LEFT (t, s) + b
    /* Do the following 16 operations. */
	SET (a, b, c, d,  0,  7,  T1);
	SET (d, a, b, c,  1, 12,  T2);
	SET (c, d, a, b,  2, 17,  T3);
	SET (b, c, d, a,  3, 22,  T4);
	SET (a, b, c, d,  4,  7,  T5);
	SET (d, a, b, c,  5, 12,  T6);
	SET (c, d, a, b,  6, 17,  T7);
	SET (b, c, d, a,  7, 22,  T8);
	SET (a, b, c, d,  8,  7,  T9);
	SET (d, a, b, c,  9, 12, T10);
	SET (c, d, a, b, 10, 17, T11);
	SET (b, c, d, a, 11, 22, T12);
	SET (a, b, c, d, 12,  7, T13);
	SET (d, a, b, c, 13, 12, T14);
	SET (c, d, a, b, 14, 17, T15);
	SET (b, c, d, a, 15, 22, T16);
#undef SET

	/* Round 2. */
	/* Let [abcd k s i] denote the operation
	   a = b + ((a + G(b,c,d) + X[k] + T[i]) <<< s). */
#define G(x, y, z) (((x) & (z)) | ((y) & ~(z)))
#define SET(a, b, c, d, k, s, Ti)\
  t = a + G (b,c,d) + X[k] + Ti;\
  a = ROTATE_LEFT (t, s) + b
     /* Do the following 16 operations. */
	SET (a, b, c, d,  1,  5, T17);
	SET (d, a, b, c,  6,  9, T18);
	SET (c, d, a, b, 11, 14, T19);
	SET (b, c, d, a,  0, 20, T20);
	SET (a, b, c, d,  5,  5, T21);
	SET (d, a, b, c, 10,  9, T22);
	SET (c, d, a, b, 15, 14, T23);
	SET (b, c, d, a,  4, 20, T24);
	SET (a, b, c, d,  9,  5, T25);
	SET (d, a, b, c, 14,  9, T26);
	SET (c, d, a, b,  3, 14, T27);
	SET (b, c, d, a,  8, 20, T28);
	SET (a, b, c, d, 13,  5, T29);
	SET (d, a, b, c,  2,  9, T30);
	SET (c, d, a, b,  7, 14, T31);
	SET (b, c, d, a, 12, 20, T32);
#undef SET

	/* Round 3. */
	/* Let [abcd k s t] denote the operation
	   a = b + ((a + H(b,c,d) + X[k] + T[i]) <<< s). */
#define H(x, y, z) ((x) ^ (y) ^ (z))
#define SET(a, b, c, d, k, s, Ti)\
  t = a + H (b,c,d) + X[k] + Ti;\
  a = ROTATE_LEFT (t, s) + b
     /* Do the following 16 operations. */
	SET (a, b, c, d,  5,  4, T33);
	SET (d, a, b, c,  8, 11, T34);
	SET (c, d, a, b, 11, 16, T35);
	SET (b, c, d, a, 14, 23, T36);
	SET (a, b, c, d,  1,  4, T37);
	SET (d, a, b, c,  4, 11, T38);
	SET (c, d, a, b,  7, 16, T39);
	SET (b, c, d, a, 10, 23, T40);
	SET (a, b, c, d, 13,  4, T41);
	SET (d, a, b, c,  0, 11, T42);
	SET (c, d, a, b,  3, 16, T43);
	SET (b, c, d, a,  6, 23, T44);
	SET (a, b, c, d,  9,  4, T45);
	SET (d, a, b, c, 12, 11, T46);
	SET (c, d, a, b, 15, 16, T47);
	SET (b, c, d, a,  2, 23, T48);
#undef SET

	/* Round 4. */
	/* Let [abcd k s t] denote the operation
	   a = b + ((a + I(b,c,d) + X[k] + T[i]) <<< s). */
#define I(x, y, z) ((y) ^ ((x) | ~(z)))
#define SET(a, b, c, d, k, s, Ti)\
  t = a + I (b,c,d) + X[k] + Ti;\
  a = ROTATE_LEFT (t, s) + b
     /* Do the following 16 operations. */
	SET (a, b, c, d,  0,  6, T49);
	SET (d, a, b, c,  7, 10, T50);
	SET (c, d, a, b, 14, 15, T51);
	SET (b, c, d, a,  5, 21, T52);
	SET (a, b, c, d, 12,  6, T53);
	SET (d, a, b, c,  3, 10, T54);
	SET (c, d, a, b, 10, 15, T55);
	SET (b, c, d, a,  1, 21, T56);
	SET (a, b, c, d,  8,  6, T57);
	SET (d, a, b, c, 15, 10, T58);
	SET (c, d, a, b,  6, 15, T59);
	SET (b, c, d, a, 13, 21, T60);
	SET (a, b, c, d,  4,  6, T61);
	SET (d, a, b, c, 11, 10, T62);
	SET (c, d, a, b,  2, 15, T63);
	SET (b, c, d, a,  9, 21, T64);
#undef SET

	/* Then perform the following additions. (That is increment each
	    of the four registers by the value it had before this block
	    was started.) */
	pms->abcd[0] += a;
	pms->abcd[1] += b;
	pms->abcd[2] += c;
	pms->abcd[3] += d;
}

static void
md5_init (md5_state_t *pms)
{
	pms->count[0] = pms->count[1] = 0;
	pms->abcd[0] = 0x67452301;
	pms->abcd[1] = /*0xefcdab89*/ T_MASK ^ 0x10325476;
	pms->abcd[2] = /*0x98badcfe*/ T_MASK ^ 0x67452301;
	pms->abcd[3] = 0x10325476;
}

static void
md5_append (md5_state_t *pms, const md5_byte_t *data, int nbytes)
{
	const md5_byte_t *p = data;
	int left = nbytes;
	int offset = (pms->count[0] >> 3) & 63;
	md5_word_t nbits = (md5_word_t)(nbytes << 3);

	if (nbytes <= 0)
		return;

	/* Update the message length. */
	pms->count[1] += nbytes >> 29;
	pms->count[0] += nbits;
	if (pms->count[0] < nbits)
		pms->count[1]++;

	/* Process an initial partial block. */
	if (offset) {
		int copy = (offset + nbytes > 64 ? 64 - offset : nbytes);

		memcpy (pms->buf + offset, p, copy);
		if (offset + copy < 64)
			return;
		p += copy;
		left -= copy;
		md5_process (pms, pms->buf);
	}

	/* Process full blocks. */
	for (; left >= 64; p += 64, left -= 64)
		md5_process (pms, p);

	/* Process a final partial block. */
	if (left)
		memcpy (pms->buf, p, left);
}

static void
md5_finish (md5_state_t *pms, md5_byte_t digest[16])
{
	static const md5_byte_t pad[64] = {
	    0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	};
	md5_byte_t data[8];
	int i;

	/* Save the length before padding. */
	for (i = 0; i < 8; ++i)
		data[i] = (md5_byte_t)(pms->count[i >> 2] >> ((i & 3) << 3));
	/* Pad to 56 bytes mod 64. */
	md5_append (pms, pad, ((55 - (pms->count[0] >> 3)) & 63) + 1);
	/* Append the length. */
	md5_append (pms, data, 8);
	for (i = 0; i < 16; ++i)
		digest[i] = (md5_byte_t)(pms->abcd[i >> 2] >> ((i & 3) << 3));
}
