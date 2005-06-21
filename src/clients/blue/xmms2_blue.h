/*
 *  XMMS2 Bluetooth Client
 *  Copyright (C) 2005 Jens Taprogge
 *
 *  This code is heavily based on the XMMS2 CLI client by
 *  Peter Alm, Tobias Rundström, Anders Gustafsson.
 *
 *  It is inspired by Tom Gilbert's XMMS Bluetooth client 'bluexmms'
 */

#ifndef __XMMS2_BLUE_H
#define __XMMS2_BLUE_H

#include <xmmsclient/xmmsclient.h>
#include <xmmsclient/xmmsclient-glib.h>

#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include <glib.h>

#define DEBUG

#ifdef DEBUG
#define debug(fmt, args...) fprintf(stderr, fmt, ## args)
#else
#define debug(fmt, args...) 
#endif


struct cmds {
	char *name;
	int (*func) (xmmsc_connection_t *conn, int user, int dev);
	int arg;
};

void print_info (const char *fmt, ...);
char *format_url (char *item);
void print_error (const char *fmt, ...);
void print_hash (const void *key, const void *value, void *udata);
void format_pretty_list (xmmsc_connection_t *conn, GList *list);


/* Returns filedescriptor, negative on error */
int blue_open(char *devname);

/* 0 on success, negative on error */
int blue_init(int dev);

/* Get a line (ended by \n) from dev and copy to a.
 * returns:
 *  sucess: positive - number of chars
 *  -1: read failed
 *  -2: buffer too small
 *  if the return value of a called menufunc is negative the function exits and
 *  returns that value */
ssize_t blue_get(int dev, char *a, size_t size);
ssize_t blue_get_block(int dev, char *a, size_t size);

/* Write c appended by '\r' to dev.
 * positive: succes - number of chars written (including '\r')
 * negative: return value from write() */
ssize_t blue_put(int dev, char *c);

/* Write c to dev and check for answer expect. 
 * Use while device is _not_ in echo mode. 0 on success. 
 * returns: 
 *    0 on success. 
 *   -1 if the answer does not match 'expect'
 *   -2 if reading ar writing to the device failed. */
int blue_put_expect(int dev, char* c, char *expect);

/* Same as above, but in echo mode. */
int blue_put_echo_expect(int dev, char* c, char *expect);

/* returns:
 *   0  on menu exit
 *  -1  on failures
 *  -2  on failure of the blocking routine */
int blue_menu(int dev, char *title, struct cmds *items, int init, void *user);

/* returns:
 *  positive value on selection of a valid option
 *   0  on reject
 *  -1  on abort or failures
 *  -2  on failure of the blocking routine */
int blue_select(int dev, char *title, char **item, int init);

/* max steps: 10 */
int blue_percent(int dev, char *title, int steps, int init, int (*updfunc)(int perc, void *user), void *user);

#endif
