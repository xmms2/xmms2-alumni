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
#ifndef __CMD_STATUS_H__
#define __CMD_STATUS_H__

#include <xmmsclient/xmmsclient.h>
#include <xmmsclient/xmmsclient-glib.h>

#include <glib.h>

/**
 * Function prototypes
 */
void handle_current_id (xmmsc_result_t *res, void *userdata);
void handle_playtime (xmmsc_result_t *res, void *userdata);
void handle_mediainfo_update (xmmsc_result_t *res, void *userdata);
void handle_status_change (xmmsc_result_t *res, void *userdata);
void do_mediainfo (xmmsc_result_t *res, void *userdata);
void update_display ();
void quit (void *data);


/**
 * Globals
 */
static gboolean has_songname = FALSE;
static gboolean fetching_songname = FALSE;
static guint current_id = 0;
static guint last_dur = 0;
static gint curr_dur = 0;
static gchar songname[256];
static guint curr_status = 0;

static gchar *status_messages[] = {
	"Stopped",
	"Playing",
	"Paused"
};

#endif
