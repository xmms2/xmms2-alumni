/*  youtube - An XMMS2 plugin that scrapes direct FLV links from a
 *  YouTube page
 *  Copyright (C) 2008 Anthony Garcia
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

/* YouTube has a habit of altering their page layout, let us pray
 * they do not alter it further.
 *
 * This code should be easily adaptable to new layouts if they do.
 */

#include <stdlib.h>
#include <curl/curl.h>
#include "xmms/xmms_xformplugin.h"
#include "xmms/xmms_log.h"
#include "xmms/xmms_config.h"
#include "xmms/xmms_medialib.h"

typedef struct {
	CURL *conn;

	/* config properties */
	gchar *uname;
	gchar *pword;

	/* video ID */
	gchar *vid;

	/* magic t value */
	gchar *t;

	/* video "title" */
	gchar *title;

	/* URL to the FLV */
	GString *flvurl;

	/* "channel" the video is part of */
	gchar *channel;

	/* misc. string, private */
	GString *miscstr;

	gchar curlerror[CURL_ERROR_SIZE];
} xmms_youtube_data_t;

static const gchar *metatitle = "<title>YouTube - ";
static const gchar *baseurl = "http://www.youtube.com/watch?v=";
static const gchar *nextstr = "next_url=%2Fwatch%3Fv%3D";
static const gchar *fmtstr = "http://www.youtube.com/get_video?video_id=%s&t=%s";
static const gchar *loginfmtstr = "http://www.youtube.com/signup"
                                  "?current_form=loginForm&username=%s&"
				  "password=%s&action_login=Log+In";
static const gchar *agestr = "http://www.youtube.com/verify_age?"
                             "action_confirm=Confirm";
static const gchar *ageconfirm = "video or group may contain content that is inappropriate for some users";
static const gchar *badlogin = "Sorry, your login was incorrect.";

static gboolean xmms_youtube_setup (xmms_xform_plugin_t *xform);
static gboolean xmms_youtube_init (xmms_xform_t *xform);

static gboolean yt_login (xmms_xform_t *ytdata);
static gboolean yt_get_vidparams (xmms_xform_t *xform);
static size_t yt_write (void *ptr, size_t size, size_t nmemb, void *stream);
static xmms_youtube_data_t *yt_new (xmms_xform_t *xform);
static void yt_free (xmms_xform_t *xform);
static char *yt_deentisize (char *in);

XMMS_XFORM_PLUGIN ("youtube",
                   "YouTube Transport",
                   XMMS_VERSION,
                   "Builds URLs to FLVs of YouTube videos",
                   xmms_youtube_setup);

static gboolean
xmms_youtube_setup (xmms_xform_plugin_t *xform)
{
	xmms_xform_methods_t methods;
	XMMS_XFORM_METHODS_INIT (methods);

	methods.init = xmms_youtube_init;
	xmms_xform_plugin_methods_set (xform, &methods);

	/* can't use regular youtube.com/watch?v=id because
	 * of the automatic url encoding.
	 * youtube.com/watch/?v=id will just be a 404.
	 */
	xmms_xform_plugin_indata_add (xform,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "application/x-url",
	                              XMMS_STREAM_TYPE_URL,
	                              "yt://*",
	                              XMMS_STREAM_TYPE_END);

	/* leave either blank for no login */
	xmms_xform_plugin_config_property_register (xform, "username",
	                                            "", NULL, NULL);
	xmms_xform_plugin_config_property_register (xform, "password",
	                                            "", NULL, NULL);

	return TRUE;
}

static gboolean
xmms_youtube_init (xmms_xform_t *xform)
{
	gboolean ret = FALSE;
	const gchar *property = NULL;
	GString *inurl;
	xmms_youtube_data_t *ytdata;

	inurl = g_string_new (NULL);
	ytdata = yt_new (xform);
	xmms_xform_private_data_set (xform, ytdata);

	curl_easy_setopt (ytdata->conn, CURLOPT_FOLLOWLOCATION, 1);
	curl_easy_setopt (ytdata->conn, CURLOPT_COOKIEFILE, "");
	curl_easy_setopt (ytdata->conn, CURLOPT_USERAGENT, "XMMS2/" XMMS_VERSION);
	curl_easy_setopt (ytdata->conn, CURLOPT_WRITEFUNCTION, yt_write);
	curl_easy_setopt (ytdata->conn, CURLOPT_WRITEDATA, xform);
	curl_easy_setopt (ytdata->conn, CURLOPT_ERRORBUFFER, ytdata->curlerror);
	/* curl_easy_setopt (ytdata->conn, CURLOPT_VERBOSE, 1); */

	g_string_append (g_string_assign (inurl, baseurl), ytdata->vid);

	if (!yt_login (xform)) {
		xmms_log_error ("Login failed!");
	}

	curl_easy_setopt (ytdata->conn, CURLOPT_URL, inurl->str);

	ret = TRUE;

	if (inurl->len && curl_easy_perform (ytdata->conn) == CURLE_OK) {

		/* If the age flag is set */
		if (strstr (ytdata->miscstr->str, ageconfirm)) {
			g_string_truncate (ytdata->miscstr, 0);
			g_string_printf (inurl, "%s&%s%s", agestr, nextstr,
			                 ytdata->vid);

			curl_easy_setopt (ytdata->conn, CURLOPT_URL, inurl->str);
			if (curl_easy_perform (ytdata->conn) != CURLE_OK) {
				xmms_log_error ("%s", ytdata->curlerror);
			}
		}

		if (yt_get_vidparams (xform) && ytdata->flvurl->len) {

			property = XMMS_MEDIALIB_ENTRY_PROPERTY_TITLE;
			if (ytdata->title) {
				xmms_xform_metadata_set_str (xform,
				                             property,
				                             ytdata->title);
			}

			property = XMMS_MEDIALIB_ENTRY_PROPERTY_CHANNEL;
			xmms_xform_metadata_set_str (xform, property,
			                             (ytdata->channel)?
			                             ytdata->channel : "YouTube");

			xmms_xform_outdata_type_add (xform,
			                             XMMS_STREAM_TYPE_MIMETYPE,
			                             "application/x-url",
			                             XMMS_STREAM_TYPE_URL,
			                             ytdata->flvurl->str,
			                             XMMS_STREAM_TYPE_END);
		} else {
			xmms_log_error ("Not a YouTube video page");
			ret = FALSE;
		}
	} else {
		xmms_log_error ("%s", ytdata->curlerror);
		ret = FALSE;
	}

	yt_free (xform);
	g_string_free (inurl, TRUE);
	return ret;
}

static gboolean
yt_login (xmms_xform_t *xform)
{
	xmms_youtube_data_t *ytdata;
	gboolean ret = FALSE;
	GString *loginurl;

	ytdata = xmms_xform_private_data_get (xform);

	if (!*ytdata->uname || !*ytdata->pword) {
		return TRUE;
	}

	loginurl = g_string_new (NULL);
	g_string_printf (loginurl, loginfmtstr, ytdata->uname, ytdata->pword);
	curl_easy_setopt (ytdata->conn, CURLOPT_URL, loginurl->str);

	if (curl_easy_perform (ytdata->conn) != CURLE_OK) {
		xmms_log_error ("%s\n", ytdata->curlerror);
	} else if (strstr (ytdata->miscstr->str, badlogin)) {
		xmms_log_error ("Bad login");
	} else {
		ret = TRUE;
	}

	g_string_free (loginurl, TRUE);
	g_string_truncate (ytdata->miscstr, 0);
	return ret;
}

static gboolean
yt_get_vidparams (xmms_xform_t *xform)
{
	xmms_youtube_data_t *ytdata;
	gchar *cloc = NULL, *flashargs = NULL, *brace = NULL;
	size_t vt = 0;

	ytdata = xmms_xform_private_data_get (xform);

	/* as far as I know the channel name is the same
	 * as the username that uploaded the video.
	 */
	cloc = strstr (ytdata->miscstr->str, "'VIDEO_USERNAME': '");
	if (cloc) {
		cloc += 19;
		vt = strcspn (cloc, "'");
		ytdata->channel = g_strndup (cloc, vt);
	}

	cloc = strstr (ytdata->miscstr->str, metatitle);
	if (cloc) {
		cloc += strlen (metatitle);
		vt = strcspn (cloc, "<");
		ytdata->title = yt_deentisize (g_strndup (cloc, vt));
	}

	flashargs = strstr (ytdata->miscstr->str, "SWF_ARGS");
	if (!flashargs) {
		return FALSE;
	}

	brace = strchr (flashargs, '}');
	if (!brace) {
		return FALSE;
	} else {
		*(brace + 1) = '\0';
	}

	if ((cloc = strstr (flashargs, "\"t\": \""))) {
		cloc += 6;
		vt = strcspn (cloc, "\"");
		ytdata->t = g_strndup (cloc, vt);

		g_string_printf (ytdata->flvurl, fmtstr, ytdata->vid, ytdata->t);
		g_string_truncate (ytdata->miscstr, 0);

		return TRUE;
	}

	return FALSE;
}

static size_t
yt_write (void *ptr, size_t size, size_t nmemb, void *stream)
{
	xmms_youtube_data_t *data;

	data = xmms_xform_private_data_get (stream);
	g_string_append_len (data->miscstr, ptr, size * nmemb);

	return size * nmemb;
}

static xmms_youtube_data_t *
yt_new (xmms_xform_t *xform)
{
	xmms_youtube_data_t *ret;
	xmms_config_property_t *conf;

	ret = g_new0 (xmms_youtube_data_t, 1);
	ret->flvurl = g_string_new (NULL);
	ret->conn = curl_easy_init ();

	conf = xmms_xform_config_lookup (xform, "username");
	ret->uname = curl_easy_escape (ret->conn, xmms_config_property_get_string (conf), 0);

	conf = xmms_xform_config_lookup (xform, "password");
	ret->pword = curl_easy_escape (ret->conn, xmms_config_property_get_string (conf), 0);

	ret->vid = g_path_get_basename (xmms_xform_get_url (xform));

	ret->miscstr = g_string_new (NULL);

	return ret;
}

static void
yt_free (xmms_xform_t *xform)
{
	xmms_youtube_data_t *ytdata;

	ytdata = xmms_xform_private_data_get (xform);

	curl_easy_cleanup (ytdata->conn);
	g_string_free (ytdata->flvurl, TRUE);
	g_free (ytdata->vid);
	g_free (ytdata->title);
	g_free (ytdata->t);
	g_string_free (ytdata->miscstr, TRUE);
	g_free (ytdata->channel);
	curl_free (ytdata->uname);
	curl_free (ytdata->pword);
	g_free (ytdata);
}

char *
yt_deentisize (char *in)
{
	char *ret = in, *col;

	for (; *in; in++) {
		if (*in == '&') {
			if ((col = strchr (in, ';'))) {
				*col++ = '\0';
				if (!strcmp (in + 1, "quot")) {
					*in = '"';
				} else if (!strcmp (in + 1, "amp")) {
					*in = '&';
				} else if (!strcmp (in + 1, "apos")) {
					*in = '\'';
				} else if (!strcmp (in + 1, "lt")) {
					*in = '<';
				} else if (!strcmp (in + 1, "gt")) {
					*in = '>';
				} else {
					*in = ' ';
				}

				memmove (in + 1, col, strlen (col) + 1);
			}
		}
	}

	return ret;
}
