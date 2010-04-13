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

#include <stdlib.h>
#include <curl/curl.h>
#include "xmms/xmms_xformplugin.h"
#include "xmms/xmms_log.h"
#include "xmms/xmms_config.h"
#include "xmms/xmms_medialib.h"
#include "xmms/xmms_bindata.h"

typedef struct {
	CURL *conn;

	/* video ID */
	gchar *vid;

	/* video "title" */
	gchar *title;

	/* URL to the FLV */
	GString *flvurl;

	/* "channel" the video is part of */
	gchar *channel;

	/* mime type of the last received data, if any */
	gchar *last_mime;

	gboolean download_thumbnail;

	/* misc. string, private */
	GString *miscstr;

	gchar curlerror[CURL_ERROR_SIZE];
} xmms_youtube_data_t;

static const gchar *fmtstr = "http://www.youtube.com/get_video?video_id=%s&t=%s";
static const gchar *infostr = "http://www.youtube.com/get_video_info?video_id=";

static gboolean xmms_youtube_setup (xmms_xform_plugin_t *xform);
static gboolean xmms_youtube_init (xmms_xform_t *xform);

static gboolean yt_get_vidparams (xmms_xform_t *xform);
static size_t yt_write (void *ptr, size_t size, size_t nmemb, void *stream);
static size_t yt_header_write (void *ptr, size_t size, size_t nmemb, void *stream);
static xmms_youtube_data_t *yt_new (xmms_xform_t *xform);
static void yt_free (xmms_xform_t *xform);

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

	xmms_xform_plugin_indata_add (xform,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "application/x-url",
	                              XMMS_STREAM_TYPE_URL,
	                              "http://*youtube.*/watch*",
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_plugin_config_property_register (xform, "download_thumbnail",
	                                            "1", NULL, NULL);

	return TRUE;
}

static gboolean
xmms_youtube_init (xmms_xform_t *xform)
{
	gboolean ret = FALSE;
	const gchar *property = NULL;
	xmms_youtube_data_t *ytdata;
	xmms_config_property_t *conf;

	ytdata = yt_new (xform);
	xmms_xform_private_data_set (xform, ytdata);

	conf = xmms_xform_config_lookup (xform, "download_thumbnail");
	ytdata->download_thumbnail = xmms_config_property_get_int (conf);

	curl_easy_setopt (ytdata->conn, CURLOPT_FOLLOWLOCATION, 1);
	curl_easy_setopt (ytdata->conn, CURLOPT_COOKIEFILE, "");
	curl_easy_setopt (ytdata->conn, CURLOPT_USERAGENT, "XMMS2/" XMMS_VERSION);
	curl_easy_setopt (ytdata->conn, CURLOPT_WRITEFUNCTION, yt_write);
	curl_easy_setopt (ytdata->conn, CURLOPT_WRITEDATA, xform);
	curl_easy_setopt (ytdata->conn, CURLOPT_HEADERDATA, xform);
	curl_easy_setopt (ytdata->conn, CURLOPT_HEADERFUNCTION, yt_header_write);
	curl_easy_setopt (ytdata->conn, CURLOPT_ERRORBUFFER, ytdata->curlerror);
	/* curl_easy_setopt (ytdata->conn, CURLOPT_VERBOSE, 1); */

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

		ret = TRUE;
	} else {
		xmms_log_error ("Failed to get video. See logs");
	}

	yt_free (xform);
	return ret;
}

static gboolean
yt_get_vidparams (xmms_xform_t *xform)
{
	xmms_youtube_data_t *ytdata;
	gboolean ret = FALSE;
	gchar *url, **qs_part;
	gint i;

	ytdata = xmms_xform_private_data_get (xform);

	url = g_strconcat (infostr, ytdata->vid, NULL);
	curl_easy_setopt (ytdata->conn, CURLOPT_URL, url);

	if (curl_easy_perform (ytdata->conn) != CURLE_OK) {
		xmms_log_error ("%s\n", ytdata->curlerror);
	} else {
		qs_part = g_strsplit (ytdata->miscstr->str, "&", 0);
		g_string_truncate (ytdata->miscstr, 0);

		for (i = 0; qs_part[i]; i++) {
			gchar *k = qs_part[i], *v, *uv;

			if (!(v = strchr (k, '='))) {
				continue;
			}
			*v++ = '\0';

			uv = curl_easy_unescape (ytdata->conn, v,
			                         strlen (v), NULL);
			g_strdelimit (uv, "+", ' ');

			XMMS_DBG ("'%s' : '%s'", k, uv);

			if (!strcmp (k, "token")) {
				g_string_printf (ytdata->flvurl, fmtstr, ytdata->vid, uv);
				ret = TRUE;
			} else if (!strcmp (k, "creator")) {
				ytdata->channel = g_strdup (uv);
			} else if (!strcmp (k, "title")) {
				ytdata->title = g_strdup (uv);
			} else if (!strcmp (k, "errorcode")) {
				xmms_log_error ("YouTube returned error %s", uv);
				ret = FALSE;
			} else if (!strcmp (k, "reason")) {
				xmms_log_error ("%s", uv);
				ret = FALSE;
			} else if (!strcmp (k, "thumbnail_url") &&
			           ytdata->download_thumbnail) {
				curl_easy_setopt (ytdata->conn, CURLOPT_URL, uv);
				if (curl_easy_perform (ytdata->conn) != CURLE_OK) {
					xmms_log_error ("Couldn't retrieve thumbnail image");
				} else {
					if (ytdata->last_mime) {
						gchar thumb_hash[33];
						const gchar *prop;

						XMMS_DBG ("Thumbnail mime type is `%s'",
						          ytdata->last_mime);

						xmms_bindata_calculate_md5 ((guchar *)ytdata->miscstr->str,
						                            ytdata->miscstr->len,
						                            thumb_hash);
						xmms_bindata_plugin_add ((guchar *)ytdata->miscstr->str,
						                         ytdata->miscstr->len,
						                         thumb_hash);

						prop = XMMS_MEDIALIB_ENTRY_PROPERTY_PICTURE_FRONT;
						xmms_xform_metadata_set_str (xform, prop, thumb_hash);
						prop = XMMS_MEDIALIB_ENTRY_PROPERTY_PICTURE_FRONT_MIME;
						xmms_xform_metadata_set_str (xform, prop, ytdata->last_mime);
					}
					g_string_truncate (ytdata->miscstr, 0);
				}
			}

			curl_free (uv);
		}

		g_strfreev (qs_part);
	}

	g_free (url);
	g_string_truncate (ytdata->miscstr, 0);

	return ret;
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
	const gchar *vid;

	ret = g_new0 (xmms_youtube_data_t, 1);
	ret->flvurl = g_string_new (NULL);
	ret->conn = curl_easy_init ();

	xmms_xform_metadata_get_str (xform, "v", &vid);
	ret->vid = g_strdup (vid);

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
	g_string_free (ytdata->miscstr, TRUE);
	g_free (ytdata->channel);
	g_free (ytdata->last_mime);
	g_free (ytdata);
}

static size_t
yt_header_write (void *ptr, size_t size, size_t nmemb, void *stream)
{
	xmms_youtube_data_t *ytdata;
	gchar *hline = ptr;

	ytdata = xmms_xform_private_data_get ((xmms_xform_t *)stream);

	hline[(size * nmemb) - 2] = '\0';

	if (!g_ascii_strncasecmp ("content-type", hline, 12)) {
		g_free (ytdata->last_mime);
		ytdata->last_mime = g_strdup (hline + 14);
	}

	return size * nmemb;
}
