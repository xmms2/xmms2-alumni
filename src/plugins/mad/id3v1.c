/*  XMMS2 - X Music Multiplexer System
 *
 *  Copyright (C) 2003-2007 XMMS2 Team
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 */

/**
 *  ID3v1
 */

#include <glib.h>
#include "xmms/xmms_xformplugin.h"
#include "xmms/xmms_medialib.h"
#include "xmms/xmms_log.h"
#include "id3v1.h"

static const gchar * const id3_genres[] = {
        "Blues", "Classic Rock", "Country", "Dance",
        "Disco", "Funk", "Grunge", "Hip-Hop",
        "Jazz", "Metal", "New Age", "Oldies",
        "Other", "Pop", "R&B", "Rap", "Reggae",
        "Rock", "Techno", "Industrial", "Alternative",
        "Ska", "Death Metal", "Pranks", "Soundtrack",
        "Euro-Techno", "Ambient", "Trip-Hop", "Vocal",
        "Jazz+Funk", "Fusion", "Trance", "Classical",
        "Instrumental", "Acid", "House", "Game",
        "Sound Clip", "Gospel", "Noise", "Alt",
        "Bass", "Soul", "Punk", "Space",
        "Meditative", "Instrumental Pop",
        "Instrumental Rock", "Ethnic", "Gothic",
        "Darkwave", "Techno-Industrial", "Electronic",
        "Pop-Folk", "Eurodance", "Dream",
        "Southern Rock", "Comedy", "Cult",
        "Gangsta Rap", "Top 40", "Christian Rap",
        "Pop/Funk", "Jungle", "Native American",
        "Cabaret", "New Wave", "Psychedelic", "Rave",
        "Showtunes", "Trailer", "Lo-Fi", "Tribal",
        "Acid Punk", "Acid Jazz", "Polka", "Retro",
        "Musical", "Rock & Roll", "Hard Rock", "Folk",
        "Folk/Rock", "National Folk", "Swing",
        "Fast-Fusion", "Bebob", "Latin", "Revival",
        "Celtic", "Bluegrass", "Avantgarde",
        "Gothic Rock", "Progressive Rock",
        "Psychedelic Rock", "Symphonic Rock", "Slow Rock",
        "Big Band", "Chorus", "Easy Listening",
        "Acoustic", "Humour", "Speech", "Chanson",
        "Opera", "Chamber Music", "Sonata", "Symphony",
        "Booty Bass", "Primus", "Porn Groove",
        "Satire", "Slow Jam", "Club", "Tango",
        "Samba", "Folklore", "Ballad", "Power Ballad",
        "Rhythmic Soul", "Freestyle", "Duet",
        "Punk Rock", "Drum Solo", "A Cappella",
        "Euro-House", "Dance Hall", "Goa",
        "Drum & Bass", "Club-House", "Hardcore",
        "Terror", "Indie", "BritPop", "Negerpunk",
        "Polsk Punk", "Beat", "Christian Gangsta Rap",
        "Heavy Metal", "Black Metal", "Crossover",
        "Contemporary Christian", "Christian Rock",
        "Merengue", "Salsa", "Thrash Metal",
        "Anime", "JPop", "Synthpop"
};

typedef struct id3v1tag_St {
        char tag[3]; /* always "TAG": defines ID3v1 tag 128 bytes before EOF */
        char title[30];
        char artist[30];
        char album[30];
        char year[4];
        union {
                struct {
                        char comment[30];
                } v1_0;
                struct {
                        char comment[28];
                        char __zero;
                        unsigned char track_number;
                } v1_1;
        } u;
        unsigned char genre;
} id3v1tag_t;

static void
xmms_mad_id3v1_set (xmms_xform_t *xform, const char *key, const char *value,
                    int len, const char *encoding)
{
	gsize readsize,writsize;
	GError *err = NULL;
	gchar *tmp;

	/* property already set? */
	if (xmms_xform_metadata_has_val (xform, key)) {
		return;
	}

	g_clear_error (&err);

	tmp = g_convert (value, len, "UTF-8", encoding, &readsize, &writsize, &err);
	if (!tmp) {
		/* in case of not supported encoding, we try to fallback to latin1 */
		xmms_log_info ("Converting ID3v1 tag '%s' failed (check id3v1_encoding property): %s", key, err ? err->message : "Error not set");
		err = NULL;
		tmp = g_convert (value, len, "UTF-8", "ISO8859-1", &readsize, &writsize, &err);
	}
	if (tmp) {
		g_strstrip (tmp);
		if (tmp[0] != '\0') {
			xmms_xform_metadata_set_str (xform, key, tmp);
		}
		g_free (tmp);
	}
}

gboolean
xmms_mad_id3v1_parse (xmms_xform_t *xform, guchar *buf)
{
	xmms_config_property_t *config;
	id3v1tag_t *tag = (id3v1tag_t *) buf;
	const char *encoding;
	gboolean tmp;

	if (strncmp (tag->tag, "TAG", 3) != 0) {
		return FALSE;
	}

	XMMS_DBG ("Found ID3v1 TAG!");

	config = xmms_xform_config_lookup (xform, "id3v1_encoding");
	g_return_val_if_fail (config, FALSE);
	encoding = xmms_config_property_get_string (config);

	xmms_mad_id3v1_set (xform, XMMS_MEDIALIB_ENTRY_PROPERTY_ARTIST, tag->artist,
	                    sizeof (tag->artist), encoding);
	xmms_mad_id3v1_set (xform, XMMS_MEDIALIB_ENTRY_PROPERTY_ALBUM, tag->album,
	                    sizeof (tag->album), encoding);
	xmms_mad_id3v1_set (xform, XMMS_MEDIALIB_ENTRY_PROPERTY_TITLE, tag->title,
	                    sizeof (tag->title), encoding);
	xmms_mad_id3v1_set (xform, XMMS_MEDIALIB_ENTRY_PROPERTY_YEAR, tag->year,
	                    sizeof (tag->year), encoding);


	if (!xmms_xform_metadata_has_val (xform,
	    XMMS_MEDIALIB_ENTRY_PROPERTY_GENRE)) {
		if (tag->genre >= G_N_ELEMENTS (id3_genres)) {
			xmms_xform_metadata_set_str (xform,
			                             XMMS_MEDIALIB_ENTRY_PROPERTY_GENRE,
			                             "Unknown");
		} else {
			xmms_xform_metadata_set_str (xform,
			                             XMMS_MEDIALIB_ENTRY_PROPERTY_GENRE,
			                             id3_genres[tag->genre]);
		}
	}

	if (tag->u.v1_1.__zero == 0 && tag->u.v1_1.track_number > 0) {
		/* V1.1 */
		xmms_mad_id3v1_set (xform, XMMS_MEDIALIB_ENTRY_PROPERTY_COMMENT,
		                    tag->u.v1_1.comment, sizeof (tag->u.v1_1.comment),
		                    encoding);

		tmp =
			xmms_xform_metadata_has_val (xform,
			                             XMMS_MEDIALIB_ENTRY_PROPERTY_TRACKNR);
		if (!tmp) {
			xmms_xform_metadata_set_int (xform,
			                             XMMS_MEDIALIB_ENTRY_PROPERTY_TRACKNR,
			                             tag->u.v1_1.track_number);
		}
	} else {
		xmms_mad_id3v1_set (xform, XMMS_MEDIALIB_ENTRY_PROPERTY_COMMENT,
		                    tag->u.v1_0.comment, sizeof (tag->u.v1_0.comment),
		                    encoding);
	}

	return TRUE;
}
