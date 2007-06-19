/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2007 XMMS2 Team
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

/**
 * @file midi decoder
 */

#include "xmms/xmms_xformplugin.h"
#include "xmms/xmms_sample.h"
#include "xmms/xmms_log.h"
#include "xmms/xmms_medialib.h"

#include <glib.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

<<<<<<< HEAD:src/plugins/midi/midi.c
=======
#include "midi.h"

>>>>>>> dfe6712a3c63766a1a91bd3c8eca1155d0637d23:src/plugins/midi/midi.c
static gboolean xmms_midi_plugin_setup (xmms_xform_plugin_t *xform_plugin);
static gint xmms_midi_read (xmms_xform_t *xform, xmms_sample_t *buf, gint len, xmms_error_t *err);
static gboolean xmms_midi_init (xmms_xform_t *decoder);
static void xmms_midi_destroy (xmms_xform_t *decoder);
static gint64 xmms_midi_seek (xmms_xform_t *xform, gint64 samples, xmms_xform_seek_mode_t whence, xmms_error_t *err);

XMMS_XFORM_PLUGIN ("midi",
                   "MIDI Decoder", XMMS_VERSION,
                   "MIDI Decoder",
                   xmms_midi_plugin_setup);

static gboolean
xmms_midi_plugin_setup (xmms_xform_plugin_t *xform_plugin)
{
        xmms_xform_methods_t methods;

        XMMS_XFORM_METHODS_INIT (methods);
        methods.init = xmms_midi_init;
        methods.destroy = xmms_midi_destroy;
        methods.read = xmms_midi_read;
        methods.seek = xmms_midi_seek;

        xmms_xform_plugin_methods_set (xform_plugin, &methods);

        xmms_xform_plugin_indata_add (xform_plugin,
                                      XMMS_STREAM_TYPE_MIMETYPE,
                                      "audio/midi",
                                      NULL);


        xmms_magic_add ("midi header",
                        "audio/midi",
			"0 string MThd",
                        NULL);

        xmms_magic_extension_add ("audio/midi", "*.mid");

        return TRUE;
}

static gboolean xmms_midi_init (xmms_xform_t *xform)
{
	struct rootElement *root;
	struct sequenceState *seq;
<<<<<<< HEAD:src/plugins/midi/midi.c
	char buff[100000];
=======
>>>>>>> dfe6712a3c63766a1a91bd3c8eca1155d0637d23:src/plugins/midi/midi.c
	xmms_error_t error;
	
	g_return_val_if_fail(xform,FALSE);

<<<<<<< HEAD:src/plugins/midi/midi.c
//	root = midi_read(xform);

	xmms_xform_peek(xform,buff,10000,&error);

	freopen("/home/thisnukes4u/test.mid","w",stdout);
	printf("%s",buff);
=======
//	root = midi_read_head(xform);
	
//	root = midi_read_next(root);
>>>>>>> dfe6712a3c63766a1a91bd3c8eca1155d0637d23:src/plugins/midi/midi.c

	g_return_val_if_fail(root,FALSE);

//	seq = md_sequence_init(root);
<<<<<<< HEAD:src/plugins/midi/midi.c
	xmms_xform_private_data_set(xform,seq);
=======
	xmms_xform_private_data_set(xform,root);
>>>>>>> dfe6712a3c63766a1a91bd3c8eca1155d0637d23:src/plugins/midi/midi.c

	xmms_xform_outdata_type_add (xform,
                                     XMMS_STREAM_TYPE_MIMETYPE,
                                     "audio/seq",
                                     XMMS_STREAM_TYPE_END);

        return TRUE;
}

/* MIDI works one sequence at a time, so 
 * we only read one in.
 */
static gint xmms_midi_read (xmms_xform_t *xform, xmms_sample_t *buf, gint len, xmms_error_t *err)
{
<<<<<<< HEAD:src/plugins/midi/midi.c
=======
	struct rootElement *root;
>>>>>>> dfe6712a3c63766a1a91bd3c8eca1155d0637d23:src/plugins/midi/midi.c
	struct sequenceState *seq;
<<<<<<< HEAD:src/plugins/midi/midi.c
=======
	int buff;
	xmms_error_t error;
>>>>>>> dfe6712a3c63766a1a91bd3c8eca1155d0637d23:src/plugins/midi/midi.c

//	printf("Starting to read midi...\n");
<<<<<<< HEAD:src/plugins/midi/midi.c
//
//	g_return_val_if_fail(xform,-1);
//
//	seq = xmms_xform_private_data_get(xform);
//
//	g_return_val_if_fail(seq,-1);
=======

	g_return_val_if_fail(xform,-1);

	xmms_xform_read(xform,&buff,4,&error);
	printf("%x\n",buff);

//	root = xmms_xform_private_data_get(xform);

//	g_return_val_if_fail(root,-1);
>>>>>>> dfe6712a3c63766a1a91bd3c8eca1155d0637d23:src/plugins/midi/midi.c

//	buf = md_sequence_next(seq);

	return 1;
}

static gint64 xmms_midi_seek (xmms_xform_t *xform, gint64 samples, xmms_xform_seek_mode_t whence, xmms_error_t *err)
{
	return -1;
}

static void
xmms_midi_destroy (xmms_xform_t *xform)
{
	struct sequenceState *seq;

	g_return_if_fail(xform);	

	seq = xmms_xform_private_data_get(xform);	
	g_return_if_fail(seq);

	free(seq);
}

