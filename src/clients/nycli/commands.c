/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2012 XMMS2 Team
 *
 *  PLUGINS ARE NOT CONSIDERED TO BE DERIVED WORK !!!
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 */

#include "commands.h"

#include "cli_infos.h"
#include "cli_cache.h"
#include "command_trie.h"
#include "command_utils.h"
#include "cmdnames.h"
#include "configuration.h"
#include "utils.h"
#include "column_display.h"
#include "matching_browse.h"

#include <sys/stat.h>

#define COMMAND_HELP_DESCRIPTION_INDENT 2

#define NULL_SUB(elem, null, notnull) (elem) == NULL ? (null) : (notnull)

#define GOODCHAR(a) ((((a) >= 'a') && ((a) <= 'z')) || \
                     (((a) >= 'A') && ((a) <= 'Z')) || \
                     (((a) >= '0') && ((a) <= '9')) || \
                     ((a) == ':') || \
                     ((a) == '/') || \
                     ((a) == '-') || \
                     ((a) == '_') || \
                     ((a) == '.') || \
                     ((a) == '*') || \
                     ((a) == '?'))


static gboolean playlist_currpos_get (cli_infos_t *, const gchar *, gint *);
static gboolean playlist_length_get (cli_infos_t *, const gchar *, gint *);

/* Setup commands */

#define CLI_SIMPLE_SETUP(name, cmd, req, usage, desc) \
	void \
	cmd##_setup (command_action_t *action) \
	{ command_action_fill (action, name, cmd, req, NULL, usage, desc); }

CLI_SIMPLE_SETUP("play", cli_play,
                 COMMAND_REQ_CONNECTION,
                 NULL,
                 _("Start playback."))
CLI_SIMPLE_SETUP("pause", cli_pause,
                 COMMAND_REQ_CONNECTION,
                 NULL,
                 _("Pause playback."))
CLI_SIMPLE_SETUP("toggle", cli_toggle, /* <<<<< */
                 COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE,
                 NULL,
                 _("Toggle playback."))
CLI_SIMPLE_SETUP("stop", cli_stop,
                 COMMAND_REQ_CONNECTION,
                 NULL,
                 _("Stop playback."))
CLI_SIMPLE_SETUP("seek", cli_seek,
                 COMMAND_REQ_CONNECTION,
                 _("<time|offset>"),
                 _("Seek to a relative or absolute position."))
CLI_SIMPLE_SETUP("prev", cli_prev,
                 COMMAND_REQ_CONNECTION,
                 _("[offset]"),
                 _("Jump to previous song."))
CLI_SIMPLE_SETUP("next", cli_next,
                 COMMAND_REQ_CONNECTION,
                 _("[offset]"),
                 _("Jump to next song."))
CLI_SIMPLE_SETUP("info", cli_info,
                 COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE,
                 _("<pattern|positions>"),
                 _("Display all the properties for all media matching the pattern."))
CLI_SIMPLE_SETUP("exit", cli_exit,
                 COMMAND_REQ_NONE,
                 NULL,
                 _("Exit the shell-like interface."))
CLI_SIMPLE_SETUP("playlist switch", cli_pl_switch,
                 COMMAND_REQ_CONNECTION,
                 _("<playlist>"),
                 _("Change the active playlist."))
CLI_SIMPLE_SETUP("playlist remove", cli_pl_remove,
                 COMMAND_REQ_CONNECTION,
                 _("<playlist>"),
                 _("Remove the given playlist."))
CLI_SIMPLE_SETUP("playlist clear", cli_pl_clear,
                 COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE,
                 _("[playlist]"),
                 _("Clear a playlist.  By default, clear the active playlist."))
CLI_SIMPLE_SETUP("playlist shuffle", cli_pl_shuffle,
                 COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE,
                 _("[playlist]"),
                 _("Shuffle a playlist.  By default, shuffle the active playlist."))
CLI_SIMPLE_SETUP("collection list", cli_coll_list,
                 COMMAND_REQ_CONNECTION,
                 NULL,
                 _("List all collections."))
CLI_SIMPLE_SETUP("collection show", cli_coll_show,
                 COMMAND_REQ_CONNECTION,
                 _("<collection>"),
                 _("Display a human-readable description of a collection."));
CLI_SIMPLE_SETUP("collection remove", cli_coll_remove,
                 COMMAND_REQ_CONNECTION,
                 _("<collection>"),
                 _("Remove a collection."))
CLI_SIMPLE_SETUP("collection config", cli_coll_config,
                 COMMAND_REQ_CONNECTION,
                 _("<collection> [attrname [attrvalue]]"),
                 _("Get or set attributes for the given collection.\n"
                   "If no attribute name is provided, list all attributes.\n"
                   "If only an attribute name is provided, display the value of the attribute.\n"
                   "If both attribute name and value are provided, set the new value of the attribute."))
CLI_SIMPLE_SETUP("server browse", cli_server_browse,
                 COMMAND_REQ_CONNECTION,
                 _("<url>"),
                 _("Browse a resource location."))
CLI_SIMPLE_SETUP("server remove", cli_server_remove,
                 COMMAND_REQ_CONNECTION,
                 _("<pattern>"),
                 _("Remove the matching media from the media library."))
CLI_SIMPLE_SETUP("server rehash", cli_server_rehash,
                 COMMAND_REQ_CONNECTION,
                 _("[pattern]"),
                 _("Rehash the media matched by the pattern,\n"
                   "or the whole media library if no pattern is provided"))
CLI_SIMPLE_SETUP("server config", cli_server_config,
                 COMMAND_REQ_CONNECTION,
                 _("[name [value]]"),
                 _("Get or set configuration values.\n"
                   "If no name or value is provided, list all configuration values.\n"
                   "If only a name is provided, display the content of the corresponding configuration value.\n"
                   "If both name and a value are provided, set the new content of the configuration value."))
CLI_SIMPLE_SETUP("server plugins", cli_server_plugins,
                 COMMAND_REQ_CONNECTION,
                 NULL,
                 _("List the plugins loaded in the server."))
CLI_SIMPLE_SETUP("server stats", cli_server_stats,
                 COMMAND_REQ_CONNECTION,
                 NULL,
                 _("Display statistics about the server: uptime, version, size of the medialib, etc"))
CLI_SIMPLE_SETUP("server sync", cli_server_sync,
                 COMMAND_REQ_CONNECTION,
                 NULL,
                 _("Force the saving of collections to the disk (otherwise only performed on shutdown)"))
CLI_SIMPLE_SETUP("server shutdown", cli_server_shutdown,
                 COMMAND_REQ_CONNECTION | COMMAND_REQ_NO_AUTOSTART,
                 NULL,
                 _("Shutdown the server."))

/* FIXME: macro for setup with flags (+ use ##x for f/f_setup?) */

void
cli_help_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "alias", 'a', 0, G_OPTION_ARG_NONE, NULL, _("List aliases, or alias definition."), NULL },
		{ NULL }
	};
	command_action_fill (action, "help", &cli_help, COMMAND_REQ_NONE, flags,
	                     _("[-a] [command]"),
	                     _("List all commands, or help on one command."));
}

void
cli_jump_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "backward", 'b', 0, G_OPTION_ARG_NONE, NULL, _("Jump backward to the first media matching the pattern"), NULL },
		{ NULL }
	};
	command_action_fill (action, "jump", &cli_jump, COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE, flags,
	                     _("[-b] <pattern|positions>"),
	                     _("Jump to the first media matching the pattern."));
}

void
cli_search_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "order",   'o', 0, G_OPTION_ARG_STRING, NULL, _("List of properties to order by (prefix by '-' for reverse ordering)."), "prop[,...]" },
		{ "columns", 'l', 0, G_OPTION_ARG_STRING, NULL, _("List of properties to use as columns."), "prop[,...]" },
		{ NULL }
	};
	command_action_fill (action, "search", &cli_search, COMMAND_REQ_CONNECTION, flags,
	                     _("[-o <prop[,...]>] [-l <prop[,...]>] <pattern>"),
	                     _("Search and print all media matching the pattern."));
}

void
cli_list_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "playlist",   'p', 0, G_OPTION_ARG_STRING, NULL, _("List the given playlist."), "name" },
		{ NULL }
	};
	command_action_fill (action, "list", &cli_list, COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE, flags,
	                     _("[-p <name>] <pattern|position>"),
	                     _("List the contents of a playlist (the active playlist by default). If a\n"
	                       "pattern is provided, contents are further filtered and only the matching\n"
	                       "media are displayed."));
}

void
cli_add_setup (command_action_t *action)
{
	/* FIXME: support collection ? */
	const argument_t flags[] = {
		{ "file", 'f', 0, G_OPTION_ARG_NONE, NULL, _("Treat the arguments as file paths instead of a pattern."), "path" },
		{ "pls", 'P', 0, G_OPTION_ARG_NONE, NULL, _("Treat the files as playlist files (implies --file.)"), "path" },
		{ "pattern", 't', 0, G_OPTION_ARG_NONE, NULL, _("Force treating arguments as pattern."), "pattern" },
		{ "non-recursive", 'N', 0, G_OPTION_ARG_NONE, NULL, _("Do not add directories recursively."), NULL },
		{ "playlist", 'p', 0, G_OPTION_ARG_STRING, NULL, _("Add to the given playlist."), "name" },
		{ "next", 'n', 0, G_OPTION_ARG_NONE, NULL, _("Add after the current track."), NULL },
		{ "at", 'a', 0, G_OPTION_ARG_INT, NULL, _("Add media at a given position in the playlist, or at a given offset from the current track."), "pos|offset" },
		{ "attribute", 'A', 0, G_OPTION_ARG_STRING_ARRAY, NULL, _("Add media with given key=value attribute(s)."), NULL },
		{ "order", 'o', 0, G_OPTION_ARG_STRING, NULL, _("Order media by specified properties."), NULL },
		{ NULL }
	};
	command_action_fill (action, "add", &cli_add, COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE, flags,
	                     _("[-t | -f [-N] [-P] [-A key=value]... ] [-p <playlist>] [-n | -a <pos|offset>] [pattern | paths] -o prop[,...]"),
	                     _("Add the matching media or files to a playlist."));
}

void
cli_remove_setup (command_action_t *action)
{
	/* FIXME: support collection ? */
	const argument_t flags[] = {
		{ "playlist", 'p', 0, G_OPTION_ARG_STRING, NULL, _("Remove from the given playlist, instead of the active playlist."), "name" },
		{ NULL }
	};
	command_action_fill (action, "remove", &cli_remove, COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE, flags,
	                     _("[-p <playlist>] <pattern|positions>"),
	                     _("Remove the matching media from a playlist."));
}

void
cli_move_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "playlist", 'p', 0, G_OPTION_ARG_STRING, NULL, _("Playlist to act on."), "name" },
		{ "next", 'n', 0, G_OPTION_ARG_NONE, NULL, _("Move the matching tracks after the current track."), NULL },
		{ "at", 'a', 0, G_OPTION_ARG_INT, NULL, _("Move the matching tracks by an offset or to a position."), "pos|offset"},
		{ NULL }
	};
	command_action_fill (action, "move", &cli_move, COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE, flags,
	                     _("[-p <playlist>] [-n | -a <pos|offset>] <pattern|positions>"),
	                     _("Move entries inside a playlist."));
}

void
cli_current_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "refresh", 'r', 0, G_OPTION_ARG_INT, NULL, _("Delay between each refresh of the status. If 0, the status is only printed once (default)."), "time" },
		{ "format",  'f', 0, G_OPTION_ARG_STRING, NULL, _("Format string used to display status."), "format" },
		{ NULL }
	};
	command_action_fill (action, "current", &cli_current, COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE, flags,
	                     _("[-r <time>] [-f <format>]"),
	                     _("Display current playback status, either continuously or once."));
}

void
cli_pl_create_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "switch", 's', 0, G_OPTION_ARG_NONE, NULL, _("Switch to the newly created playlist."), NULL },
		{ "playlist", 'p', 0, G_OPTION_ARG_STRING, NULL, _("Copy the content of the playlist into the new playlist."), "name" },
		{ NULL }
	};
	command_action_fill (action, "playlist create", &cli_pl_create, COMMAND_REQ_CONNECTION, flags,
	                     _("[-s] [-p <playlist>] <name>"),
	                     _("Create a new playlist."));
}

void
cli_pl_rename_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "force", 'f', 0, G_OPTION_ARG_NONE, NULL, _("Force the rename of the collection, overwrite an existing collection if needed."), NULL },
		{ "playlist", 'p', 0, G_OPTION_ARG_STRING, NULL, _("Rename the given playlist."), "name" },
		{ NULL }
	};
	command_action_fill (action, "playlist rename", &cli_pl_rename, COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE, flags,
	                     _("[-f] [-p <playlist>] <newname>"),
	                     _("Rename a playlist.  By default, rename the active playlist."));
}

void
cli_pl_sort_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "playlist", 'p', 0, G_OPTION_ARG_STRING, NULL, _("Rename the given playlist."), "name" },
		{ NULL }
	};
	command_action_fill (action, "playlist sort", &cli_pl_sort, COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE, flags,
	                     _("[-p <playlist>] [prop] ..."),
	                     _("Sort a playlist by a list of properties.  By default, sort the active playlist.\n"
	                        "To sort by a property in reverse, prefix its name by a '-'."));
}

void
cli_coll_create_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "force", 'f', 0, G_OPTION_ARG_NONE, NULL, _("Force creating of the collection, overwrite an existing collection if needed."), NULL},
		{ "collection", 'c', 0, G_OPTION_ARG_STRING, NULL, _("Copy an existing collection to the new one."), "name"},
		{ "empty", 'e', 0, G_OPTION_ARG_NONE, NULL, _("Initialize an empty collection."), NULL},
		{ NULL }
	};
	command_action_fill (action, "collection create", &cli_coll_create, COMMAND_REQ_CONNECTION, flags,
	                     _("[-f] [-e] [-c <collection>] <name> [pattern]"),
	                     _("Create a new collection.\nIf pattern is provided, it is used to define the collection."
	                       "\nOtherwise, the new collection contains the whole media library."));
}

void
cli_coll_rename_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "force", 'f', 0, G_OPTION_ARG_NONE, NULL, _("Force renaming of the collection, overwrite an existing collection if needed."), NULL},
		{ NULL }
	};
	command_action_fill (action, "collection rename", &cli_coll_rename, COMMAND_REQ_CONNECTION, flags,
	                     _("[-f] <oldname> <newname>"),
	                     _("Rename a collection."));
}

void
cli_pl_config_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "type",    't', 0, G_OPTION_ARG_STRING, NULL, _("Change the type of the playlist: list, queue, pshuffle."), "type" },
		{ "history", 's', 0, G_OPTION_ARG_INT, NULL, _("Size of the history of played tracks (for queue, pshuffle)."), "n" },
		{ "upcoming",'u', 0, G_OPTION_ARG_INT, NULL, _("Number of upcoming tracks to maintain (for pshuffle)."), "n" },
		{ "input",   'i', 0, G_OPTION_ARG_STRING, NULL, _("Input collection for the playlist (for pshuffle). Default to 'All Media'."), "coll" },
		{ "jumplist",'j', 0, G_OPTION_ARG_STRING, NULL, _("Jump to another playlist when the end of the playlist is reached."), "playlist"},
		{ NULL }
	};
	command_action_fill (action, "playlist config", &cli_pl_config, COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE, flags,
	                     _("[-t <type>] [-s <history>] [-u <upcoming>] [-i <coll>] [-j <playlist>] [playlist]"),
	                     _("Configure a playlist by changing its type, attributes, etc.\nBy default, configure the active playlist."));
}

void
cli_pl_list_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "all",  'a', 0, G_OPTION_ARG_NONE, NULL, _("Include hidden playlists."), NULL },
		{ NULL }
	};
	command_action_fill (action, "playlist list", &cli_pl_list, COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE, flags,
	                     _("[-a]"),
	                     _("List all playlists."));
}

void
cli_server_import_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "non-recursive", 'N',  0, G_OPTION_ARG_NONE, NULL, _("Do not import directories recursively."), NULL },
		{ NULL }
	};
	command_action_fill (action, "server import", &cli_server_import, COMMAND_REQ_CONNECTION, flags,
	                     _("[-N] <path>"),
	                     _("Import new files into the media library.\n"
	                     "By default, directories are imported recursively."));
}

void
cli_server_property_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "int",    'i',  0, G_OPTION_ARG_NONE, NULL, _("Force the value to be treated as integer."), NULL },
		{ "string", 's',  0, G_OPTION_ARG_NONE, NULL, _("Force the value to be treated as a string."), NULL },
		{ "delete", 'D',  0, G_OPTION_ARG_NONE, NULL, _("Delete the selected property."), NULL },
		{ "source", 'S',  0, G_OPTION_ARG_STRING, NULL, _("Property source."), NULL },
		{ NULL }
	};
	command_action_fill (action, "server property", &cli_server_property, COMMAND_REQ_CONNECTION | COMMAND_REQ_CACHE, flags,
	                     _("[-i | -s | -D] [-S] <mid> [name [value]]"),
	                     _("Get or set properties for a given media.\n"
	                     "If no name or value is provided, list all properties.\n"
	                     "If only a name is provided, display the value of the property.\n"
	                     "If both a name and a value are provided, set the new value of the property.\n\n"
	                     "By default, set operations use source \"client/" CLI_CLIENTNAME "\", while list and display operations use source-preference.\n"
	                     "Use the --source option to override this behaviour.\n\n"
	                     "By default, the value will be used to determine whether it should be saved as a string or an integer.\n"
	                     "Use the --int or --string flag to override this behaviour."));
}

void
cli_server_volume_setup (command_action_t *action)
{
	const argument_t flags[] = {
		{ "channel", 'c',  0, G_OPTION_ARG_STRING, NULL, _("Get or set the volume only for one channel."), "name" },
		{ NULL }
	};
	command_action_fill (action, "server volume", &cli_server_volume, COMMAND_REQ_CONNECTION, flags,
	                     _("[-c <name>] [value]"),
	                     _("Get or set the audio volume (in a range of 0-100).\n"
	                     "If a value is provided, set the new value of the volume. Otherwise, display the current volume.\n"
	                     "By default, the command applies to all audio channels. Use the --channel flag to override this behaviour."));
}

static void
fill_column_display (cli_infos_t *infos, column_display_t *disp,
                     const gchar **columns)
{
	gint i;
	const gchar *nextsep = NULL;

	for (i = 0; columns[i]; ++i) {
		/* Separator between columns */
		if (nextsep) {
			column_display_add_separator (disp, nextsep);
		}
		nextsep = "| ";

		/* FIXME: Allow flags to change alignment */

		if (strcmp (columns[i], "id") == 0) {
			column_display_add_property (disp, columns[i], columns[i], 5,
			                             COLUMN_DEF_SIZE_FIXED,
			                             COLUMN_DEF_ALIGN_LEFT);
		} else if (strcmp (columns[i], "pos") == 0) {
			column_display_add_special (disp, "pos", NULL, 5,
			                            COLUMN_DEF_SIZE_FIXED,
			                            COLUMN_DEF_ALIGN_RIGHT,
			                            column_display_render_position);
			nextsep = "/";
		} else if (strcmp (columns[i], "curr") == 0) {
			column_display_add_special (disp, "",
			                            GINT_TO_POINTER(infos->cache->currpos),
			                            2,
			                            COLUMN_DEF_SIZE_FIXED,
			                            COLUMN_DEF_ALIGN_LEFT,
			                            column_display_render_highlight);
			nextsep = NULL;
		} else if (strcmp (columns[i], "next") == 0) {
			int currpos = infos->cache->currpos;
			/* If no currpos, start counting from the beginning */
			if (currpos < 0) {
				currpos = 0;
			}
			column_display_add_special (disp, "next",
			                            GINT_TO_POINTER(currpos), 4,
			                            COLUMN_DEF_SIZE_FIXED,
			                            COLUMN_DEF_ALIGN_RIGHT,
			                            column_display_render_next);
		} else {
			column_display_add_property (disp, columns[i], columns[i], 20,
			                             COLUMN_DEF_SIZE_RELATIVE,
			                             COLUMN_DEF_ALIGN_LEFT);
		}
	}
}

static column_display_t *
create_column_display (cli_infos_t *infos, command_context_t *ctx,
                       const gchar **default_columns)
{
	const gchar **columns = NULL;
	column_display_t *coldisp;

	coldisp = column_display_init (infos);

	column_display_set_list_marker (coldisp,
	                                configuration_get_string (infos->config,
	                                                          "PLAYLIST_MARKER"));

	command_flag_stringlist_get (ctx, "columns", &columns);
	if (columns) {
		fill_column_display (infos, coldisp, columns);
	} else {
		fill_column_display (infos, coldisp, default_columns);
	}

	g_free (columns);

	return coldisp;
}

static column_display_t *
create_list_column_display (cli_infos_t *infos)
{
	column_display_t *coldisp;
	const gchar *format;

	format = configuration_get_string (infos->config, "CLASSIC_LIST_FORMAT");

	/* FIXME: compute field size dynamically instead of hardcoding maxlen? */

	coldisp = column_display_init (infos);

	column_display_set_list_marker (coldisp,
	                                configuration_get_string (infos->config,
	                                                          "PLAYLIST_MARKER"));

	column_display_add_special (coldisp, "",
	                            GINT_TO_POINTER(infos->cache->currpos), 2,
	                            COLUMN_DEF_SIZE_FIXED,
	                            COLUMN_DEF_ALIGN_LEFT,
	                            column_display_render_highlight);
	column_display_add_separator (coldisp, "[");
	column_display_add_special (coldisp, "pos", NULL, 0,
	                            COLUMN_DEF_SIZE_AUTO,
	                            COLUMN_DEF_ALIGN_RIGHT,
	                            column_display_render_position);
	column_display_add_separator (coldisp, "/");
	column_display_add_property (coldisp, "id", "id", 0,
	                             COLUMN_DEF_SIZE_AUTO,
	                             COLUMN_DEF_ALIGN_LEFT);
	column_display_add_separator (coldisp, "] ");

	column_display_add_format (coldisp, "tracks", format, 0,
	                           COLUMN_DEF_SIZE_AUTO,
	                           COLUMN_DEF_ALIGN_LEFT);

	/* FIXME: making duration part of the format would require proper
	 * rendering of duration in xmmsv_dict_format and conditional
	 * expressions to the parentheses if no duration is present. */

	/* FIXME: if time takes 6 chars, the display will exceed termwidth.. */
	column_display_add_separator (coldisp, " (");
	column_display_add_special (coldisp, "duration", (gpointer) "duration", 5,
	                            COLUMN_DEF_SIZE_FIXED,
	                            COLUMN_DEF_ALIGN_LEFT,
	                            column_display_render_time);
	column_display_add_separator (coldisp, ")");

	return coldisp;
}


/* Define commands */

gboolean
cli_play (cli_infos_t *infos, command_context_t *ctx)
{
	playback_play (infos);
	return FALSE;
}

gboolean
cli_pause (cli_infos_t *infos, command_context_t *ctx)
{
	playback_pause (infos);
	return FALSE;
}

gboolean
cli_toggle (cli_infos_t *infos, command_context_t *ctx)
{
	playback_toggle (infos);
	return FALSE;
}

gboolean
cli_stop (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;

	res = xmmsc_playback_stop (infos->sync);
	xmmsc_result_wait (res);
	done (res, infos);

	return FALSE;
}

gboolean
cli_seek (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	command_arg_time_t t;

	if (command_arg_time_get (ctx, 0, &t)) {
		if (t.type == COMMAND_ARG_TIME_OFFSET) {
			res = xmmsc_playback_seek_ms (infos->sync, t.value.offset * 1000, XMMS_PLAYBACK_SEEK_CUR);
		} else {
			res = xmmsc_playback_seek_ms (infos->sync, t.value.pos * 1000, XMMS_PLAYBACK_SEEK_SET);
		}

		xmmsc_result_wait (res);
		done (res, infos);
	} else {
		g_printf (_("Error: failed to parse the time argument!\n"));
	}

	return FALSE;
}

gboolean
cli_current (cli_infos_t *infos, command_context_t *ctx)
{
	const gchar *format;
	gint refresh;

	if (!command_flag_int_get (ctx, "refresh", &refresh)) {
		refresh = 0;
	}

	if (!command_flag_string_get (ctx, "format", &format)) {
		format = configuration_get_string (infos->config, "STATUS_FORMAT");
	}

	currently_playing_mode (infos, format, refresh);

	return refresh != 0; /* need I/O if we are refreshing */
}

gboolean
cli_prev (cli_infos_t *infos, command_context_t *ctx)
{
	gint n;
	gint offset = 1;

	if (command_arg_int_get (ctx, 0, &n)) {
		offset = n;
	}

	set_next_rel (infos, - offset);

	return FALSE;
}

gboolean
cli_next (cli_infos_t *infos, command_context_t *ctx)
{
	gint n;
	gint offset = 1;

	if (command_arg_int_get (ctx, 0, &n)) {
		offset = n;
	}

	set_next_rel (infos, offset);

	return FALSE;
}

gboolean
cli_jump (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	xmmsc_coll_t *query;
	gboolean backward = FALSE;
	playlist_positions_t *positions;

	command_flag_boolean_get (ctx, "backward", &backward);

	/* Select by positions */
	if (command_arg_positions_get (ctx, 0, &positions, infos->cache->currpos)) {
		position_jump (infos, positions);
		playlist_positions_free (positions);

	/* Select by pattern */
	} else if (command_arg_pattern_get (ctx, 0, &query, TRUE)) {
		/* FIXME: benchmark if efficient to reduce query to Active playlist */
		res = xmmsc_coll_query_ids (infos->sync, query, NULL, 0, 0);
		xmmsc_result_wait (res);
		if (backward) {
			list_jump_back (res, infos);
		} else {
			list_jump (res, infos);
		}
		xmmsc_coll_unref (query);
	}

	return FALSE;
}

gboolean
cli_search (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_coll_t *query;
	xmmsc_result_t *res;
	xmmsv_t *fetchval;
	column_display_t *coldisp;
	const gchar **order = NULL;
	const gchar **columns = NULL;
	const gchar *default_columns[] = { "id", "artist", "album", "title", NULL };

	if (!command_arg_pattern_get (ctx, 0, &query, TRUE)) {
		return FALSE;
	}

	coldisp = create_column_display (infos, ctx, default_columns);

	command_flag_stringlist_get (ctx, "columns", &columns);
	if (columns) {
		fetchval = xmmsv_make_stringlist ((gchar **) columns, -1);
	} else {
		fetchval = xmmsv_make_stringlist ((gchar **) default_columns, -1);
	}

	/* If the user hasn't requested any special order, we assume ordering
	 * search result per artist, and then album. To make this result more
	 * readable we put the compilation albums at the end to get proper album
	 * grouping.
	 */
	if (!command_flag_stringlist_get (ctx, "order", &order)) {
		xmmsv_coll_t *compilation, *compilation_sorted;
		xmmsv_coll_t *regular, *regular_sorted;
		xmmsv_coll_t *complement, *concatenated;
		xmmsv_t *fields, *artist_order, *compilation_order, *regular_order;

		/* All various artists entries that match the user query. */
		compilation = xmmsv_coll_new (XMMS_COLLECTION_TYPE_MATCH);
		xmmsv_coll_add_operand (compilation, query);
		xmmsv_coll_attribute_set_string (compilation, "field", "compilation");
		xmmsv_coll_attribute_set_string (compilation, "value", "1");

		/* All entries that aren't various artists, or don't match the user query */
		complement = xmmsv_coll_new (XMMS_COLLECTION_TYPE_COMPLEMENT);
		xmmsv_coll_add_operand (complement, compilation);

		/* All entries that aren't various artists, and match the user query */
		regular = xmmsv_coll_new (XMMS_COLLECTION_TYPE_INTERSECTION);
		xmmsv_coll_add_operand (regular, query);
		xmmsv_coll_add_operand (regular, complement);
		xmmsv_coll_unref (complement);

		compilation_order = xmmsv_build_list (
			XMMSV_LIST_ENTRY_STR ("album"),
			XMMSV_LIST_ENTRY_STR ("partofset"),
			XMMSV_LIST_ENTRY_STR ("tracknr"),
			XMMSV_LIST_END);

		compilation_sorted = xmmsv_coll_add_order_operators (compilation,
		                                                     compilation_order);
		xmmsv_coll_unref (compilation);
		xmmsv_unref (compilation_order);

		fields = xmmsv_build_list (XMMSV_LIST_ENTRY_STR ("album_artist_sort"),
		                           XMMSV_LIST_ENTRY_STR ("album_artist"),
		                           XMMSV_LIST_ENTRY_STR ("artist_sort"),
		                           XMMSV_LIST_ENTRY_STR ("artist"),
		                           XMMSV_LIST_END);

		artist_order = xmmsv_build_dict (XMMSV_DICT_ENTRY_STR ("type", "value"),
		                                 XMMSV_DICT_ENTRY ("field", fields),
		                                 XMMSV_DICT_END);

		regular_order = xmmsv_build_list (
			XMMSV_LIST_ENTRY_STR ("album"),
			XMMSV_LIST_ENTRY_STR ("partofset"),
			XMMSV_LIST_ENTRY_STR ("tracknr"),
			XMMSV_LIST_END);

		xmmsv_list_insert (regular_order, 0, artist_order);
		xmmsv_unref (artist_order);

		regular_sorted = xmmsv_coll_add_order_operators (regular, regular_order);
		xmmsv_coll_unref (regular);
		xmmsv_unref (regular_order);

		concatenated = xmmsv_coll_new (XMMS_COLLECTION_TYPE_UNION);
		xmmsv_coll_add_operand (concatenated, regular_sorted);
		xmmsv_coll_unref (regular_sorted);
		xmmsv_coll_add_operand (concatenated, compilation_sorted);
		xmmsv_coll_unref (compilation_sorted);

		res = xmmsc_coll_query_infos (infos->sync, concatenated, NULL,
		                              0, 0, fetchval, NULL);
		xmmsv_coll_unref (concatenated);
	} else {
		xmmsv_t *orderval = xmmsv_make_stringlist ((gchar **) order, -1);
		res = xmmsc_coll_query_infos (infos->sync, query, orderval,
		                              0, 0, fetchval, NULL);
		xmmsv_unref (orderval);
	}

	xmmsc_result_wait (res);

	list_print_row (res, NULL, coldisp, TRUE, TRUE);

	xmmsv_unref (fetchval);
	xmmsc_coll_unref (query);

	g_free (order);
	g_free (columns);

	return FALSE;
}

gboolean
cli_list (cli_infos_t *infos, command_context_t *ctx)
{
	gchar *pattern = NULL;
	xmmsv_coll_t *query = NULL, *filter = NULL, *pl;
	xmmsc_result_t *res;
	column_display_t *coldisp;
	playlist_positions_t *positions;
	gint pos;
	const gchar *playlist = NULL;
	gboolean new_list, filter_by_pos = FALSE;
	const gchar *default_columns[] = { "curr", "pos", "id", "artist", "album",
	                                   "title", NULL };

	/* Default to active playlist (from cache) */
	command_flag_string_get (ctx, "playlist", &playlist);

	if (!playlist_currpos_get (infos, playlist, &pos)) {
		g_printf (_("Error: failed to get current position in playlist.\n"));
		return FALSE;
	}

	if (!playlist) {
		playlist = XMMS_ACTIVE_PLAYLIST;
	}

	/* Filter by positions */
	if (command_arg_positions_get (ctx, 0, &positions, pos)) {
		filter_by_pos = TRUE;

	/* Filter by pattern */
	} else if (command_arg_longstring_get_escaped (ctx, 0, &pattern)) {
		/* Check if is pattern */
		if (!xmmsv_coll_parse (pattern, &query)) {
			g_printf (_("Error: failed to parse the pattern!\n"));
			g_free (pattern);
			return FALSE;
		}
	}

	/* Has filter, retrieve ids from intersection */
	if (query != NULL) {
		pl = xmmsv_coll_new (XMMS_COLLECTION_TYPE_REFERENCE);
		xmmsv_coll_attribute_set_string (pl, "namespace", XMMS_COLLECTION_NS_PLAYLISTS);
		xmmsv_coll_attribute_set_string (pl, "reference", playlist);

		filter = xmmsv_coll_new (XMMS_COLLECTION_TYPE_INTERSECTION);
		xmmsv_coll_add_operand (filter, query);
		xmmsv_coll_add_operand (filter, pl);
	}

	new_list = !configuration_get_boolean (infos->config, "CLASSIC_LIST");
	if (new_list) {
		coldisp = create_column_display (infos, ctx, default_columns);
	} else {
		coldisp = create_list_column_display (infos);
	}

	res = xmmsc_playlist_list_entries (infos->sync, playlist);
	xmmsc_result_wait (res);

	if (filter_by_pos) {
		positions_print_list (res, positions, coldisp, new_list);
	} else {
		list_print_row (res, filter, coldisp, new_list, FALSE);
	}

	if (filter != NULL) {
		xmmsv_coll_unref (filter);
		xmmsv_coll_unref (query);
		xmmsv_coll_unref (pl);
	}

	if (filter_by_pos) {
		playlist_positions_free (positions);
	}

	g_free (pattern);
	return FALSE;
}

gboolean
cli_info (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_coll_t *query;
	xmmsc_result_t *res;
	playlist_positions_t *positions;

	/* Select by positions */
	if (command_arg_positions_get (ctx, 0, &positions, infos->cache->currpos)) {
		positions_print_info (infos, positions);
		playlist_positions_free (positions);

	/* Select by pattern */
	} else if (command_arg_pattern_get (ctx, 0, &query, FALSE)) {
		res = xmmsc_coll_query_ids (infos->sync, query, NULL, 0, 0);
		xmmsc_result_wait (res);
		list_print_info (res, infos);
		xmmsc_coll_unref (query);

	/* Default to current song */
	} else {
		guint id;
		id = infos->cache->currid;
		res = xmmsc_medialib_get_info (infos->sync, id);
		xmmsc_result_wait (res);
		print_property (infos, res, id, NULL, NULL);
	}

	return FALSE;
}

static xmmsv_coll_t *
get_coll (cli_infos_t *infos, const gchar *name, xmmsv_coll_namespace_t ns) {
	xmmsc_result_t *res;
	xmmsv_coll_t *coll;

	res = xmmsc_coll_get (infos->sync, name, ns);
	xmmsc_result_wait (res);

	if (!xmmsv_get_coll (xmmsc_result_get_value (res), &coll)) {
		g_printf (_("Error: Could not retrieve collection %s.\n"), name);
		coll = NULL;
	} else {
		xmmsv_coll_ref (coll);
	}
	xmmsc_result_unref (res);

	return coll;
}

/* Get current position in @playlist or in active playlist if
   @playlist == NULL. */
static gboolean
playlist_currpos_get (cli_infos_t *infos, const gchar *playlist, gint *pos)
{
	xmmsv_coll_t *coll;
	const gchar *str;

	if (playlist) {
		/* FIXME: Getting the whole playlist is a bit of overkill (at least
		          linear in the size of the playlist.), but I am not aware of a
		          more efficient IPC-method. */
		if ((coll = get_coll (infos, playlist, XMMS_COLLECTION_NS_PLAYLISTS))) {
			if (xmmsv_dict_entry_get_string (xmmsv_coll_attributes_get (coll),
		                                     "position", &str)) {
				*pos = strtol (str, NULL, 10);
			} else {
				*pos = -1;
			}
			xmmsv_coll_unref (coll);
		} else {
			return FALSE;
		}
	} else {
		*pos = infos->cache->currpos;
	}

	return TRUE;
}

/* Get length of @playlist or of active playlist if @playlist == NULL. */
static gboolean
playlist_length_get (cli_infos_t *infos, const gchar *playlist, gint *len)
{
	xmmsv_coll_t *coll;

	if (playlist) {
		/* FIXME: Getting the whole playlist is a bit of overkill (at least
		          linear in the size of the playlist.), but I am not aware of a
		          more efficient IPC-method. */
		if ((coll = get_coll (infos, playlist, XMMS_COLLECTION_NS_PLAYLISTS))) {
			*len = xmmsv_coll_idlist_get_size (coll);
			xmmsv_coll_unref (coll);
		} else {
			return FALSE;
		}
	} else {
		*len = infos->cache->active_playlist->len;
	}

	return TRUE;
}


static gboolean
cmd_flag_pos_get_playlist (cli_infos_t *infos, command_context_t *ctx,
                           gint *pos, const gchar *playlist)
{
	gboolean next;
	gint at;
	gboolean at_isset;
	gint tmp;

	command_flag_boolean_get (ctx, "next", &next);
	at_isset = command_flag_int_get (ctx, "at", &at);

	if (next && at_isset) {
		g_printf (_("Error: --next and --at are mutually exclusive!\n"));
		return FALSE;
	} else if (next) {
		playlist_currpos_get (infos, playlist, &tmp);
		if (tmp >= 0) {
			*pos = tmp + 1;
		} else {
			g_printf (_("Error: --next cannot be used if there is no "
			            "active track!\n"));
			return FALSE;
		}
	} else if (at_isset) {
		/* FIXME: handle relative values ? */
		/* beware: int vs uint */
		if (!playlist_length_get (infos, playlist, &tmp)) {
			return FALSE;
		}

		if (at == 0 || (at > 0 && at > tmp + 1)) {
			g_printf (_("Error: specified position is outside the playlist!\n"));
			return FALSE;
		} else {
			*pos = at - 1;  /* playlist ids start at 0 */
		}
	} else {
		/* No flag given, no position found! */
		return FALSE;
	}

	return TRUE;
}

static gboolean
cmd_flag_pos_get (cli_infos_t *infos, command_context_t *ctx, gint *pos)
{
	return cmd_flag_pos_get_playlist (infos, ctx, pos, NULL);
}

static gchar *
encode_url (gchar *url)
{
	static const gchar hex[16] = "0123456789abcdef";
	gint i = 0, j = 0;
	gchar *res;

	res = g_new0 (gchar, strlen (url) * 3 + 1);
	if (!res)
		return NULL;

	for (i = 0; url[i]; i++) {
		guchar chr = url[i];
		if (GOODCHAR (chr)) {
			res[j++] = chr;
		} else if (chr == ' ') {
			res[j++] = '+';
		} else {
			res[j++] = '%';
			res[j++] = hex[((chr & 0xf0) >> 4)];
			res[j++] = hex[(chr & 0x0f)];
		}
	}

	return res;
}

static gboolean
guesspls (cli_infos_t *infos, const gchar *url)
{
	if (!configuration_get_boolean (infos->config, "GUESS_PLS")) {
		return FALSE;
	}

	if (g_str_has_suffix (url, ".m3u") || g_str_has_suffix (url, ".pls")) {
		return TRUE;
	}

	return FALSE;
}

static gboolean
guessfile (const gchar *pattern)
{
	char *p;
	struct stat filestat;

	/* if matches a local path, it's probably a file */
	if (stat (pattern, &filestat) == 0 &&
	    (S_ISREG(filestat.st_mode) || S_ISDIR(filestat.st_mode))) {
		return TRUE;
	}

	p = strpbrk (pattern, ":/~");

	if (!p) {
		/* Doesn't contain any of the chars above, not a file? */
		return FALSE;
	} else if (p[0] == ':' && p[1] == '/' && p[2] == '/') {
		/* Contains '://', likely a URL */
		return TRUE;
	} else if (p == pattern && p[0] == '/') {
		/* Starts with '/', should be an absolute path */
		return TRUE;
	} else if (p == pattern && p[0] == '~' && strchr (p, '/')) {
		/* Starts with '~' and contains '/', should be a home path */
		return TRUE;
	}

	return FALSE;
}

/**
 * Build a dict out of a number of key=value strings.
 *
 * @return a dict with 0..n attributes.
 */
static xmmsv_t *
cli_add_parse_attributes (command_context_t *ctx)
{
	const gchar **attributes;
	xmmsv_t *result;
	gint i;

	result = xmmsv_new_dict ();

	if (command_flag_stringarray_get (ctx, "attribute", &attributes)) {
		for (i = 0; attributes[i] != NULL; i++) {
			gchar **parts = g_strsplit (attributes[i], "=", 2);
			if (parts[0] != NULL && parts[1] != NULL) {
				xmmsv_dict_set_string (result, parts[0], parts[1]);
			}
			g_strfreev (parts);
		}
	}

	return result;
}

gboolean
cli_add (cli_infos_t *infos, command_context_t *ctx)
{
	gchar *pattern = NULL;
	const gchar *sortby = NULL;
	gchar **properties;
	const gchar *playlist;
	xmmsc_coll_t *query;
	xmmsc_result_t *res;
	xmmsv_t *attributes;
	xmmsv_t *order = NULL;
	gint pos;
	const gchar *path;
	gboolean fileargs;
	gboolean norecurs;
	gboolean plsfile;
	gboolean forceptrn;
	gint i, count;

/*
--file  Add a path from the local filesystem
--non-recursive  Do not add directories recursively.
--playlist  Add to the given playlist.
--next  Add after the current track.
--at  Add media at a given position in the playlist, or at a given offset from the current track.
--order Order media matched by pattern.
*/
	attributes = cli_add_parse_attributes (ctx);

	/* FIXME: offsets not supported (need to identify positive offsets) :-( */
	if (command_flag_string_get (ctx, "playlist", &playlist)) {
		if (!cmd_flag_pos_get_playlist (infos, ctx, &pos, playlist)) {
			/* append by default */
			playlist_length_get (infos, playlist, &pos);
		}
	} else {
		if (!cmd_flag_pos_get (infos, ctx, &pos)) {
			playlist_length_get (infos, NULL, &pos);
		}
		playlist = XMMS_ACTIVE_PLAYLIST;
	}

	command_flag_boolean_get (ctx, "pattern", &forceptrn);
	command_flag_boolean_get (ctx, "pls", &plsfile);
	command_flag_boolean_get (ctx, "file", &fileargs);
	command_flag_boolean_get (ctx, "non-recursive", &norecurs);
	command_arg_longstring_get_escaped (ctx, 0, &pattern);

	command_flag_string_get (ctx, "order", &sortby);
	if (sortby) {
		properties = g_strsplit (sortby, ",", MAX_STRINGLIST_TOKENS);
		order = xmmsv_make_stringlist (properties, -1);

		g_strfreev (properties);
	}

	if (forceptrn && (plsfile || fileargs)) {
		g_printf (_("Error: --pattern is mutually exclusive with "
		            "--file and --pls!\n"));
		goto finish;
	}

	/* We need either a file or a pattern! */
	if (!pattern) {
		g_printf (_("Error: you must provide a pattern or files to add!\n"));
		goto finish;
	}

	fileargs = fileargs || plsfile;

	if (!forceptrn) {
		/* if any of the arguments is a valid path, we treat them all as files */
		for (i = 0, count = command_arg_count (ctx); !fileargs && i < count; ++i) {
			command_arg_string_get (ctx, i, &path);
			fileargs = fileargs || guessfile (path);
		}
	}

	if (fileargs) {
		for (i = 0, count = command_arg_count (ctx); i < count; ++i) {
			GList *files = NULL, *it;
			gchar *vpath, *enc;

			command_arg_string_get (ctx, i, &path);
			vpath = format_url (path, G_FILE_TEST_IS_REGULAR | G_FILE_TEST_IS_DIR);
			if (vpath == NULL) {
				g_printf (_("Warning: Skipping invalid url '%s'.\n"), path);
				continue;
			}

			enc = encode_url (vpath);
			files = matching_browse (infos->sync, enc);

			for (it = g_list_first (files); it != NULL; it = g_list_next (it)) {
				browse_entry_t *entry = it->data;
				const gchar *url;
				gboolean is_directory;

				browse_entry_get (entry, &url, &is_directory);

				if (plsfile || guesspls (infos, url)) {
					if (xmmsv_dict_get_size (attributes) > 0) {
						g_printf (_("Warning: Skipping attributes together with playlist."));
					}
					xmmsc_result_t *plsres;
					gchar *decoded = decode_url (url);

					plsres = xmmsc_coll_idlist_from_playlist_file (infos->sync,
					                                               decoded);
					xmmsc_result_wait (plsres);
					add_pls (plsres, infos, playlist, pos);
					g_free (decoded);
				} else if (norecurs || !is_directory) {
					gchar *decoded = decode_url (url);
					res = xmmsc_playlist_insert_full (infos->sync, playlist,
					                                  pos, decoded, attributes);
					xmmsc_result_wait (res);
					xmmsc_result_unref (res);
					g_free (decoded);
				} else {
					if (xmmsv_dict_get_size (attributes) > 0) {
						g_printf (_("Warning: Skipping attributes together with playlist."));
					}
					res = xmmsc_playlist_rinsert_encoded (infos->sync, playlist,
					                                      pos, url);
					xmmsc_result_wait (res);
					xmmsc_result_unref (res);
				}
				pos++; /* next insert at next pos, to keep order */

				browse_entry_free (entry);
			}

			g_free (enc);
			g_free (vpath);
			g_list_free (files);
		}
	} else {
		if (xmmsv_dict_get_size (attributes) > 0) {
			g_printf (_("Warning: Skipping attributes together with pattern."));
			goto finish;
		}

		if (norecurs) {
			g_printf (_("Error:"
			            "--non-recursive only applies when passing --file!\n"));
			goto finish;
		}

		if (!xmmsc_coll_parse (pattern, &query)) {
			g_printf (_("Error: failed to parse the pattern!\n"));
			goto finish;
		} else {
			res = xmmsc_coll_query_ids (infos->sync, query, order, 0, 0);
			xmmsc_result_wait (res);
			add_list (res, infos, playlist, pos);
			xmmsc_coll_unref (query);
		}
	}

	finish:

	if (order) {
		xmmsv_unref (order);
	}

	xmmsv_unref (attributes);
	g_free (pattern);

	return FALSE;
}

gboolean
cli_remove (cli_infos_t *infos, command_context_t *ctx)
{
	const gchar *playlist = NULL;
	xmmsc_coll_t *query;
	xmmsc_result_t *res, *plres;
	playlist_positions_t *positions;

	command_flag_string_get (ctx, "playlist", &playlist);
	if (!playlist
	    || strcmp (playlist, infos->cache->active_playlist_name) == 0) {
		playlist = NULL;
	}

	/* Select by positions */
	if (command_arg_positions_get (ctx, 0, &positions, infos->cache->currpos)) {
		positions_remove (infos, playlist, positions);
		playlist_positions_free (positions);

	/* Select by pattern */
	} else if (command_arg_pattern_get (ctx, 0, &query, TRUE)) {
		res = xmmsc_coll_query_ids (infos->sync, query, NULL, 0, 0);
		xmmsc_result_wait (res);
		if (!playlist) {
			/* Optimize by reading active playlist from cache */
			remove_cached_list (res, infos);
		} else {
			plres = xmmsc_playlist_list_entries (infos->sync, playlist);
			xmmsc_result_wait (plres);
			remove_list (res, plres, infos, playlist);
		}
		xmmsc_coll_unref (query);
	}

	return FALSE;
}

/*
usage: move [-p <playlist>] [-n | -a <pos|offset>]  <pattern>

  Move entries inside a playlist.

Valid options:
  -p, --playlist  Playlist to act on.
  -n, --next      Move the matching tracks after the current track.
  -a, --at        Move the matching tracks by an offset or to a position.
  -h, --help      Display command help.
*/

gboolean
cli_move (cli_infos_t *infos, command_context_t *ctx)
{
	const gchar *playlist;
	gint pos;
	xmmsc_result_t *res;
	xmmsc_coll_t *query;
	playlist_positions_t *positions;

	if (!command_flag_string_get (ctx, "playlist", &playlist)) {
		playlist = NULL;
	}

	if (!cmd_flag_pos_get (infos, ctx, &pos)) {
		g_printf (_("Error: you must provide a position to move entries to!\n"));
		return FALSE;
	}

	/* Select by positions */
	if (command_arg_positions_get (ctx, 0, &positions, infos->cache->currpos)) {
		positions_move (infos, playlist, positions, pos);
		playlist_positions_free (positions);

	/* Select by pattern */
	} else if (command_arg_pattern_get (ctx, 0, &query, TRUE)) {
		res = xmmsc_coll_query_ids (infos->sync, query, NULL, 0, 0);
		xmmsc_result_wait (res);
		move_entries (res, infos, playlist, pos);
		xmmsc_coll_unref (query);
	}

	return FALSE;
}

gboolean
cli_pl_list (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	gboolean all;

	/* FIXME: support pattern argument (only display playlist containing matching media) */

	command_flag_boolean_get (ctx, "all", &all);

	res = xmmsc_playlist_list (infos->sync);
	xmmsc_result_wait (res);

	list_print_playlists (res, infos, all);

	return FALSE;
}

gboolean
cli_pl_switch (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	gchar *playlist;

	if (!command_arg_longstring_get (ctx, 0, &playlist)) {
		g_printf (_("Error: failed to read new playlist name!\n"));
		return FALSE;
	}

	res = xmmsc_playlist_load (infos->sync, playlist);
	xmmsc_result_wait (res);
	done (res, infos);

	g_free (playlist);

	return FALSE;
}

gboolean
cli_pl_create (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	gboolean switch_to;
	gchar *newplaylist;
	const gchar *copy;

	if (!command_arg_longstring_get (ctx, 0, &newplaylist)) {
		g_printf (_("Error: failed to read new playlist name!\n"));
		return FALSE;
	}

	if (!command_flag_boolean_get (ctx, "switch", &switch_to)) {
		switch_to = FALSE;
	}

	/* FIXME: Prevent overwriting existing playlists! */
	if (playlist_exists (infos, newplaylist)) {
		g_printf (_("Error: playlist %s already exists!\n"), newplaylist);
		return FALSE;
	}

	if (command_flag_string_get (ctx, "playlist", &copy)) {
		/* Copy the given playlist. */
		res = xmmsc_coll_get (infos->sync, copy, XMMS_COLLECTION_NS_PLAYLISTS);
		xmmsc_result_wait (res);
		copy_playlist (res, infos, newplaylist);
	} else {
		/* Simply create a new empty playlist */
		res = xmmsc_playlist_create (infos->sync, newplaylist);
		xmmsc_result_wait (res);
		done (res, infos);
	}

	if (switch_to) {
		res = xmmsc_playlist_load (infos->sync, newplaylist);
		xmmsc_result_wait (res);
		xmmsc_result_unref (res);
	}

	g_free (newplaylist);

	return FALSE;
}

gboolean
cli_pl_rename (cli_infos_t *infos, command_context_t *ctx)
{
/* 	xmmsc_result_t *res; */
	gboolean force;
	const gchar *oldname;
	gchar *newname;

	if (!command_flag_boolean_get (ctx, "force", &force)) {
		force = FALSE;
	}

	if (!command_arg_longstring_get (ctx, 0, &newname)) {
		g_printf (_("Error: failed to read new playlist name!\n"));
		return FALSE;
	}

	if (!command_flag_string_get (ctx, "playlist", &oldname)) {
		oldname = infos->cache->active_playlist_name;
	}

	coll_rename (infos, oldname, newname,
	             XMMS_COLLECTION_NS_PLAYLISTS, force);

	g_free (newname);

	return FALSE;
}

gboolean
cli_pl_remove (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	gchar *playlist;

	if (!command_arg_longstring_get (ctx, 0, &playlist)) {
		g_printf (_("Error: failed to read the playlist name!\n"));
		return FALSE;
	}

	/* Do not remove active playlist! */
	if (strcmp (playlist, infos->cache->active_playlist_name) == 0) {
		g_printf (_("Error: you cannot remove the active playlist!\n"));
		g_free (playlist);
		return FALSE;
	}

	res = xmmsc_playlist_remove (infos->sync, playlist);
	xmmsc_result_wait (res);
	done (res, infos);

	g_free (playlist);

	return FALSE;
}

gboolean
cli_pl_clear (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	gchar *playlist;

	if (!command_arg_longstring_get (ctx, 0, &playlist)) {
		playlist = g_strdup (infos->cache->active_playlist_name);
	}

	res = xmmsc_playlist_clear (infos->sync, playlist);
	xmmsc_result_wait (res);
	done (res, infos);

	g_free (playlist);

	return FALSE;
}

gboolean
cli_pl_shuffle (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	gchar *playlist;
	gboolean free_playlist = TRUE;

	if (!command_arg_longstring_get (ctx, 0, &playlist)) {
		playlist = infos->cache->active_playlist_name;
		free_playlist = FALSE;
	}

	res = xmmsc_playlist_shuffle (infos->sync, playlist);
	xmmsc_result_wait (res);
	done (res, infos);

	if (free_playlist)
		g_free (playlist);

	return FALSE;
}

gboolean
cli_pl_sort (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	xmmsv_t *orderval;
	const gchar *playlist;

	if (!command_flag_string_get (ctx, "playlist", &playlist)) {
		playlist = XMMS_ACTIVE_PLAYLIST;
	}

	if (command_arg_count (ctx) == 0) {
		orderval = xmmsv_new_list ();
		xmmsv_list_append_string (orderval, "artist");
		xmmsv_list_append_string (orderval, "album");
		xmmsv_list_append_string (orderval, "tracknr");
	} else {
		gchar **order = command_argv_get (ctx);
		orderval = xmmsv_make_stringlist (order, -1);
	}

	res = xmmsc_playlist_sort (infos->sync, playlist, orderval);
	xmmsc_result_wait (res);
	done (res, infos);

	xmmsv_unref (orderval);

	return FALSE;
}

gboolean
cli_pl_config (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	gchar *playlist;
	gint history, upcoming;
	gboolean modif = FALSE;
	const gchar *input, *jumplist, *typestr;
	xmmsv_coll_t *coll;
	xmmsv_t *val;

	history = -1;
	upcoming = -1;
	input = NULL;
	jumplist = NULL;

	/* Convert type string to type id */
	if (command_flag_string_get (ctx, "type", &typestr)) {
		modif = TRUE;
	} else {
		typestr = NULL;
	}

	if (command_flag_int_get (ctx, "history", &history)) {
		modif = TRUE;
	}
	if (command_flag_int_get (ctx, "upcoming", &upcoming)) {
		modif = TRUE;
	}

	/* FIXME: extract namespace too */
	if (command_flag_string_get (ctx, "input", &input)) {
		modif = TRUE;
	}

	if (command_flag_string_get (ctx, "jumplist", &jumplist)) {
		modif = TRUE;
	}

	if (!command_arg_longstring_get (ctx, 0, &playlist)) {
		playlist = infos->cache->active_playlist_name;
	}

	res = xmmsc_coll_get (infos->sync, playlist, XMMS_COLLECTION_NS_PLAYLISTS);
	xmmsc_result_wait (res);
	if (modif) {
		/* Send the previous coll_t for update. */
		if (typestr == NULL) {
			val = xmmsc_result_get_value (res);
			if (xmmsv_get_coll (val, &coll)) {
				xmmsv_coll_attribute_get_string (coll, "type", &typestr);
			} else {
				g_printf (_("Cannot find the playlist to configure!\n"));
				cli_infos_loop_resume (infos);
				xmmsc_result_unref (res);
			}
		}
		if (typestr != NULL) {
			configure_playlist (res, infos, playlist, history, upcoming,
			                    typestr, input, jumplist);
		}
	} else {
		/* Display current config of the playlist. */
		playlist_print_config (res, infos, playlist);
	}

	if (playlist != infos->cache->active_playlist_name) {
		g_free (playlist);
	}

	return FALSE;
}

/* Strings must be free manually */
static void
coll_name_split (const gchar *str, gchar **ns, gchar **name)
{
	gchar **v;

	v = g_strsplit (str, "/", 2);
	if (!v[0]) {
		*ns = NULL;
		*name = NULL;
	} else if (!v[1]) {
		*ns = g_strdup (XMMS_COLLECTION_NS_COLLECTIONS);
		*name = v[0];
	} else {
		*ns = v[0];
		*name = v[1];
	}

	g_free (v);
}

gboolean
cli_coll_list (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;

	res = xmmsc_coll_list (infos->sync, XMMS_COLLECTION_NS_COLLECTIONS);
	xmmsc_result_wait (res);
	list_print_collections (res, infos);

	return FALSE;
}

gboolean
cli_coll_show (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	gchar *collection, *name, *ns;

	if (!command_arg_longstring_get (ctx, 0, &collection)) {
		g_printf (_("Error: You must provide a collection!\n"));
		return FALSE;
	}

	coll_name_split (collection, &ns, &name);

	res = xmmsc_coll_get (infos->sync, name, ns);
	xmmsc_result_wait (res);
	coll_show (infos, res);

	g_free (ns);
	g_free (name);
    g_free (collection);

	return FALSE;
}

gboolean
cli_coll_create (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_coll_t *coll = NULL;
	gchar *ns, *name, *pattern = NULL;
	gboolean force = FALSE, empty = FALSE, copy;
	const gchar *collection, *fullname;

	if (!command_arg_string_get (ctx, 0, &fullname)) {
		g_printf (_("Error: You must provide a collection name!\n"));
		return FALSE;
	}

	command_flag_boolean_get (ctx, "empty", &empty);
	copy = command_flag_string_get (ctx, "collection", &collection);
	command_arg_longstring_get_escaped (ctx, 1, &pattern);

	if ((empty && copy) || (empty && pattern) || (copy && pattern)) {
		g_printf (_("Error: -e, -c and pattern are mutually exclusive!"));
		return FALSE;
	}

	command_flag_boolean_get (ctx, "force", &force);
	coll_name_split (fullname, &ns, &name);

	if (pattern) {
		if (!xmmsc_coll_parse (pattern, &coll)) {
			g_printf (_("Error: failed to parse the pattern!\n"));
		}
	} else if (copy) {
		xmmsc_result_t *res;
		xmmsv_t *val;
		gchar *from_ns, *from_name;

		coll_name_split (collection, &from_ns, &from_name);

		/* get collection to copy from */
		res = xmmsc_coll_get (infos->sync, from_name, from_ns);
		xmmsc_result_wait (res);
		val = xmmsc_result_get_value (res);

		if (xmmsv_get_coll (val, &coll)) {
			xmmsv_coll_ref (coll);
		} else {
			g_printf (_("Error: cannot find collection to copy\n"));
		}

		xmmsc_result_unref (res);
		g_free (from_ns);
		g_free (from_name);
	} else if (empty) {
		xmmsc_coll_t *univ;

		/* empty collection == NOT 'All Media' */
		univ = xmmsc_coll_universe ();

		coll = xmmsc_coll_new (XMMS_COLLECTION_TYPE_COMPLEMENT);
		xmmsc_coll_add_operand (coll, univ);
		xmmsc_coll_unref (univ);
	} else {
		coll = xmmsc_coll_universe ();
	}

	if (coll) {
		coll_save (infos, coll, ns, name, force);
		xmmsc_coll_unref (coll);
	}

	g_free (pattern);
	g_free (ns);
	g_free (name);

	return FALSE;
}

gboolean
cli_coll_rename (cli_infos_t *infos, command_context_t *ctx)
{
	gboolean force;
	gchar *from_ns, *to_ns, *from_name, *to_name;
	const gchar *oldname, *newname;

	if (!command_flag_boolean_get (ctx, "force", &force)) {
		force = FALSE;
	}

	if (!command_arg_string_get (ctx, 0, &oldname)) {
		g_printf (_("Error: failed to read collection name!\n"));
		return FALSE;
	}

	if (!command_arg_string_get (ctx, 1, &newname)) {
		g_printf (_("Error: failed to read collection new name!\n"));
		return FALSE;
	}

	coll_name_split (oldname, &from_ns, &from_name);
	coll_name_split (newname, &to_ns, &to_name);

	if (strcmp (from_ns, to_ns)) {
		g_printf ("Error: collections namespaces can't be different!\n");
	} else {
		coll_rename (infos, oldname, newname, to_ns, force);
	}

	g_free (from_ns);
	g_free (from_name);
	g_free (to_ns);
	g_free (to_name);

	return FALSE;
}

gboolean
cli_coll_remove (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	gchar *collection, *name, *ns;

	if (!command_arg_longstring_get (ctx, 0, &collection)) {
		g_printf (_("Error: failed to read the collection name!\n"));
		return FALSE;
	}

	coll_name_split (collection, &ns, &name);

	res = xmmsc_coll_remove (infos->sync, name, ns);
	xmmsc_result_wait (res);
	done (res, infos);

	g_free (ns);
	g_free (name);
    g_free (collection);

	return FALSE;
}

gboolean
cli_coll_config (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	gchar *name, *ns;
	const gchar *collection, *attrname, *attrvalue;

	if (!command_arg_string_get (ctx, 0, &collection)) {
		g_printf (_("Error: you must provide a collection!\n"));
		return FALSE;
	}

	coll_name_split (collection, &ns, &name);

	if (!command_arg_string_get (ctx, 1, &attrname)) {
		attrname = NULL;
		attrvalue = NULL;
	} else if (!command_arg_string_get (ctx, 2, &attrvalue)) {
		attrvalue = NULL;
	}

	res = xmmsc_coll_get (infos->sync, name, ns);
	xmmsc_result_wait (res);

	if (attrvalue) {
		configure_collection (res, infos, ns, name, attrname, attrvalue);
	} else {
		collection_print_config (res, infos, attrname);
	}

	g_free (ns);
	g_free (name);

	return FALSE;
}

gboolean
cli_server_import (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;

	gint i, count;
	const gchar *path;
	gboolean norecurs;

	if (!command_flag_boolean_get (ctx, "non-recursive", &norecurs)) {
		norecurs = FALSE;
	}

	for (i = 0, count = command_arg_count (ctx); i < count; ++i) {
		GList *files = NULL, *it;
		gchar *vpath, *enc;

		command_arg_string_get (ctx, i, &path);
		vpath = format_url (path, G_FILE_TEST_IS_REGULAR | G_FILE_TEST_IS_DIR);
		if (vpath == NULL) {
			g_printf (_ ("Warning: Skipping invalid url: '%s'"), path);
			continue;
		}

		enc = encode_url (vpath);
		files = matching_browse (infos->sync, enc);

		for (it = g_list_first (files); it != NULL; it = g_list_next (it)) {
			browse_entry_t *entry = it->data;
			const gchar *url;
			gboolean is_directory;

			browse_entry_get (entry, &url, &is_directory);

			if (norecurs || !is_directory) {
				res = xmmsc_medialib_add_entry_encoded (infos->sync,
				                                        url);
				xmmsc_result_unref (res);
			} else {
				res = xmmsc_medialib_import_path_encoded (infos->sync,
				                                          url);
				xmmsc_result_unref (res);
			}

			browse_entry_free (entry);
		}

		g_free (enc);
		g_free (vpath);
		g_list_free (files);
	}

	if (count == 0) {
		g_printf (_("Error: no path to import!\n"));
	}

	return FALSE;
}

gboolean
cli_server_browse (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsv_list_iter_t *it;
	xmmsc_result_t *res;
	xmmsv_t *value;
	const gchar *message, *url;

	if (!command_arg_string_get (ctx, 0, &url)) {
		return FALSE;
	}

	res = xmmsc_xform_media_browse (infos->sync, url);
	xmmsc_result_wait (res);

	value = xmmsc_result_get_value (res);

	if (xmmsv_get_error (value, &message)) {
		g_printf (_("Server error: %s\n"), message);
		xmmsc_result_unref (res);
		return FALSE;
	}

	xmmsv_get_list_iter (value, &it);

	while (xmmsv_list_iter_valid (it)) {
		const gchar *path;
		xmmsv_t *dict;
		gboolean isdir;

		xmmsv_list_iter_entry (it, &dict);

		/* Use realpath instead of path, good for playlists */
		if (!xmmsv_dict_entry_get_string (dict, "realpath", &path)) {
			if (!xmmsv_dict_entry_get_string (dict, "path", &path)) {
				/* broken data */
				continue;
			}
		}

		/* Append trailing slash to indicate directory */
		xmmsv_dict_entry_get_int (dict, "isdir", &isdir);

		g_print ("%s%c\n", path, isdir ? '/' : ' ');

		xmmsv_list_iter_next (it);
	}

	xmmsc_result_unref (res);
	return FALSE;
}

gboolean
cli_server_remove (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	xmmsc_coll_t *coll;

	gchar *pattern;

	if (!command_arg_longstring_get_escaped (ctx, 0, &pattern)) {
		g_printf (_("Error: you must provide a pattern!\n"));
		return FALSE;
	}

	if (!xmmsc_coll_parse (pattern, &coll)) {
		g_printf (_("Error: failed to parse the pattern!\n"));
		goto finish;
	}

	res = xmmsc_coll_query_ids (infos->sync, coll, NULL, 0, 0);
	xmmsc_result_wait (res);
	remove_ids (infos, res);

	finish:
	g_free (pattern);

	return FALSE;
}

gboolean
cli_server_rehash (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	xmmsc_coll_t *coll;

	gchar *pattern;

	if (command_arg_longstring_get_escaped (ctx, 0, &pattern)) {
		if (!xmmsc_coll_parse (pattern, &coll)) {
			g_printf (_("Error: failed to parse the pattern!\n"));
			goto finish;
		}

		res = xmmsc_coll_query_ids (infos->sync, coll, NULL, 0, 0);
		xmmsc_result_wait (res);
		rehash_ids (infos, res);
	} else {
		/* Rehash all media-library */
		res = xmmsc_medialib_rehash (infos->sync, 0);
		xmmsc_result_wait (res);
		done (res, infos);

		pattern = NULL;
	}

    finish:
	g_free (pattern);

	return FALSE;
}

gboolean
cli_server_config (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;
	const gchar *confname, *confval;

	if (!command_arg_string_get (ctx, 0, &confname)) {
		confname = NULL;
		confval = NULL;
	} else if (!command_arg_string_get (ctx, 1, &confval)) {
		confval = NULL;
	}

	if (confval) {
		res = xmmsc_config_set_value (infos->sync, confname, confval);
		xmmsc_result_wait (res);
		done (res, infos);
	} else {
		print_config (infos, confname);
	}

	return FALSE;
}

gboolean
cli_server_property (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;

	gint mid;
	gchar *default_source = NULL;
	gboolean delete, fint, fstring;
	const gchar *source, *propname, *propval;

	delete = fint = fstring = FALSE;

	command_flag_boolean_get (ctx, "delete", &delete);
	command_flag_boolean_get (ctx, "int", &fint);
	command_flag_boolean_get (ctx, "string", &fstring);

	if (delete && (fint || fstring)) {
		g_printf ("Error: --int and --string flags are invalid with --delete!\n");
		return FALSE;
	}

	if (fint && fstring) {
		g_printf ("Error: --int and --string flags are mutually exclusive!\n");
		return FALSE;
	}

	if (!command_arg_int_get (ctx, 0, &mid)) {
		g_printf ("Error: you must provide a media-id!\n");
		return FALSE;
	}

	default_source = g_strdup_printf ("client/%s", CLI_CLIENTNAME);

	if (!command_flag_string_get (ctx, "source", &source)) {
		source = default_source;
	}

	if (!command_arg_string_get (ctx, 1, &propname)) {
		propname = NULL;
		propval = NULL;
	} else if (!command_arg_string_get (ctx, 2, &propval)) {
		propval = NULL;
	}

	if (delete) {
		if (!propname) {
			g_printf (_("Error: you must provide a property to delete!\n"));
			goto finish;
		}
		res = xmmsc_medialib_entry_property_remove_with_source (infos->sync,
		                                                        mid,
		                                                        source,
		                                                        propname);
		xmmsc_result_wait (res);
		done (res, infos);
	} else if (!propval) {
		res = xmmsc_medialib_get_info (infos->sync, mid);
		xmmsc_result_wait (res);
		/* use source-preference when printing and user hasn't set --source */
		print_property (infos, res, mid,
		                source == default_source ? NULL : source,
		                propname);
	} else {
		gint value;
		gboolean cons;
		gchar *endptr;

		value = strtol (propval, &endptr, 0);

		/* determine save-type of the property */
		cons = endptr == '\0';
		fstring =  !cons & !fint;
		fint = cons | fint;

		if (fint) {
			res = xmmsc_medialib_entry_property_set_int_with_source (infos->sync,
			                                                         mid,
			                                                         source,
			                                                         propname,
			                                                         value);
		} else {
			res = xmmsc_medialib_entry_property_set_str_with_source (infos->sync,
			                                                         mid,
			                                                         source,
			                                                         propname,
			                                                         propval);
		}

		xmmsc_result_wait (res);
		done (res, infos);
	}

finish:
	g_free (default_source);

	return FALSE;
}

gboolean
cli_server_plugins (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;

	res = xmmsc_main_list_plugins (infos->sync, XMMS_PLUGIN_TYPE_ALL);
	xmmsc_result_wait (res);
	list_plugins (infos, res);

	return FALSE;
}

gboolean
cli_server_volume (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;

	const gchar *channel;
	gint volume;
	const gchar *volstr;
	bool relative_vol;

	if (!command_flag_string_get (ctx, "channel", &channel)) {
		channel = NULL;
	}

	if (!command_arg_int_get (ctx, 0, &volume)) {
		res = xmmsc_playback_volume_get (infos->sync);
		xmmsc_result_wait (res);
		print_volume (res, infos, channel);
	} else {
		if (command_arg_string_get (ctx, 0, &volstr)) {
			relative_vol = (volstr[0] == '+') || volume < 0;
		}
		if (relative_vol) {
			adjust_volume (infos, channel, volume);
		} else {
			set_volume (infos, channel, volume);
		}
	}

	return FALSE;
}

gboolean
cli_server_stats (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;

	res = xmmsc_main_stats (infos->sync);
	xmmsc_result_wait (res);

	print_stats (infos, res);

	return FALSE;
}

gboolean
cli_server_sync (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;

	res = xmmsc_coll_sync (infos->sync);
	xmmsc_result_wait (res);
	done (res, infos);

	return FALSE;
}

/* The loop is resumed in the disconnect callback */
gboolean
cli_server_shutdown (cli_infos_t *infos, command_context_t *ctx)
{
	xmmsc_result_t *res;

	if (infos->sync) {
		res = xmmsc_quit (infos->sync);
		xmmsc_result_wait (res);
		done (res, infos);
	}
	return FALSE;
}

gboolean
cli_exit (cli_infos_t *infos, command_context_t *ctx)
{
	cli_infos_loop_stop (infos);
	return FALSE;
}

static void
help_short_command (gpointer elem, gpointer udata)
{
	command_name_t *cmd = (command_name_t *)elem;
	/* FIXME: if contains space, change to <subcommand>, then allow 'help playlist' */
	g_printf ("   %s%s\n", cmd->name,
	          NULL_SUB (cmd->subcommands, "", " <subcommand>"));
}

static void
help_list_commands (GList *names,
                    const gchar *sing, const gchar *plur, const gchar *det)
{
	g_printf (_("usage: xmms2 <%s> [args]\n\n"), sing);
	g_printf (_("Available %s:\n"), plur);
	g_list_foreach (cmdnames_find (names, NULL),
	                help_short_command, NULL);
	g_printf (_("\nType 'help <%s>' for detailed help about %s %s.\n"),
	          sing, det, sing);
}

static void
help_list_subcommands (GList *names, gchar *cmd,
                       const gchar *sing, const gchar *plur, const gchar *det)
{
	gchar **cmdv = g_strsplit (cmd, " ", 0);
	g_printf (_("usage: xmms2 %s <sub%s> [args]\n\n"), cmd, sing);
	g_printf (_("Available '%s' sub%s:\n"), cmd, plur);
	g_list_foreach (cmdnames_find (names, cmdv),
	                help_short_command, NULL);
	g_printf (_("\nType 'help %s <sub%s>' for detailed help "
	            "about a sub%s.\n"), cmd, sing, sing);
	g_strfreev (cmdv);
}

static void
help_list (GList *names, gchar *cmd, cmd_type_t cmdtype)
{
	const gchar *cmdtxt_sing, *cmdtxt_plur, *cmdtxt_det;

	/* This is a bit tedious, english-- */
	switch (cmdtype) {
	case CMD_TYPE_ALIAS:
		cmdtxt_sing = _("alias");
		cmdtxt_plur = _("aliases");
		cmdtxt_det  = _("an");
		break;
	case CMD_TYPE_COMMAND:
	default:
		cmdtxt_sing = _("command");
		cmdtxt_plur = _("commands");
		cmdtxt_det  = _("a");
		break;
	}

	if (!cmd) {
		help_list_commands (names, cmdtxt_sing, cmdtxt_plur, cmdtxt_det);
	} else {
		help_list_subcommands (names, cmd, cmdtxt_sing, cmdtxt_plur, cmdtxt_det);
	}
}

void
help_command (cli_infos_t *infos, GList *cmdnames, gchar **cmd, gint num_args,
              cmd_type_t cmdtype)
{
	command_action_t *action;
	command_trie_match_type_t match;
	gint i, k;
	gint padding, max_flag_len = 0;

	gchar **argv = cmd;
	gint argc = num_args;
	gboolean auto_complete = configuration_get_boolean (infos->config,
	                                                    "AUTO_UNIQUE_COMPLETE");

	match = command_trie_find (infos->commands, &argv, &argc,
	                           auto_complete, &action, NULL);
	if (match == COMMAND_TRIE_MATCH_ACTION) {
		g_printf (_("usage: %s"), action->name);
		if (action->usage) {
			g_printf (" %s", action->usage);
		}
		g_printf ("\n\n");
		print_indented (action->description, COMMAND_HELP_DESCRIPTION_INDENT);
		g_printf ("\n\n");
		if (action->argdefs && action->argdefs[0].long_name) {
			/* Find length of longest option */
			for (i = 0; action->argdefs[i].long_name; ++i) {
				if (max_flag_len < strlen (action->argdefs[i].long_name)) {
					max_flag_len = strlen (action->argdefs[i].long_name);
				}
			}

			g_printf (_("Valid options:\n"));
			for (i = 0; action->argdefs[i].long_name; ++i) {
				padding = max_flag_len - strlen (action->argdefs[i].long_name) + 2;

				if (action->argdefs[i].short_name) {
					g_printf ("  -%c, ", action->argdefs[i].short_name);
				} else {
					g_printf ("      ");
				}

				g_printf ("--%s", action->argdefs[i].long_name);

				for (k = 0; k < padding; ++k) {
					g_printf (" ");
				}
				g_printf ("%s\n", action->argdefs[i].description);
				/* FIXME: align multi-line */
			}
		}
	} else if (match == COMMAND_TRIE_MATCH_SUBTRIE) {
		help_list (cmdnames, action->name, cmdtype);
	} else {
		/* FIXME: Better handle help for subcommands! */
		g_printf (_("Unknown command: '"));
		for (i = 0; i < num_args; ++i) {
			if (i > 0) g_printf (" ");
			g_printf ("%s", cmd[i]);
		}
		g_printf (_("'\n"));
		g_printf (_("Type 'help' for the list of commands.\n"));
	}
}

gboolean
cli_help (cli_infos_t *infos, command_context_t *ctx)
{
	gint num_args;
	gboolean alias;
	GList *names = infos->cmdnames;
	cmd_type_t cmdtype = CMD_TYPE_COMMAND;

	num_args = command_arg_count (ctx);

	if (command_flag_boolean_get (ctx, "alias", &alias) && alias) {
		names = infos->aliasnames;
		cmdtype = CMD_TYPE_ALIAS;
	}

	/* No argument, display the list of commands */
	if (num_args == 0) {
		help_list (names, NULL, cmdtype);
	} else {
		help_command (infos, names, command_argv_get (ctx), num_args, cmdtype);
	}

	/* No data pending */
	return FALSE;
}
