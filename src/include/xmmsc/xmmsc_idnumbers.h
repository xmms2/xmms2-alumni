/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2009 XMMS2 Team
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

#ifndef __SIGNAL_XMMS_H__
#define __SIGNAL_XMMS_H__

/* Don't forget to up this when protocol changes */
#define XMMS_IPC_PROTOCOL_VERSION 18

#define XMMS_IPC_OBJECT_KEY "__object"
#define XMMS_IPC_METHOD_KEY "__method"
#define XMMS_IPC_ARGUMENTS_KEY "__arguments"
#define XMMS_IPC_ERROR_KEY "__error"
#define XMMS_IPC_RESULT_KEY "__result"

/* Objects */
#define XMMS_IPC_OBJECT_SIGNAL "signal"
#define XMMS_IPC_OBJECT_MAIN "main"
#define XMMS_IPC_OBJECT_PLAYLIST "playlist"
#define XMMS_IPC_OBJECT_CONFIG "config"
#define XMMS_IPC_OBJECT_PLAYBACK "playback"
#define XMMS_IPC_OBJECT_MEDIALIB "medialib"
#define XMMS_IPC_OBJECT_COLLECTION "collection"
#define XMMS_IPC_OBJECT_VISUALIZATION "visualization"
#define XMMS_IPC_OBJECT_MEDIAINFO_READER "mediainfo_reader"
#define XMMS_IPC_OBJECT_XFORM "xform"
#define XMMS_IPC_OBJECT_BINDATA "bindata"

/* Methods */
#define XMMS_IPC_SIGNAL_PLAYLIST_CHANGED "playlist_changed"
#define XMMS_IPC_SIGNAL_CONFIGVALUE_CHANGED "config"
#define XMMS_IPC_SIGNAL_PLAYBACK_STATUS "status"
#define XMMS_IPC_SIGNAL_PLAYBACK_VOLUME_CHANGED "volume_changed"
#define XMMS_IPC_SIGNAL_PLAYBACK_PLAYTIME "playtime"
#define XMMS_IPC_SIGNAL_PLAYBACK_CURRENTID "current_id"
#define XMMS_IPC_SIGNAL_PLAYLIST_CURRENT_POS "current_pos"
#define XMMS_IPC_SIGNAL_PLAYLIST_LOADED "playlist_loaded"
#define XMMS_IPC_SIGNAL_MEDIALIB_ENTRY_ADDED "entry_added"
#define XMMS_IPC_SIGNAL_MEDIALIB_ENTRY_UPDATE "entry_changed"
#define XMMS_IPC_SIGNAL_COLLECTION_CHANGED "changed"
#define XMMS_IPC_SIGNAL_QUIT "quit"
#define XMMS_IPC_SIGNAL_MEDIAINFO_READER_STATUS "status"
#define XMMS_IPC_SIGNAL_MEDIAINFO_READER_UNINDEXED "unindexed"

/* Signal subsystem methods */
#define XMMS_IPC_CMD_SIGNAL "signal"
#define XMMS_IPC_CMD_BROADCAST "broadcast"

/* Main methods */
#define XMMS_IPC_CMD_HELLO "hello"
#define XMMS_IPC_CMD_QUIT "quit"
#define XMMS_IPC_CMD_PLUGIN_LIST "list_plugins"
#define XMMS_IPC_CMD_STATS "stats"

/* Playlist methods */
#define XMMS_IPC_CMD_SHUFFLE "shuffle"
#define XMMS_IPC_CMD_SET_POS "set_next"
#define XMMS_IPC_CMD_SET_POS_REL "set_next_rel"
#define XMMS_IPC_CMD_ADD_URL "add_url"
#define XMMS_IPC_CMD_ADD_ID "add_id"
#define XMMS_IPC_CMD_ADD_IDLIST "add_idlist"
#define XMMS_IPC_CMD_ADD_COLL "add_coll"
#define XMMS_IPC_CMD_REMOVE_ENTRY "remove_entry"
#define XMMS_IPC_CMD_MOVE_ENTRY "move_entry"
#define XMMS_IPC_CMD_CLEAR "clear"
#define XMMS_IPC_CMD_SORT "sort"
#define XMMS_IPC_CMD_LIST "list_entries"
#define XMMS_IPC_CMD_CURRENT_POS "current_pos"
#define XMMS_IPC_CMD_CURRENT_ACTIVE "current_active"
#define XMMS_IPC_CMD_INSERT_URL "insert_url"
#define XMMS_IPC_CMD_INSERT_ID "insert_id"
#define XMMS_IPC_CMD_INSERT_COLL "insert_coll"
#define XMMS_IPC_CMD_LOAD "load"
#define XMMS_IPC_CMD_RADD "radd"
#define XMMS_IPC_CMD_RINSERT "rinsert"

/* Config methods */
#define XMMS_IPC_CMD_GETVALUE "get_value"
#define XMMS_IPC_CMD_SETVALUE "set_value"
#define XMMS_IPC_CMD_REGVALUE "register_value"
#define XMMS_IPC_CMD_LISTVALUES "list_values"

/* playback methods */
#define XMMS_IPC_CMD_START "start"
#define XMMS_IPC_CMD_STOP "stop"
#define XMMS_IPC_CMD_PAUSE "pause"
#define XMMS_IPC_CMD_DECODER_KILL "tickle"
#define XMMS_IPC_CMD_CPLAYTIME "playtime"
#define XMMS_IPC_CMD_SEEKMS "seek_ms"
#define XMMS_IPC_CMD_SEEKSAMPLES "seek_samples"
#define XMMS_IPC_CMD_PLAYBACK_STATUS "status"
#define XMMS_IPC_CMD_CURRENTID "current_id"
#define XMMS_IPC_CMD_VOLUME_SET "volume_set"
#define XMMS_IPC_CMD_VOLUME_GET "volume_get"

/* Medialib methods */
#define XMMS_IPC_CMD_INFO "get_info"
#define XMMS_IPC_CMD_PATH_IMPORT "import_path"
#define XMMS_IPC_CMD_REHASH "rehash"
#define XMMS_IPC_CMD_GET_ID "get_id"
#define XMMS_IPC_CMD_REMOVE_ID "remove_entry"
#define XMMS_IPC_CMD_PROPERTY_SET_STR "set_property_string"
#define XMMS_IPC_CMD_PROPERTY_SET_INT "set_property_int"
#define XMMS_IPC_CMD_PROPERTY_REMOVE "remove_property"
#define XMMS_IPC_CMD_MOVE_URL "move_entry"
#define XMMS_IPC_CMD_MLIB_ADD_URL "add_entry"

/* Collection methods */
#define XMMS_IPC_CMD_COLLECTION_GET "get"
#define XMMS_IPC_CMD_COLLECTION_LIST "list"
#define XMMS_IPC_CMD_COLLECTION_SAVE "save"
#define XMMS_IPC_CMD_COLLECTION_REMOVE "remove"
#define XMMS_IPC_CMD_COLLECTION_FIND "find"
#define XMMS_IPC_CMD_COLLECTION_RENAME "rename"
#define XMMS_IPC_CMD_QUERY_IDS "query_ids"
#define XMMS_IPC_CMD_QUERY_INFOS "query_infos"
#define XMMS_IPC_CMD_IDLIST_FROM_PLS "idlist_from_playlist"
#define XMMS_IPC_CMD_COLLECTION_SYNC "sync"

/* bindata methods */
#define XMMS_IPC_CMD_GET_DATA "retrieve"
#define XMMS_IPC_CMD_ADD_DATA "add"
#define XMMS_IPC_CMD_REMOVE_DATA "remove"
#define XMMS_IPC_CMD_LIST_DATA "list"

/* visualization methods */
#define XMMS_IPC_CMD_VISUALIZATION_QUERY_VERSION "query_version"
#define XMMS_IPC_CMD_VISUALIZATION_REGISTER "register"
#define XMMS_IPC_CMD_VISUALIZATION_INIT_SHM "init_shm"
#define XMMS_IPC_CMD_VISUALIZATION_INIT_UDP "init_udp"
#define XMMS_IPC_CMD_VISUALIZATION_PROPERTY "set_property"
#define XMMS_IPC_CMD_VISUALIZATION_PROPERTIES "set_properties"
#define XMMS_IPC_CMD_VISUALIZATION_SHUTDOWN "shutdown"

/* xform methods */
#define XMMS_IPC_CMD_BROWSE "browse"

typedef enum {
	XMMS_PLAYLIST_CHANGED_ADD,
	XMMS_PLAYLIST_CHANGED_INSERT,
	XMMS_PLAYLIST_CHANGED_SHUFFLE,
	XMMS_PLAYLIST_CHANGED_REMOVE,
	XMMS_PLAYLIST_CHANGED_CLEAR,
	XMMS_PLAYLIST_CHANGED_MOVE,
	XMMS_PLAYLIST_CHANGED_SORT,
	XMMS_PLAYLIST_CHANGED_UPDATE
} xmms_playlist_changed_actions_t;

typedef enum {
	XMMS_COLLECTION_CHANGED_ADD,
	XMMS_COLLECTION_CHANGED_UPDATE,
	XMMS_COLLECTION_CHANGED_RENAME,
	XMMS_COLLECTION_CHANGED_REMOVE
} xmms_collection_changed_actions_t;

typedef enum {
	XMMS_PLAYBACK_STATUS_STOP,
	XMMS_PLAYBACK_STATUS_PLAY,
	XMMS_PLAYBACK_STATUS_PAUSE
} xmms_playback_status_t;

typedef enum {
	XMMS_PLAYBACK_SEEK_CUR = 1,
	XMMS_PLAYBACK_SEEK_SET
} xmms_playback_seek_mode_t;

typedef enum {
	XMMS_MEDIAINFO_READER_STATUS_IDLE,
	XMMS_MEDIAINFO_READER_STATUS_RUNNING
} xmms_mediainfo_reader_status_t;

typedef enum {
	XMMS_PLUGIN_TYPE_ALL,
	XMMS_PLUGIN_TYPE_OUTPUT,
	XMMS_PLUGIN_TYPE_XFORM
} xmms_plugin_type_t;

typedef enum {
	XMMS_COLLECTION_TYPE_REFERENCE,
	XMMS_COLLECTION_TYPE_UNION,
	XMMS_COLLECTION_TYPE_INTERSECTION,
	XMMS_COLLECTION_TYPE_COMPLEMENT,
	XMMS_COLLECTION_TYPE_HAS,
	XMMS_COLLECTION_TYPE_EQUALS,
	XMMS_COLLECTION_TYPE_MATCH,
	XMMS_COLLECTION_TYPE_SMALLER,
	XMMS_COLLECTION_TYPE_GREATER,
	XMMS_COLLECTION_TYPE_IDLIST,
	XMMS_COLLECTION_TYPE_QUEUE,
	XMMS_COLLECTION_TYPE_PARTYSHUFFLE,
	XMMS_COLLECTION_TYPE_LAST = XMMS_COLLECTION_TYPE_PARTYSHUFFLE
} xmmsv_coll_type_t;

typedef enum {
	XMMS_MEDIALIB_ENTRY_STATUS_NEW,
	XMMS_MEDIALIB_ENTRY_STATUS_OK,
	XMMS_MEDIALIB_ENTRY_STATUS_RESOLVING,
	XMMS_MEDIALIB_ENTRY_STATUS_NOT_AVAILABLE,
	XMMS_MEDIALIB_ENTRY_STATUS_REHASH
} xmmsc_medialib_entry_status_t;

typedef const char* xmmsv_coll_namespace_t;
#define	XMMS_COLLECTION_NS_ALL          "*"
#define XMMS_COLLECTION_NS_COLLECTIONS  "Collections"
#define XMMS_COLLECTION_NS_PLAYLISTS    "Playlists"

#define XMMS_ACTIVE_PLAYLIST "_active"

/* Default source preferences for accessing "propdicts" (decl. in value.c) */
extern const char *default_source_pref[];

/* compability */
typedef xmmsv_coll_type_t xmmsc_coll_type_t;
typedef xmmsv_coll_namespace_t xmmsc_coll_namespace_t;


#endif /* __SIGNAL_XMMS_H__ */
