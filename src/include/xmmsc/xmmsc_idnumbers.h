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

#ifndef __SIGNAL_XMMS_H__
#define __SIGNAL_XMMS_H__

/* Don't forget to up this when protocol changes */
#define XMMS_IPC_PROTOCOL_VERSION 12

typedef enum {
	XMMSV_TYPE_NONE,
	XMMSV_TYPE_ERROR,
	XMMSV_TYPE_UINT32,
	XMMSV_TYPE_INT32,
	XMMSV_TYPE_STRING,
	XMMSV_TYPE_COLL,
	XMMSV_TYPE_BIN,
	XMMSV_TYPE_LIST,
	XMMSV_TYPE_DICT,
	XMMSV_TYPE_END
} xmmsv_type_t;

typedef enum {
	XMMS_IPC_OBJECT_MAIN,
	XMMS_IPC_OBJECT_PLAYLIST,
	XMMS_IPC_OBJECT_CONFIG,
	XMMS_IPC_OBJECT_OUTPUT,
	XMMS_IPC_OBJECT_MEDIALIB,
	XMMS_IPC_OBJECT_COLLECTION,
	XMMS_IPC_OBJECT_SIGNAL,
	XMMS_IPC_OBJECT_SERVICE,
	XMMS_IPC_OBJECT_VISUALISATION,
	XMMS_IPC_OBJECT_MEDIAINFO_READER,
	XMMS_IPC_OBJECT_XFORM,
	XMMS_IPC_OBJECT_BINDATA,
	XMMS_IPC_OBJECT_END
} xmms_ipc_objects_t;

typedef enum {
	XMMS_IPC_SIGNAL_OBJECT_DESTROYED,
	XMMS_IPC_SIGNAL_PLAYLIST_CHANGED,
	XMMS_IPC_SIGNAL_CONFIGVALUE_CHANGED,
	XMMS_IPC_SIGNAL_PLAYBACK_STATUS,
	XMMS_IPC_SIGNAL_OUTPUT_VOLUME_CHANGED,
	XMMS_IPC_SIGNAL_OUTPUT_PLAYTIME,
	XMMS_IPC_SIGNAL_OUTPUT_CURRENTID,
	XMMS_IPC_SIGNAL_OUTPUT_OPEN_FAIL,
	XMMS_IPC_SIGNAL_PLAYLIST_CURRENT_POS,
	XMMS_IPC_SIGNAL_PLAYLIST_LOADED,
	XMMS_IPC_SIGNAL_MEDIALIB_ENTRY_ADDED,
	XMMS_IPC_SIGNAL_MEDIALIB_ENTRY_UPDATE,
	XMMS_IPC_SIGNAL_COLLECTION_CHANGED,
	XMMS_IPC_SIGNAL_TRANSPORT_MIMETYPE,
	XMMS_IPC_SIGNAL_DECODER_THREAD_EXIT,
	XMMS_IPC_SIGNAL_VISUALISATION_DATA,
	XMMS_IPC_SIGNAL_QUIT,
	XMMS_IPC_SIGNAL_MEDIAINFO_READER_STATUS,
	XMMS_IPC_SIGNAL_MEDIAINFO_READER_UNINDEXED,
	XMMS_IPC_SIGNAL_SERVICE,
	XMMS_IPC_SIGNAL_SERVICE_CHANGED,
	XMMS_IPC_SIGNAL_SERVICE_SHUTDOWN,
	XMMS_IPC_SIGNAL_END
} xmms_ipc_signals_t;

typedef enum {
	/* Main */
	XMMS_IPC_CMD_HELLO,
	XMMS_IPC_CMD_QUIT,
	XMMS_IPC_CMD_REPLY,
	XMMS_IPC_CMD_ERROR,
	XMMS_IPC_CMD_PLUGIN_LIST,
	XMMS_IPC_CMD_STATS,

	/* Playlist */
	XMMS_IPC_CMD_SHUFFLE,
	XMMS_IPC_CMD_SET_POS,
	XMMS_IPC_CMD_SET_POS_REL,
	XMMS_IPC_CMD_ADD_URL,
	XMMS_IPC_CMD_ADD_ID,
	XMMS_IPC_CMD_ADD_IDLIST,
	XMMS_IPC_CMD_ADD_COLL,
	XMMS_IPC_CMD_REMOVE_ENTRY,
	XMMS_IPC_CMD_MOVE_ENTRY,
	XMMS_IPC_CMD_CLEAR,
	XMMS_IPC_CMD_SORT,
	XMMS_IPC_CMD_LIST,
	XMMS_IPC_CMD_CURRENT_POS,
	XMMS_IPC_CMD_CURRENT_ACTIVE,
	XMMS_IPC_CMD_INSERT_URL,
	XMMS_IPC_CMD_INSERT_ID,
	XMMS_IPC_CMD_INSERT_COLL,
	XMMS_IPC_CMD_LOAD,
	XMMS_IPC_CMD_RADD,

	/* Config */
	XMMS_IPC_CMD_GETVALUE,
	XMMS_IPC_CMD_SETVALUE,
	XMMS_IPC_CMD_REGVALUE,
	XMMS_IPC_CMD_LISTVALUES,

	/* output */
	XMMS_IPC_CMD_START,
	XMMS_IPC_CMD_STOP,
	XMMS_IPC_CMD_PAUSE,
	XMMS_IPC_CMD_DECODER_KILL,
	XMMS_IPC_CMD_CPLAYTIME,
	XMMS_IPC_CMD_SEEKMS,
	XMMS_IPC_CMD_SEEKMS_REL,
	XMMS_IPC_CMD_SEEKSAMPLES,
	XMMS_IPC_CMD_SEEKSAMPLES_REL,
	XMMS_IPC_CMD_OUTPUT_STATUS,
	XMMS_IPC_CMD_CURRENTID,
	XMMS_IPC_CMD_VOLUME_SET,
	XMMS_IPC_CMD_VOLUME_GET,

	/* Medialib */
	XMMS_IPC_CMD_INFO,
	XMMS_IPC_CMD_PATH_IMPORT,
	XMMS_IPC_CMD_REHASH,
	XMMS_IPC_CMD_GET_ID,
	XMMS_IPC_CMD_REMOVE_ID,
	XMMS_IPC_CMD_PROPERTY_SET_STR,
	XMMS_IPC_CMD_PROPERTY_SET_INT,
	XMMS_IPC_CMD_PROPERTY_REMOVE,
	XMMS_IPC_CMD_MOVE_URL,

	/* Collection */
	XMMS_IPC_CMD_COLLECTION_GET,
	XMMS_IPC_CMD_COLLECTION_LIST,
	XMMS_IPC_CMD_COLLECTION_SAVE,
	XMMS_IPC_CMD_COLLECTION_REMOVE,
	XMMS_IPC_CMD_COLLECTION_FIND,
	XMMS_IPC_CMD_COLLECTION_RENAME,
	XMMS_IPC_CMD_QUERY_IDS,
	XMMS_IPC_CMD_QUERY_INFOS,
	XMMS_IPC_CMD_IDLIST_FROM_PLS,
	XMMS_IPC_CMD_COLLECTION_SYNC,

	/* Service client */
	XMMS_IPC_CMD_SERVICE_REGISTER,
	XMMS_IPC_CMD_SERVICE_UNREGISTER,
	XMMS_IPC_CMD_SERVICE_LIST,
	XMMS_IPC_CMD_SERVICE_DESCRIBE,
	XMMS_IPC_CMD_SERVICE_REQUEST,
	XMMS_IPC_CMD_SERVICE_RETURN,
	XMMS_IPC_CMD_SERVICE_SHUTDOWN,

	/* Signal subsystem */
	XMMS_IPC_CMD_SIGNAL,
	XMMS_IPC_CMD_BROADCAST,

	/* xform object */
	XMMS_IPC_CMD_BROWSE,

	/* bindata object */
	XMMS_IPC_CMD_GET_DATA,
	XMMS_IPC_CMD_ADD_DATA,
	XMMS_IPC_CMD_REMOVE_DATA,
	XMMS_IPC_CMD_LIST_DATA,

	/* end */
	XMMS_IPC_CMD_END
} xmms_ipc_cmds_t;

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
	XMMS_SERVICE_CHANGED_REGISTER,
	XMMS_SERVICE_CHANGED_UNREGISTER
} xmms_service_changed_actions_t;

typedef enum {
	XMMS_PLAYBACK_STATUS_STOP,
	XMMS_PLAYBACK_STATUS_PLAY,
	XMMS_PLAYBACK_STATUS_PAUSE
} xmms_playback_status_t;

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
	XMMS_COLLECTION_TYPE_PARTYSHUFFLE
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

/* Service Clients */
#define XMMSC_SERVICE_CHANGE_TYPE              "type"
#define XMMSC_SERVICE_PROP_NAME                "name"
#define XMMSC_SERVICE_PROP_DESCRIPTION         "description"
#define XMMSC_SERVICE_PROP_MAJOR               "major"
#define XMMSC_SERVICE_PROP_MINOR               "minor"
#define XMMSC_SERVICE_PROP_METHODS             "methods"
#define XMMSC_SERVICE_METHOD_PROP_DESCRIPTION  "description"
#define XMMSC_SERVICE_METHOD_PROP_ARGUMENTS    "args"
#define XMMSC_SERVICE_METHOD_PROP_RETURN_TYPE  "rettype"
#define XMMSC_SERVICE_METHOD_PROP_COOKIE       "cookie"
#define XMMSC_SERVICE_METHOD_ARG_PROP_NAME     "name"
#define XMMSC_SERVICE_METHOD_ARG_PROP_TYPE     "type"
#define XMMSC_SERVICE_METHOD_ARG_PROP_OPTIONAL "optional"
#define XMMSC_SERVICE_METHOD_ARG_PROP_VALUE    "value"

#endif /* __SIGNAL_XMMS_H__ */
