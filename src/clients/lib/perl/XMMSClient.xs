#include "perl_xmmsclient.h"

void
perl_xmmsclient_xmmsc_disconnect_callback_set_cb (void *userdata)
{
	PerlXMMSClientCallback *cb = (PerlXMMSClientCallback *)userdata;

	perl_xmmsclient_callback_invoke (cb);
}

void
perl_xmmsclient_xmmsc_io_need_out_callback_set_cb (int flag, void *userdata)
{
	PerlXMMSClientCallback *cb = (PerlXMMSClientCallback *)userdata;

	perl_xmmsclient_callback_invoke (cb);
}

MODULE = Audio::XMMSClient	PACKAGE = Audio::XMMSClient	PREFIX = xmmsc_


## General

SV *
new (class, clientname=NULL)
		const char *class
		const char *clientname
	PREINIT:
		xmmsc_connection_t *con;
	INIT:
		if (!clientname) {
			clientname = SvPV_nolen (get_sv ("0", 0));
		}
	CODE:
		con = xmmsc_init (clientname);

		if (con == NULL) {
			RETVAL = &PL_sv_undef;
		}
		else {
			RETVAL = perl_xmmsclient_new_sv_from_ptr (con, class);
		}
	OUTPUT:
		RETVAL

int
xmmsc_connect (c, ipcpath=NULL)
		xmmsc_connection_t *c
		const char *ipcpath
	PREINIT:
		char *xmms_path_env = NULL;
	INIT:
		if (ipcpath == NULL) {
			xmms_path_env = getenv ("XMMS_PATH");

			if (xmms_path_env != NULL)
				ipcpath = xmms_path_env;
		}
	OUTPUT:
		RETVAL

void
xmmsc_disconnect_callback_set (c, func, data=NULL)
		xmmsc_connection_t *c
		SV *func
		SV *data
	PREINIT:
		PerlXMMSClientCallback *cb = NULL;
	CODE:
		cb = perl_xmmsclient_callback_new (func, data, NULL, 0, NULL);

		xmmsc_disconnect_callback_set_full (c,
		                                    perl_xmmsclient_xmmsc_disconnect_callback_set_cb, cb,
		                                    (xmmsc_user_data_free_func_t)perl_xmmsclient_callback_destroy);

void
xmmsc_io_disconnect (c)
		xmmsc_connection_t *c

char *
xmmsc_get_last_error (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_plugin_list (c, type=XMMS_PLUGIN_TYPE_ALL)
		xmmsc_connection_t *c
		xmms_plugin_type_t type

xmmsc_result_t *
xmmsc_main_stats (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_quit (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_broadcast_quit (c)
		xmmsc_connection_t *c


## Medialib

xmmsc_result_t *
xmmsc_medialib_get_id (c, url)
		xmmsc_connection_t *c
		const char *url

xmmsc_result_t *
xmmsc_medialib_remove_entry (c, entry)
		xmmsc_connection_t *c
		int entry

xmmsc_result_t *
xmmsc_medialib_add_entry (c, url)
		xmmsc_connection_t *c
		const char *url

xmmsc_result_t *
xmmsc_medialib_add_entry_args (c, url, ...)
		xmmsc_connection_t *c
		const char *url
	PREINIT:
		int i;
		int nargs;
		const char **args;
	INIT:
		nargs = items - 2;
		args = (const char **)malloc (sizeof (char *) * nargs);

		for (i = 2; i < items; i++) {
			args[i] = SvPV_nolen (ST (i));
		}
	C_ARGS:
		c, url, nargs, args
	CLEANUP:
		free (args);

xmmsc_result_t *
xmmsc_medialib_add_entry_encoded (c, url)
		xmmsc_connection_t *c
		const char *url

perl_xmmsclient_playlist_t *
playlist(c, playlist=XMMS_ACTIVE_PLAYLIST)
		xmmsc_connection_t *c
		const char *playlist
	CODE:
		RETVAL = perl_xmmsclient_playlist_new (c, playlist);
	OUTPUT:
		RETVAL

xmmsc_result_t *
xmmsc_medialib_path_import (c, path)
		xmmsc_connection_t *c
		const char *path

xmmsc_result_t *
xmmsc_medialib_path_import_encoded (c, path)
		xmmsc_connection_t *c
		const char *path

xmmsc_result_t *
xmmsc_medialib_rehash (c, id=0)
		xmmsc_connection_t *c
		uint32_t id

xmmsc_result_t *
xmmsc_medialib_get_info (c, id)
		xmmsc_connection_t *c
		uint32_t id

xmmsc_result_t *
xmmsc_broadcast_medialib_entry_added (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_broadcast_medialib_entry_changed (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_medialib_entry_property_set_int (c, id, key, value)
		xmmsc_connection_t *c
		uint32_t id
		const char *key
		int value

xmmsc_result_t *
xmmsc_medialib_entry_property_set_int_with_source (c, id, source, key, value)
		xmmsc_connection_t *c
		uint32_t id
		const char *source
		const char *key
		int value

xmmsc_result_t *
xmmsc_medialib_entry_property_set_str (c, id, key, value)
		xmmsc_connection_t *c
		uint32_t id
		const char *key
		const char *value

xmmsc_result_t *
xmmsc_medialib_entry_property_set_str_with_source (c, id, source, key, value)
		xmmsc_connection_t *c
		uint32_t id
		const char *source
		const char *key
		const char *value

xmmsc_result_t *
xmmsc_medialib_entry_property_remove (c, id, key)
		xmmsc_connection_t *c
		uint32_t id
		const char *key

xmmsc_result_t *
xmmsc_medialib_entry_property_remove_with_source (c, id, source, key)
		xmmsc_connection_t *c
		uint32_t id
		const char *source
		const char *key


## Collections

xmmsc_result_t *
xmmsc_coll_get (c, collname, namespace)
		xmmsc_connection_t *c
		const char *collname
		xmmsc_coll_namespace_t namespace

xmmsc_result_t *
xmmsc_coll_list (c, namespace)
		xmmsc_connection_t *c
		xmmsc_coll_namespace_t namespace

xmmsc_result_t *
xmmsc_coll_save (c, coll, name, namespace)
		xmmsc_connection_t *c
		xmmsc_coll_t *coll
		const char *name
		xmmsc_coll_namespace_t namespace

xmmsc_result_t *
xmmsc_coll_remove (c, name, namespace)
		xmmsc_connection_t *c
		const char *name
		xmmsc_coll_namespace_t namespace

xmmsc_result_t *
xmmsc_coll_find (c, mediaid, namespace)
		xmmsc_connection_t *c
		unsigned int mediaid
		xmmsc_coll_namespace_t namespace

xmmsc_result_t *
xmmsc_coll_rename (c, from, to, namespace)
		xmmsc_connection_t *c
		char *from
		char *to
		xmmsc_coll_namespace_t namespace

xmmsc_result_t *
xmmsc_coll_query_ids (c, coll, ...)
		xmmsc_connection_t *c
		xmmsc_coll_t *coll
	INIT:
		const char **order = NULL;
		unsigned int limit_start = 0;
		unsigned int limit_len = 0;
		HV *args;
		SV *val;

		if (items == 3 && SvROK (ST (2)) && (SvTYPE (SvRV (ST (2))) == SVt_PVHV)) {
			args = (HV *)SvRV (ST (2));

			if ((val = perl_xmmsclient_hv_fetch (args, "order", 5))) {
				order = (const char **)perl_xmmsclient_unpack_char_ptr_ptr (val);
			}

			if ((val = perl_xmmsclient_hv_fetch (args, "limit_start", 11))) {
				limit_start = SvUV (val);
			}

			if ((val = perl_xmmsclient_hv_fetch (args, "limit_len", 9))) {
				limit_len = SvUV (val);
			}
		}
		else {
			order = (const char **)perl_xmmsclient_unpack_char_ptr_ptr (ST (2));
			limit_start = SvOK (ST (3)) ? SvUV (ST (3)) : 0;
			limit_len = SvOK (ST (4)) ? SvUV (ST (4)) : 0;
		}
	C_ARGS:
		c, coll, order, limit_start, limit_len
	CLEANUP:
		free (order);

xmmsc_result_t *
xmmsc_coll_query_infos (c, coll, ...)
		xmmsc_connection_t *c
		xmmsc_coll_t *coll
	INIT:
		const char **order = NULL;
		unsigned int limit_start = 0;
		unsigned int limit_len = 0;
		const char **fetch = NULL;
		const char **group = NULL;
		HV *args;
		SV *val;

		if (items == 3 && SvROK (ST (2)) && (SvTYPE (SvRV (ST (2))) == SVt_PVHV)) {
			args = (HV *)SvRV (ST (2));

			if ((val = perl_xmmsclient_hv_fetch (args, "order", 5))) {
				order = (const char **)perl_xmmsclient_unpack_char_ptr_ptr (val);
			}

			if ((val = perl_xmmsclient_hv_fetch (args, "fetch", 5))) {
				fetch = (const char **)perl_xmmsclient_unpack_char_ptr_ptr (val);
			}

			if ((val = perl_xmmsclient_hv_fetch (args, "group", 5))) {
				group = (const char **)perl_xmmsclient_unpack_char_ptr_ptr (val);
			}

			if ((val = perl_xmmsclient_hv_fetch (args, "limit_start", 11))) {
				limit_start = SvUV (val);
			}

			if ((val = perl_xmmsclient_hv_fetch (args, "limit_len", 9))) {
				limit_len = SvUV (val);
			}
		}
		else {
			order = (const char **)perl_xmmsclient_unpack_char_ptr_ptr (ST (2));
			limit_start = SvOK (ST (3)) ? SvUV (ST (3)) : 0;
			limit_len = SvOK (ST (4)) ? SvUV (ST (4)) : 0;
			fetch = (const char **)perl_xmmsclient_unpack_char_ptr_ptr (ST (5));
			group = (const char **)perl_xmmsclient_unpack_char_ptr_ptr (ST (6));
		}
	C_ARGS:
		c, coll, order, limit_start, limit_len, fetch, group
	CLEANUP:
		free (order);
		free (fetch);
		free (group);

xmmsc_result_t *
xmmsc_broadcast_collection_changed (c)
		xmmsc_connection_t *c

## XForm

xmmsc_result_t *
xmmsc_xform_media_browse (c, url)
		xmmsc_connection_t *c
		const char *url

xmmsc_result_t *
xmmsc_xform_media_browse_encoded (c, url)
		xmmsc_connection_t *c
		const char *url


## Bindata

xmmsc_result_t *
xmmsc_bindata_add (c, data)
		xmmsc_connection_t *c
	PREINIT:
		STRLEN len = 0;
	INPUT:
		const unsigned char *data = ($type)SvPVbyte ($arg, len);
	C_ARGS:
		c, data, len

xmmsc_result_t *
xmmsc_bindata_retrieve (c, hash)
		xmmsc_connection_t *c
		const char *hash

xmmsc_result_t *
xmmsc_bindata_remove (c, hash)
		xmmsc_connection_t *c
		const char *hash


## Other

xmmsc_result_t *
xmmsc_configval_register (c, valuename, defaultvalue)
		xmmsc_connection_t *c
		const char *valuename
		const char *defaultvalue

xmmsc_result_t *
xmmsc_configval_set (c, key, val)
		xmmsc_connection_t *c
		const char *key
		const char *val

xmmsc_result_t *
xmmsc_configval_get (c, key)
		xmmsc_connection_t *c
		const char *key

xmmsc_result_t *
xmmsc_configval_list (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_broadcast_configval_changed (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_signal_visualisation_data (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_broadcast_mediainfo_reader_status (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_signal_mediainfo_reader_unindexed (c)
		xmmsc_connection_t *c

const char *
xmmsc_userconfdir_get (class)
	PREINIT:
		char path[PATH_MAX];
	CODE:
		RETVAL = xmmsc_userconfdir_get (path, PATH_MAX);
	OUTPUT:
		RETVAL


## Playback

xmmsc_result_t *
xmmsc_playback_tickle (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_playback_stop (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_playback_pause (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_playback_start (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_playback_seek_ms (c, milliseconds)
		xmmsc_connection_t *c
		uint32_t milliseconds

xmmsc_result_t *
xmmsc_playback_seek_ms_rel (c, milliseconds)
		xmmsc_connection_t *c
		int milliseconds

xmmsc_result_t *
xmmsc_playback_seek_samples (c, samples)
		xmmsc_connection_t *c
		uint32_t samples

xmmsc_result_t *
xmmsc_playback_seek_samples_rel (c, samples)
		xmmsc_connection_t *c
		int samples

xmmsc_result_t *
xmmsc_broadcast_playback_status (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_playback_status (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_broadcast_playback_current_id (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_playback_current_id (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_signal_playback_playtime (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_playback_playtime (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_playback_volume_set (c, channel, volume)
		xmmsc_connection_t *c
		const char *channel
		uint32_t volume

xmmsc_result_t *
xmmsc_playback_volume_get (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_broadcast_playback_volume_changed (c)
		xmmsc_connection_t *c


## Playlist

xmmsc_result_t *
xmmsc_playlist_list (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_broadcast_playlist_changed (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_broadcast_playlist_current_pos (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_broadcast_playlist_loaded (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_playlist_current_active (c)
		xmmsc_connection_t *c

xmmsc_result_t *
xmmsc_playlist_set_next (c, pos)
		xmmsc_connection_t *c
		uint32_t pos

xmmsc_result_t *
xmmsc_playlist_set_next_rel (c, pos)
		xmmsc_connection_t *c
		int32_t pos

xmmsc_result_t *
xmmsc_coll_idlist_from_playlist_file (c, path)
		xmmsc_connection_t *c
		const char *path


## IO

int
xmmsc_io_want_out (c)
		xmmsc_connection_t *c

int
xmmsc_io_out_handle (c)
		xmmsc_connection_t *c

int
xmmsc_io_in_handle (c)
		xmmsc_connection_t *c

int
xmmsc_io_fd_get (c)
		xmmsc_connection_t *c

void
xmmsc_io_need_out_callback_set (c, func, data=NULL)
		SV *c
		SV *func
		SV *data
	PREINIT:
		PerlXMMSClientCallback *cb = NULL;
		PerlXMMSClientCallbackParamType param_types[2];
		xmmsc_connection_t *c_con;
	CODE:
		param_types[0] = PERL_XMMSCLIENT_CALLBACK_PARAM_TYPE_CONNECTION;
		param_types[1] = PERL_XMMSCLIENT_CALLBACK_PARAM_TYPE_FLAG;

		c_con = (xmmsc_connection_t *)perl_xmmsclient_get_ptr_from_sv (c, "Audio::XMMSClient");

		cb = perl_xmmsclient_callback_new (func, data, c, 2, param_types);

		xmmsc_io_need_out_callback_set_full (c_con,
		                                     perl_xmmsclient_xmmsc_io_need_out_callback_set_cb, cb,
		                                     (xmmsc_user_data_free_func_t)perl_xmmsclient_callback_destroy);

void
DESTROY (c)
		xmmsc_connection_t *c
	CODE:
		xmmsc_unref (c);

BOOT:
	PERL_UNUSED_VAR (items);
	PERL_XMMSCLIENT_CALL_BOOT (boot_Audio__XMMSClient__Playlist);
	PERL_XMMSCLIENT_CALL_BOOT (boot_Audio__XMMSClient__Collection);
	PERL_XMMSCLIENT_CALL_BOOT (boot_Audio__XMMSClient__Result);
	PERL_XMMSCLIENT_CALL_BOOT (boot_Audio__XMMSClient__Result__PropDict);
	PERL_XMMSCLIENT_CALL_BOOT (boot_Audio__XMMSClient__Result__PropDict__Tie);
