//
//  .NET bindings for the XMMS2 client library
//
//  Copyright (C) 2005 Daniel Svensson, <nano@nittioonio.nu>
//  Copyright (C) 2008 Tilman Sauerbeck, <tilman@xmms.org>
//
//  This library is free software; you can redistribute it and/or
//  modify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation; either
//  version 2.1 of the License, or (at your option) any later version.
//
//  This library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//

using System;
using System.Text;
using System.Runtime.InteropServices;

namespace Xmms.Client {
	public class ClientHandle : SafeHandle {
		private ClientHandle() : base(IntPtr.Zero, true) {
		}

		public override bool IsInvalid {
			get {
				return handle == IntPtr.Zero;
			}
		}

		protected override bool ReleaseHandle() {
			NativeMethods.xmmsc_unref(handle);

			return true;
		}
	}

	public class ResultHandle : SafeHandle {
		private ResultHandle() : base(IntPtr.Zero, true) {
		}

		public override bool IsInvalid {
			get {
				return handle == IntPtr.Zero;
			}
		}

		protected override bool ReleaseHandle() {
			System.Diagnostics.Debug.WriteLine("killing result");
			NativeMethods.xmmsc_result_unref(handle);

			return true;
		}
	}

	public class ValueHandle : SafeHandle {
		private ValueHandle() : base(IntPtr.Zero, true) {
		}

		internal ValueHandle(IntPtr handle) : base(IntPtr.Zero, true) {
			SetHandle(NativeMethods.xmmsv_ref(handle));
		}

		public override bool IsInvalid {
			get {
				return handle == IntPtr.Zero;
			}
		}

		protected override bool ReleaseHandle() {
			System.Diagnostics.Debug.WriteLine("killing value");
			NativeMethods.xmmsv_unref(handle);

			return true;
		}
	}

	public class ListIteratorHandle : SafeHandle {
		private ListIteratorHandle() : base(IntPtr.Zero, true) {
		}

		public override bool IsInvalid {
			get {
				return handle == IntPtr.Zero;
			}
		}

		protected override bool ReleaseHandle() {
			return true;
		}
	}

	public class DictionaryIteratorHandle : SafeHandle {
		private DictionaryIteratorHandle() : base(IntPtr.Zero, true) {
		}

		public override bool IsInvalid {
			get {
				return handle == IntPtr.Zero;
			}
		}

		protected override bool ReleaseHandle() {
			return true;
		}
	}

	internal class NativeMethods {
		//
		//  XMMSClient
		//

		[DllImport("libxmmsclient")]
		internal static extern ClientHandle xmmsc_init (string clientname);

		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_connect (ClientHandle c, string ipcpath);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_disconnect_callback_set(
			ClientHandle c, DiscoShit func, HandleRef udata
		);

		[DllImport("libxmmsclient")]
		internal static extern string xmmsc_get_last_error (ClientHandle c);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_unref(IntPtr c);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_ref(ClientHandle c);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_lock_set(
			ClientHandle c, HandleRef _lock,
			LockHandler locker, LockHandler unlocker
		);

		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_quit(ClientHandle c);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_broadcast_disconnect(
			ClientHandle res
		);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_signal_disconnect(
			ClientHandle res
		);

		/* Playlist Commands */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_list(
			ClientHandle c
		);

		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_shuffle(
			ClientHandle c, string playlist
		);

		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_add_args (ClientHandle c, string playlist, string url, int argc, string[] argv);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_add_url (ClientHandle c, string playlist, string url);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_add_id (ClientHandle c, string playlist, uint id);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_add_encoded (ClientHandle c, string playlist, string url);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_add_collection (ClientHandle c, string playlist, HandleRef collection, string[] order);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_remove_entry (ClientHandle c, string playlist, uint pos);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_clear (ClientHandle c, string playlist);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_remove (ClientHandle c, string playlist);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_list_entries (ClientHandle c, string playlist);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_sort (ClientHandle c, string playlist, string[] properties);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_set_next (ClientHandle c, uint pos);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_set_next_rel (ClientHandle c, int pos);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_move_entry (ClientHandle c, string playlist, uint id, uint moves);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_current_pos (ClientHandle c, string playlist);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_current_active (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_insert_args (ClientHandle c, string playlist, int pos, string url, int argc, string[] argv);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_insert_url (ClientHandle c, string playlist, int pos, string url);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_insert_id (ClientHandle c, string playlist, int pos, uint id);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_insert_encoded (ClientHandle c, string playlist, int pos, string url);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_insert_collection (ClientHandle c, string playlist, int pos, HandleRef collection, string[] order);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_load (ClientHandle c, string playlist);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_radd (ClientHandle c, string playlist);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_radd_encoded (ClientHandle c, string playlist, string url);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_import (ClientHandle c, string playlist, string url);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playlist_export (ClientHandle c, string playlist, string url);

		/* Playlist Broadcasts */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_broadcast_playlist_changed (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_broadcast_playlist_current_pos (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_broadcast_playlist_loaded (ClientHandle c);



		/* Playback Commands */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playback_stop (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playback_tickle (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playback_start (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playback_pause (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playback_current_id (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playback_seek_ms (ClientHandle c, uint milliseconds);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playback_seek_ms_rel (ClientHandle c, uint milliseconds);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playback_seek_samples (ClientHandle c, uint samples);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playback_seek_samples_rel (ClientHandle c, uint samples);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playback_playtime (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playback_status (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playback_volume_get (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_playback_volume_set (ClientHandle c, string chn, uint val);

		/* Playback Broadcasts */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_broadcast_playback_volume_changed (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_broadcast_playback_status (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_broadcast_playback_current_id (ClientHandle c);

		/* Playback Signals */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_signal_playback_playtime (ClientHandle c);



		/* Config Commands */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_configval_set (ClientHandle c, string key, string val);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_configval_list (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_configval_get (ClientHandle c, string key);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_configval_register (ClientHandle c, string valuename, string defaultvalue);

		/* Config Broadcasts */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_broadcast_configval_changed (ClientHandle c);



		/* Stats Commands */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_plugin_list(
			ClientHandle c, PluginType type
		);

		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_main_stats (ClientHandle c);

		/* Stats Broadcasts */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_broadcast_mediainfo_reader_status (ClientHandle c);

		/* Stats Signals */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_signal_visualisation_data (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_signal_mediainfo_reader_unindexed (ClientHandle c);



		/* Medialib Commands */
		[DllImport("libxmmsclient")]
		internal static extern int xmmsc_entry_format (StringBuilder dst, int len, string fmt, ClientHandle res);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_select (ClientHandle c, string query);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_add_entry (ClientHandle c, string url);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_add_entry_args (ClientHandle c, string url, int argc, string[] argv);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_add_entry_encoded (ClientHandle c, string url);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_get_info (ClientHandle c, uint id);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_path_import (ClientHandle c, string path);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_path_import_encoded (ClientHandle c, string path);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_rehash (ClientHandle c, uint id);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_get_id (ClientHandle c, string url);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_remove_entry (ClientHandle c, uint entry);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_entry_property_set_int (ClientHandle c, uint id, string key, int val);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_entry_property_set_int_with_source (ClientHandle c, uint id, string src, string key, int val);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_entry_property_set_str (ClientHandle c, uint id, string key, string val);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_entry_property_set_str_with_source (ClientHandle c, uint id, string src, string key, string val);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_entry_property_remove (ClientHandle c, uint id, string key);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_medialib_entry_property_remove_with_source (ClientHandle c, uint id, string source, string key);

		/* Medialib Broadcasts */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_broadcast_medialib_entry_changed (ClientHandle c);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_broadcast_medialib_entry_added (ClientHandle c);

		/* Xform Commands */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_xform_media_browse (ClientHandle c, string url);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_xform_media_browse_encoded (ClientHandle c, string url);

		/* Bindata Commands */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_bindata_add (ClientHandle c, byte[] data, int len);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_bindata_retrieve (ClientHandle c, string hash);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_bindata_remove (ClientHandle c, string hash);


		/* ClientHandle Control Commands */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_coll_get (ClientHandle c, string name, string ns);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_coll_list (ClientHandle c, string ns);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_coll_save (ClientHandle c, HandleRef coll, string name, string ns);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_coll_remove (ClientHandle c, string name, string ns);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_coll_find (ClientHandle c, uint mid, string ns);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_coll_rename (ClientHandle c, string from, string to, string ns);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_coll_query_ids (ClientHandle c, HandleRef coll, string[] order, uint start, uint end);
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_coll_query_infos (ClientHandle c, HandleRef coll, string[] order, uint start, uint end, string[] fetch, string[] group);

		/* ClientHandle Control Broadcasts */
		[DllImport("libxmmsclient")]
		internal static extern ResultHandle xmmsc_coll_broadcast_collection_changed (ClientHandle c);



		/* ClientHandle Parser Delegates */
		internal delegate IntPtr CollectionParseTokens (string str, string[] newpos);
		internal delegate IntPtr CollectionParseBuild (IntPtr tokens);

		/* ClientHandle Parser Commands */
		[DllImport("libxmmsclient")]
		internal static extern IntPtr xmmsc_coll_parse (string pattern, out ClientHandle coll);
		[DllImport("libxmmsclient")]
		internal static extern IntPtr xmmsc_coll_parse_custom (string pattern, CollectionParseTokens tokens, CollectionParseBuild build);
		[DllImport("libxmmsclient")]
		internal static extern IntPtr xmmsc_coll_default_parse_tokens (string str, string[] newpos);
		[DllImport("libxmmsclient")]
		internal static extern IntPtr xmmsc_coll_default_parse_builds (IntPtr tokens);



		/* ClientHandle Structure Delegates */
		internal delegate void CollectionAttributeForeach (string key, string val, ClientHandle udata);

		/* ClientHandle Structure Commands */
		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_coll_ref (HandleRef coll);

		[DllImport("libxmmsclient")]
		internal static extern IntPtr xmmsc_coll_new(CollectionType type);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_coll_unref (HandleRef coll);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_coll_set_type(
			HandleRef coll, CollectionType type
		);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_coll_set_idlist (HandleRef coll, uint[] ids);
		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_coll_add_operand (HandleRef coll, HandleRef op);
		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_coll_remove_operand (HandleRef coll, HandleRef op);
		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_coll_idlist_append (HandleRef coll, uint id);
		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_coll_idlist_insert (HandleRef coll, uint id, uint index);
		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_coll_idlist_move (HandleRef coll, uint index, uint newindex);
		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_coll_idlist_remove (HandleRef coll, uint index);
		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_coll_idlist_clear (HandleRef coll);
		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_coll_idlist_get_index (HandleRef coll, uint index, out uint val);
		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_coll_idlist_set_index (HandleRef coll, uint index, uint val);
		[DllImport("libxmmsclient")]
		internal static extern uint xmmsc_coll_idlist_get_size (HandleRef coll);

		[DllImport("libxmmsclient")]
		internal static extern CollectionType xmmsc_coll_get_type(
			HandleRef coll
		);

		[DllImport("libxmmsclient")]
		internal static extern uint xmmsc_coll_get_idlist (HandleRef coll);
		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_coll_operand_list_first (HandleRef coll);
		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_coll_operand_list_entry (HandleRef coll, out IntPtr operand);
		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_coll_operand_list_next (HandleRef coll);
		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_coll_operand_list_save (HandleRef coll);
		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_coll_operand_list_restore (HandleRef coll);
		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_coll_attribute_set (HandleRef coll, string key, string val);
		[DllImport("libxmmsclient")]
		internal static extern int xmmsc_coll_attribute_remove (HandleRef coll, string key);
		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_coll_attribute_get (HandleRef coll, string key, out string val);
		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_coll_attribute_foreach (HandleRef coll, CollectionAttributeForeach func, HandleRef user_data);
		[DllImport("libxmmsclient")]
		internal static extern IntPtr xmmsc_coll_universe ();

		internal delegate int ResultNotifier(
			IntPtr valuePtr, IntPtr user_data
		);

		/* Result Commands */
		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_result_ref(IntPtr res);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_result_unref(IntPtr res);

		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_result_iserror(
			ResultHandle res
		);

		[DllImport("libxmmsclient")]
		internal static extern string xmmsc_result_get_error(
			ResultHandle res
		);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_result_wait(ResultHandle res);

		[DllImport("libxmmsclient")]
		internal static extern ResultClass xmmsc_result_get_class(
			ResultHandle res
		);

		[DllImport("libxmmsclient")]
		internal static extern IntPtr xmmsc_result_get_value(
			ResultHandle res
		);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_result_notifier_set(
			ResultHandle res, ResultNotifier func, IntPtr user_data
		);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_result_disconnect(
			ResultHandle res
		);

#if false
		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_result_source_preference_set (ResultHandle res, string[] preference);
#endif

		//
		// XMMSClient.IOFunctions
		//

		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_io_want_out(ClientHandle c);

		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_io_out_handle(ClientHandle c);

		[DllImport("libxmmsclient")]
		internal static extern bool xmmsc_io_in_handle(ClientHandle c);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsc_io_fd_get(ClientHandle c);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_io_need_out_callback_set(
			ClientHandle c, IOHandler func, HandleRef udata
		);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsc_io_disconnect(ClientHandle c);

		[DllImport("libxmmsclient")]
		internal static extern IntPtr xmmsc_userconfdir_get(
			StringBuilder sb, int size
		);

		[DllImport("libxmmsclient")]
		internal static extern IntPtr xmmsv_ref(IntPtr c);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsv_unref(IntPtr c);

		[DllImport("libxmmsclient")]
		internal static extern ValueType xmmsv_get_type(
			ValueHandle value
		);

		[DllImport("libxmmsclient")]
		internal static extern ValueHandle xmmsv_new_int(int value);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_get_int(
			ValueHandle value, out int r
		);

		[DllImport("libxmmsclient")]
		internal static extern ValueHandle xmmsv_new_uint(uint value);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_get_uint(
			ValueHandle value, out uint r
		);

		[DllImport("libxmmsclient")]
		internal static extern ValueHandle xmmsv_new_string(string value);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_get_string(
			ValueHandle value, out string r
		);

		[DllImport("libxmmsclient")]
		internal static extern ValueHandle xmmsv_new_list();

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_list_get(
			ValueHandle list, int index, out ValueHandle value
		);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_list_set(
			ValueHandle list, int index, ValueHandle value
		);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_list_insert(
			ValueHandle list, int index, ValueHandle item
		);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_list_remove(
			ValueHandle list, int index
		);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_list_append(
			ValueHandle list, ValueHandle item
		);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_list_clear(ValueHandle list);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_list_get_size(ValueHandle list);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_get_list_iter(
			ValueHandle list, out ListIteratorHandle iterator
		);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_list_iter_entry(
			ListIteratorHandle iterator, out ValueHandle value
		);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_list_iter_valid(
			ListIteratorHandle iterator
		);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsv_list_iter_first(
			ListIteratorHandle iterator
		);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsv_list_iter_next(
			ListIteratorHandle iterator
		);

		[DllImport("libxmmsclient")]
		internal static extern ValueHandle xmmsv_new_dict();

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_dict_get(
			ValueHandle dictionary, string key, out ValueHandle value
		);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_dict_insert(
			ValueHandle dictionary, string key, ValueHandle value
		);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_dict_remove(
			ValueHandle dictionary, string key
		);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_dict_clear(
			ValueHandle dictionary
		);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_dict_get_size(
			ValueHandle dictionary
		);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_get_dict_iter(
			ValueHandle dictionary,
			out DictionaryIteratorHandle iterator
		);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_dict_iter_pair(
			DictionaryIteratorHandle iterator,
			out string key, out ValueHandle value
		);

		[DllImport("libxmmsclient")]
		internal static extern int xmmsv_dict_iter_valid(
			DictionaryIteratorHandle iterator
		);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsv_dict_iter_first(
			DictionaryIteratorHandle iterator
		);

		[DllImport("libxmmsclient")]
		internal static extern void xmmsv_dict_iter_next(
			DictionaryIteratorHandle iterator
		);

		[DllImport("libxmmsclient")]
		internal static extern IntPtr xmmsv_propdict_to_dict(
			ValueHandle propdict, IntPtr src_prefs
		);
	}

	public delegate void IOHandler(int i, IntPtr udata);
	public delegate void DiscoShit(int i, HandleRef udata);
	public delegate void LockHandler(int i, HandleRef udata);

	public enum PlaylistChange {
		Add = 0,
		Insert,
		Shuffle,
		Remove,
		Clear,
		Move,
		Sort,
		Update,
	}

	public enum PlaybackStatus {
		Stopped = 0,
		Playing,
		Paused,
	}

	public enum MediainfoReaderStatus {
		Idle = 0,
		Running,
	}

	public enum PluginType {
		All = 0,
		Output,
		Playlist,
		Effect,
		Xform,
	}

	public enum ResultType {
		Error = -1,
		None,
		UInt32,
		Int32,
		String,
		Stringlist,
		Dict,
		List,
		PropDict,
		Collection,
		Bin,
	}

	public enum ResultClass {
		Default = 0,
		Signal,
		Broadcast,
	}

	public enum CollectionChange {
		Add = 0,
		Update,
		Rename,
		Remove,
	}

	public enum CollectionType {
		Error = 0,
		Reference,
		Union,
		Intersection,
		Complement,
		Has,
		Match,
		Contains,
		Smaller,
		Greater,
		IdList0,
		Queue,
		PartyShuffle,
	}

	public enum ValueType {
		None = 0,
		Error,
		UInt32,
		Int32,
		String,
		Collection,
		BinaryData,
		List,
		Dictionary
	}
}
