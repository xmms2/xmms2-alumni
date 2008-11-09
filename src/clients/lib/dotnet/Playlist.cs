namespace Xmms.Client
{
	using System;
	using System.Runtime.InteropServices;

#if false
	public class Playlist
	{
		private Xmms.Client client;

		public Playlist (Xmms.Client c)
		{
			client = c;
		}

		public Xmms.List<string> List ()
		{
			return Helper.Sync<Xmms.List<string>>(NativeMethods.xmmsc_playlist_list, client);
		}

		public void List (Xmms.Callback<Xmms.List<string>> cb)
		{
			Helper.ASync<Xmms.List<string>>(NativeMethods.xmmsc_playlist_list, client, cb);
		}

		public void Shuffle ()
		{
			Shuffle("_active");
		}

		public void Shuffle (string playlist)
		{
			Helper.VoidSync<string>(NativeMethods.xmmsc_playlist_shuffle, client, playlist);
		}

		public void Shuffle (Xmms.VoidCallback cb)
		{
			Shuffle("_active", cb);
		}

		public void Shuffle (string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string>(NativeMethods.xmmsc_playlist_shuffle, client, playlist, cb);
		}

		public void Add (string url, string[] args)
		{
			Add (url, args, "_active");
		}

		public void Add (string url, string[] args, string playlist)
		{
			Helper.VoidSync<string,string,int,string[]>(NativeMethods.xmmsc_playlist_add_args, client, playlist, url, args.Length, args);
		}

		public void Add (string url, string[] args, Xmms.VoidCallback cb)
		{
			Add (url, args, "_active", cb);
		}

		public void Add (string url, string[] args, string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,string,int,string[]>(NativeMethods.xmmsc_playlist_add_args, client, playlist, url, args.Length, args, cb);
		}

		public void Add (string url)
		{
			Add (url, "_active");
		}

		public void Add (string url, string playlist)
		{
			Helper.VoidSync<string,string>(NativeMethods.xmmsc_playlist_add_url, client, playlist, url);
		}

		public void Add (string url, Xmms.VoidCallback cb)
		{
			Add (url, "_active", cb);
		}

		public void Add (string url, string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,string>(NativeMethods.xmmsc_playlist_add_url, client, playlist, url, cb);
		}

		public void Add (uint id)
		{
			Add (id, "_active");
		}

		public void Add (uint id, string playlist)
		{
			Helper.VoidSync<string,uint>(NativeMethods.xmmsc_playlist_add_id, client, playlist, id);
		}

		public void Add (uint id, Xmms.VoidCallback cb)
		{
			Add (id, "_active", cb);
		}

		public void Add (uint id, string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,uint>(NativeMethods.xmmsc_playlist_add_id, client, playlist, id, cb);
		}

		public void Add (Uri url)
		{
			Add (url, "_active");
		}

		public void Add (Uri url, string playlist)
		{
			Helper.VoidSync<string,string>(NativeMethods.xmmsc_playlist_add_encoded, client, playlist, url.ToString());
		}

		public void Add (Uri url, Xmms.VoidCallback cb)
		{
			Add (url, "_active", cb);
		}

		public void Add (Uri url, string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,string>(NativeMethods.xmmsc_playlist_add_encoded, client, playlist, url.ToString(), cb);
		}

		public void Add (object collection)
		{
			Add (collection, "_active");
		}

		public void Add (object collection, string playlist)
		{
			Helper.VoidSync<string,HandleRef,string[]>(NativeMethods.xmmsc_playlist_add_collection, client, playlist, new HandleRef (this, IntPtr.Zero), new string[1]);
		}

		public void Add (object collection, Xmms.VoidCallback cb)
		{
			Add (collection, "_active", cb);
		}

		public void Add (object collection, string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,HandleRef,string[]>(NativeMethods.xmmsc_playlist_add_collection, client, playlist, new HandleRef (this, IntPtr.Zero), new string[1], cb);
		}

		public void RemoveEntry (uint pos)
		{
			RemoveEntry (pos, "_active");
		}

		public void RemoveEntry (uint pos, string playlist)
		{
			Helper.VoidSync<string,uint>(NativeMethods.xmmsc_playlist_remove_entry, client, playlist, pos);
		}

		public void RemoveEntry (uint pos, Xmms.Callback<uint> cb)
		{
			RemoveEntry (pos, "_active", cb);
		}

		public void RemoveEntry (uint pos, string playlist, Xmms.Callback<uint> cb)
		{
			Helper.ASync<uint,string,uint>(NativeMethods.xmmsc_playlist_remove_entry, client, playlist, pos, cb);
		}

		public void Clear ()
		{
			Clear ("_active");
		}

		public void Clear (string playlist)
		{
			Helper.VoidSync<string>(NativeMethods.xmmsc_playlist_clear, client, playlist);
		}

		public void Clear (Xmms.VoidCallback cb)
		{
			Clear ("_active", cb);
		}

		public void Clear (string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string>(NativeMethods.xmmsc_playlist_clear, client, playlist, cb);
		}

		public void Remove (string playlist)
		{
			Helper.VoidSync<string>(NativeMethods.xmmsc_playlist_remove, client, playlist);
		}

		public void Remove (string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string>(NativeMethods.xmmsc_playlist_remove, client, playlist, cb);
		}

		public Xmms.List<uint> ListEntries ()
		{
			return ListEntries ("_active");
		}

		public Xmms.List<uint> ListEntries (string playlist)
		{
			return Helper.Sync<Xmms.List<uint>,string>(NativeMethods.xmmsc_playlist_list_entries, client, playlist);
		}

		public void ListEntries (Xmms.Callback<Xmms.List<uint>> cb)
		{
			ListEntries ("_active", cb);
		}

		public void ListEntries (string playlist, Xmms.Callback<Xmms.List<uint>> cb)
		{
			Helper.ASync<Xmms.List<uint>,string>(NativeMethods.xmmsc_playlist_list_entries, client, playlist, cb);
		}
		
		public void Sort (string[] props)
		{
			Sort (props, "_active");
		}

		public void Sort (string[] props, string playlist)
		{
			Helper.VoidSync<string,string[]>(NativeMethods.xmmsc_playlist_sort, client, playlist, props);
		}

		public void Sort (string[] props, Xmms.VoidCallback cb)
		{
			Sort (props, "_active", cb);
		}

		public void Sort (string[] props, string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,string[]>(NativeMethods.xmmsc_playlist_sort, client, playlist, props, cb);
		}

		public void SetNext (uint pos)
		{
			Helper.VoidSync<uint>(NativeMethods.xmmsc_playlist_set_next, client, pos);
		}
		
		public void SetNextRel (int pos)
		{
			Helper.VoidSync<int>(NativeMethods.xmmsc_playlist_set_next_rel, client, pos);
		}

		public void MoveEntry (uint cpos, uint npos)
		{
			MoveEntry (cpos, npos, "_active");
		}

		public void MoveEntry (uint cpos, uint npos, string playlist)
		{
			Helper.VoidSync<string,uint,uint>(NativeMethods.xmmsc_playlist_move_entry, client, playlist, cpos, npos);
		}

		public void MoveEntry (uint cpos, uint npos, Xmms.VoidCallback cb)
		{
			MoveEntry (cpos, npos, "_active", cb);
		}

		public void MoveEntry (uint cpos, uint npos, string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,uint,uint>(NativeMethods.xmmsc_playlist_move_entry, client, playlist, cpos, npos, cb);
		}

		public uint CurrentPos ()
		{
			return CurrentPos ("_active");
		}

		public uint CurrentPos (string playlist)
		{
			return Helper.Sync<uint,string>(NativeMethods.xmmsc_playlist_current_pos, client, playlist);
		}

		public void CurrentPos (Xmms.Callback<uint> cb)
		{
			CurrentPos ("_active", cb);
		}

		public void CurrentPos (string playlist, Xmms.Callback<uint> cb)
		{
			Helper.ASync<uint,string>(NativeMethods.xmmsc_playlist_current_pos, client, playlist, cb);
		}

		public string CurrentActive ()
		{
			return Helper.Sync<string>(NativeMethods.xmmsc_playlist_current_active, client);
		}

		public void CurrentActive (Xmms.Callback<string> cb)
		{
			Helper.ASync<string>(NativeMethods.xmmsc_playlist_current_active, client, cb);
		}

		public void Add (int pos, string url, string[] args)
		{
			Add (pos, url, args, "_active");
		}

		public void Add (int pos, string url, string[] args, string playlist)
		{
			Helper.VoidSync<string,int,string,int,string[]>(NativeMethods.xmmsc_playlist_insert_args, client, playlist, pos, url, args.Length, args);
		}

		public void Add (int pos, string url, string[] args, Xmms.VoidCallback cb)
		{
			Add (pos, url, args, "_active", cb);
		}

		public void Add (int pos, string url, string[] args, string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,int,string,int,string[]>(NativeMethods.xmmsc_playlist_insert_args, client, playlist, pos, url, args.Length, args, cb);
		}

		public void Add (int pos, string url)
		{
			Add (pos, url, "_active");
		}

		public void Add (int pos, string url, string playlist)
		{
			Helper.VoidSync<string,int,string>(NativeMethods.xmmsc_playlist_insert_url, client, playlist, pos, url);
		}

		public void Add (int pos, string url, Xmms.VoidCallback cb)
		{
			Add (pos, url, "_active", cb);
		}

		public void Add (int pos, string url, string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,int,string>(NativeMethods.xmmsc_playlist_insert_url, client, playlist, pos, url, cb);
		}

		public void Add (int pos, uint id)
		{
			Add (pos, id, "_active");
		}

		public void Add (int pos, uint id, string playlist)
		{
			Helper.VoidSync<string,int,uint>(NativeMethods.xmmsc_playlist_insert_id, client, playlist, pos, id);
		}

		public void Add (int pos, uint id, Xmms.VoidCallback cb)
		{
			Add (pos, id, "_active", cb);
		}

		public void Add (int pos, uint id, string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,int,uint>(NativeMethods.xmmsc_playlist_insert_id, client, playlist, pos, id, cb);
		}

		public void Add (int pos, Uri url)
		{
			Add (pos, url, "_active");
		}

		public void Add (int pos, Uri url, string playlist)
		{
			Helper.VoidSync<string,int,string>(NativeMethods.xmmsc_playlist_insert_encoded, client, playlist, pos, url.ToString());
		}

		public void Add (int pos, Uri url, Xmms.VoidCallback cb)
		{
			Add (pos, url, "_active", cb);
		}

		public void Add (int pos, Uri url, string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,int,string>(NativeMethods.xmmsc_playlist_insert_encoded, client, playlist, pos, url.ToString(), cb);
		}

		public void Add (int pos, object collection)
		{
			Add (pos, collection, "_active");
		}

		public void Add (int pos, object collection, string playlist)
		{
			Helper.VoidSync<string,int,HandleRef,string[]>(NativeMethods.xmmsc_playlist_insert_collection, client, playlist, pos, new HandleRef (this, IntPtr.Zero), new string[1]);
		}

		public void Add (int pos, object collection, Xmms.VoidCallback cb)
		{
			Add (pos, collection, "_active", cb);
		}

		public void Add (int pos, object collection, string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,int,HandleRef,string[]>(NativeMethods.xmmsc_playlist_insert_collection, client, playlist, pos, new HandleRef (this, IntPtr.Zero), new string[1], cb);
		}

		public void Load (string playlist)
		{
			Helper.VoidSync<string>(NativeMethods.xmmsc_playlist_load, client, playlist);
		}

		public void Load (string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string>(NativeMethods.xmmsc_playlist_load, client, playlist, cb);
		}

		public void AddDir (string url)
		{
			Helper.VoidSync<string>(NativeMethods.xmmsc_playlist_radd, client, url);
		}

		public void AddDir (string url, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string>(NativeMethods.xmmsc_playlist_radd, client, url, cb);
		}

		public void Import (string url)
		{
			Import (url, "_active");
		}

		public void Import (string url, string playlist)
		{
			Helper.VoidSync<string,string>(NativeMethods.xmmsc_playlist_import, client, playlist, url);
		}

		public void Import (string url, Xmms.VoidCallback cb)
		{
			Import (url, "_active");
		}

		public void Import (string url, string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,string>(NativeMethods.xmmsc_playlist_import, client, playlist, url, cb);
		}

		public void Export (string mime)
		{
			Import (mime, "_active");
		}

		public void Export (string mime, string playlist)
		{
			Helper.VoidSync<string,string>(NativeMethods.xmmsc_playlist_export, client, playlist, mime);
		}

		public void Export (string mime, Xmms.VoidCallback cb)
		{
			Import (mime, "_active");
		}

		public void Export (string mime, string playlist, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,string>(NativeMethods.xmmsc_playlist_export, client, playlist, mime, cb);
		}

		public void BroadcastChanged (Xmms.Callback<Xmms.Dict> cb)
		{
			Helper.ASync<Xmms.Dict>(NativeMethods.xmmsc_broadcast_playlist_changed, client, cb);
		}

		public void BroadcastCurrentPos (Xmms.Callback<uint> cb)
		{
			Helper.ASync<uint>(NativeMethods.xmmsc_broadcast_playlist_current_pos, client, cb);
		}

		public void BroadcastLoaded (Xmms.Callback<string> cb)
		{
			Helper.ASync<string>(NativeMethods.xmmsc_broadcast_playlist_loaded, client, cb);
		}
	}
#endif
}
