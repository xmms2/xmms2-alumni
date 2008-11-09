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

namespace Xmms.Client {
	public class Medialib {
		public Medialib(Client c) {
			client = c;
		}

#if false
		public Xmms.List<Xmms.Dict> Select (string query)
		{
			return Helper.Sync<Xmms.List<Xmms.Dict>,string>(NativeMethods.xmmsc_medialib_select, client, query);
		}

		public void Select (string query, Xmms.Callback<Xmms.List<Xmms.Dict>> cb)
		{
			Helper.ASync<Xmms.List<Xmms.Dict>,string>(NativeMethods.xmmsc_medialib_select, client, query, cb);
		}

		public void Add (string url)
		{
			Helper.VoidSync<string>(NativeMethods.xmmsc_medialib_add_entry, client, url);
		}

		public void Add (string url, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string>(NativeMethods.xmmsc_medialib_add_entry, client, url, cb);
		}

		public void Add (string url, string[] args)
		{
			Helper.VoidSync<string,int,string[]>(NativeMethods.xmmsc_medialib_add_entry_args, client, url, args.Length, args);
		}

		public void Add (string url, string[] args, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,int,string[]>(NativeMethods.xmmsc_medialib_add_entry_args, client, url, args.Length, args, cb);
		}

		public void Add (Uri url)
		{
			Helper.VoidSync<string>(NativeMethods.xmmsc_medialib_add_entry_encoded, client, url.ToString());
		}

		public void Add (Uri url, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string>(NativeMethods.xmmsc_medialib_add_entry_encoded, client, url.ToString(), cb);
		}
#endif

		public Result<Value.Dictionary<Value.Dictionary<Value.Value>>> GetInfo(uint id) {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_medialib_get_info(
					client.Connection, id
				);

			return new Result<Value.Dictionary<Value.Dictionary<Value.Value>>>(
				resultHandle
			);
		}

#if false
		public void ImportPath (string url)
		{
			Helper.VoidSync<string>(NativeMethods.xmmsc_medialib_path_import, client, url);
		}

		public void ImportPath (string url, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string>(NativeMethods.xmmsc_medialib_path_import, client, url, cb);
		}

		public void ImportPath (Uri url)
		{
			Helper.VoidSync<string>(NativeMethods.xmmsc_medialib_path_import_encoded, client, url.ToString());
		}

		public void ImportPath (Uri url, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string>(NativeMethods.xmmsc_medialib_path_import_encoded, client, url.ToString(), cb);
		}

		public void Rehash (uint id)
		{
			Helper.VoidSync<uint>(NativeMethods.xmmsc_medialib_rehash, client, id);
		}

		public void Rehash (uint id, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<uint>(NativeMethods.xmmsc_medialib_rehash, client, id, cb);
		}

		public uint GetID (string url)
		{
			return Helper.Sync<uint,string>(NativeMethods.xmmsc_medialib_get_id, client, url);
		}

		public void GetID (string url, Xmms.Callback<uint> cb)
		{
			Helper.ASync<uint,string>(NativeMethods.xmmsc_medialib_get_id, client, url, cb);
		}

		public void Remove (uint id)
		{
			Helper.VoidSync<uint>(NativeMethods.xmmsc_medialib_remove_entry, client, id);
		}

		public void Remove (uint id, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<uint>(NativeMethods.xmmsc_medialib_remove_entry, client, id, cb);
		}

		public void PropertyUnset (uint id, string key)
		{
			Helper.VoidSync<uint,string>(NativeMethods.xmmsc_medialib_entry_property_remove, client, id, key);
		}

		public void PropertyUnset (uint id, string key, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<uint,string>(NativeMethods.xmmsc_medialib_entry_property_remove, client, id, key, cb);
		}

		public void PropertyUnset (uint id, string key, string source)
		{
			Helper.VoidSync<uint,string,string>(NativeMethods.xmmsc_medialib_entry_property_remove_with_source, client, id, source, key);
		}

		public void PropertyUnset (uint id, string key, string source, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<uint,string,string>(NativeMethods.xmmsc_medialib_entry_property_remove_with_source, client, id, source, key, cb);
		}

		public void PropertySet (uint id, string key, string val)
		{
			Helper.VoidSync<uint,string,string>(NativeMethods.xmmsc_medialib_entry_property_set_str, client, id, key, val);
		}

		public void PropertySet (uint id, string key, string val, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<uint,string,string>(NativeMethods.xmmsc_medialib_entry_property_set_str, client, id, key, val, cb);
		}

		public void PropertySet (uint id, string key, string val, string source)
		{
			Helper.VoidSync<uint,string,string,string>(NativeMethods.xmmsc_medialib_entry_property_set_str_with_source, client, id, source, key, val);
		}

		public void PropertySet (uint id, string key, string val, string source, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<uint,string,string,string>(NativeMethods.xmmsc_medialib_entry_property_set_str_with_source, client, id, source, key, val, cb);
		}

		public void PropertySet (uint id, string key, int val)
		{
			Helper.VoidSync<uint,string,int>(NativeMethods.xmmsc_medialib_entry_property_set_int, client, id, key, val);
		}

		public void PropertySet (uint id, string key, int val, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<uint,string,int>(NativeMethods.xmmsc_medialib_entry_property_set_int, client, id, key, val, cb);
		}

		public void PropertySet (uint id, string key, int val, string source)
		{
			Helper.VoidSync<uint,string,string,int>(NativeMethods.xmmsc_medialib_entry_property_set_int_with_source, client, id, source, key, val);
		}

		public void PropertySet (uint id, string key, int val, string source, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<uint,string,string,int>(NativeMethods.xmmsc_medialib_entry_property_set_int_with_source, client, id, source, key, val, cb);
		}

		public void BroadcastEntryAdded (Xmms.Callback<uint> cb)
		{
			Helper.ASync<uint>(NativeMethods.xmmsc_broadcast_medialib_entry_added, client, cb);
		}

		public void BroadcastEntryChanged (Xmms.Callback<uint> cb)
		{
			Helper.ASync<uint>(NativeMethods.xmmsc_broadcast_medialib_entry_changed, client, cb);
		}
#endif

		private readonly Client client;
	}
}
