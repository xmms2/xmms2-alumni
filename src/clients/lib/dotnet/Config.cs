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

using System.Runtime.InteropServices;

namespace Xmms.Client {

	public class Config {

		public Config(Client c) {
			client = c;
		}

#if false
		public void Register (string key, string val)
		{
			Helper.VoidSync<string,string>(NativeMethods.xmmsc_configval_register, client, key, val);
		}

		public void Register (string key, string val, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,string>(NativeMethods.xmmsc_configval_register, client, key, val, cb);
		}

		public void Set (string key, string val)
		{
			Helper.VoidSync<string,string>(NativeMethods.xmmsc_configval_set, client, key, val);
		}
		
		public void Set (string key, string val, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,string>(NativeMethods.xmmsc_configval_set, client, key, val, cb);
		}
#endif

		public Result<Value.String> Get(string key) {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_configval_get(client.Connection, key);

			return new Result<Value.String>(resultHandle);
		}

		public Result<Value.Dictionary<Value.String>> List() {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_configval_list(client.Connection);

			return new Result<Value.Dictionary<Value.String>>(
				resultHandle
			);
		}

#if false
		public void BroadcastValueChanged (Xmms.Callback<Xmms.Dict> cb)
		{
			Helper.ASync<Xmms.Dict>(NativeMethods.xmmsc_broadcast_configval_changed, client, cb);
		}
#endif

		private readonly Client client;
	}
}
