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

	public class Stats {
		public Stats(Client c) {
			client = c;
		}

#if false
		public Xmms.Dict MainStats ()
		{
			return Helper.Sync<Xmms.Dict>(NativeMethods.xmmsc_main_stats, client);
		}

		public void MainStats (Xmms.Callback<Xmms.Dict> cb)
		{
			Helper.ASync<Xmms.Dict>(NativeMethods.xmmsc_main_stats, client, cb);
		}
#endif

		public Result<Value.List<Value.Dictionary<Value.Value>>> PluginList(
			PluginType type
		) {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_plugin_list(
					client.Connection, type
				);

			return new Result<Value.List<Value.Dictionary<Value.Value>>>(
				resultHandle
			);
		}

#if false
		public void BroadcastMediainfoReaderStatus (Xmms.Callback<uint> cb)
		{
			Helper.ASync<uint>(NativeMethods.xmmsc_broadcast_mediainfo_reader_status, client, cb);
		}
#endif

		private readonly Client client;
	}
}
