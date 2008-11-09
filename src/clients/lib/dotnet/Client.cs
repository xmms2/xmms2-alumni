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
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace Xmms.Client {

	public class Client : IDisposable {
		private readonly string name;
		private readonly ClientHandle conn;

		public readonly Playback Playback;
#if false
		public readonly Playlist Playlist;
#endif
		public readonly Medialib Medialib;
		public readonly Config Config;
#if false
		public readonly Xform Xform;
		public readonly Bindata Bindata;
		public readonly Collection Collection;
#endif
		public readonly Stats Stats;

		private IMainloop ml;
		private bool cstate;

		public Client(string clientname) {
			if (clientname == null)
				throw new ArgumentNullException("clientname");

			name = clientname;

			conn = NativeMethods.xmmsc_init(name);

			Playback = new Playback(this);
#if false
			Playlist = new Playlist(this);
#endif
			Medialib = new Medialib(this);
			Config = new Config(this);
#if false
			Xform = new Xform(this);
			Bindata = new Bindata(this);
			Collection = new Collection(this);
#endif
			Stats = new Stats(this);
		}

		public bool IsConnected {
			get { return cstate; }
		}

		internal ClientHandle Connection {
			get { return conn; }
		}

		public IMainloop Mainloop {
			get { return ml; }

			set {
				ml = value;
				ml.Connection = conn;
			}
		}

		~Client() {
			Dispose();
		}

		public void Dispose() {
			conn.Dispose();
		}

		public void Connect(string path) {
			if (path == null)
				throw new ArgumentNullException("path");

			bool ret = NativeMethods.xmmsc_connect(conn, path);

			if (!ret) {
				string error = NativeMethods.xmmsc_get_last_error(conn);

				throw new System.IO.IOException(
					"Could not connect to xmms2d: " + error
				);
			}

			cstate = true;
		}

		public static string UserConfDir {
			get {
				System.Text.StringBuilder sb =
					new System.Text.StringBuilder(256);

				if (NativeMethods.xmmsc_userconfdir_get(sb, sb.Capacity) == IntPtr.Zero) {
					Console.WriteLine(sb);
					throw new Exception ("xmmsc_userconfdir_get failed!");
				}

				return sb.ToString();
			}
		}

		public Result Quit() {
			ResultHandle resultHandle =
				NativeMethods.xmmsc_quit(Connection);

			return new Result(resultHandle);
		}
	}
}
