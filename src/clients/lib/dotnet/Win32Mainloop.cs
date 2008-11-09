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
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace Xmms.Client.Win32 {
	public class Win32Mainloop : IMainloop {

		internal static class NativeMethods {
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
				ClientHandle c, IOHandler func, IntPtr user_data
			);

			[DllImport("libxmmsclient")]
			internal static extern void xmmsc_io_disconnect(ClientHandle c);

			[DllImport("ws2_32")]
			public static extern int WSAAsyncSelect(
				int socket, IntPtr hwnd, uint wMsg, uint events
			);

			public const uint FD_READ = 0x01;
			public const uint FD_WRITE = 0x02;
			public const uint FD_CONNECT = 0x10;
			public const uint FD_CLOSE = 0x20;
		}

		private static readonly uint WM_SOCKETREAD = 0x8001;

		private class EventHandler : Control {
			public EventHandler() {
				// Stick the IOHandler in an instance variable so it won't
				// be claimed by the GC.
				needOutCallback = new IOHandler(
					delegate { requestEvents(); }
				);
			}

			public void Run() {
				NativeMethods.xmmsc_io_need_out_callback_set(
					Connection, needOutCallback, IntPtr.Zero
				);

				requestEvents();
			}

			/// <summary>
			/// Processes Windows messages.
			/// </summary>
			/// <param name="m">
			/// The Windows System.Windows.Forms.Message to process.
			/// </param>
			protected override void WndProc(ref Message m) {
				base.WndProc(ref m);

				// We are only interested in the WM_SOCKETREAD message.
				if (m.Msg != WM_SOCKETREAD)
					return;

				uint events = (uint)m.LParam.ToInt32();

				if ((events & NativeMethods.FD_CLOSE) != 0)
					NativeMethods.xmmsc_io_disconnect(Connection);
				else {
					if ((events & NativeMethods.FD_WRITE) != 0)
						NativeMethods.xmmsc_io_out_handle(Connection);

					if ((events & NativeMethods.FD_READ) != 0)
						NativeMethods.xmmsc_io_in_handle(Connection);

					requestEvents();
				}
			}

			private void requestEvents() {
				uint flags =
					NativeMethods.FD_READ |
					NativeMethods.FD_CLOSE;

				if (NativeMethods.xmmsc_io_want_out(Connection))
					flags |= NativeMethods.FD_WRITE;

				int status = NativeMethods.WSAAsyncSelect(
					NativeMethods.xmmsc_io_fd_get(Connection),
					Handle,
					WM_SOCKETREAD,
					flags
				);
			}

			public ClientHandle Connection;
			private readonly IOHandler needOutCallback;
		}

		public Win32Mainloop() {
			eventHandler = new EventHandler();
		}

		public ClientHandle Connection {
			set {
				connection = value;

				eventHandler.Connection = value;
			}
		}

		public bool IsRunning {
			get { return isRunning; }
		}

		public void Run() {
			isRunning = true;

			eventHandler.Run();
		}

		private ClientHandle connection;
		private bool isRunning;
		private readonly EventHandler eventHandler;
	}
}
