namespace Xmms.Client
{
	using System;
	using System.Runtime.InteropServices;
	using Gtk;

	namespace Glib {
		internal class API
		{
			[DllImport("libxmmsclient-glib")]
			public static extern IntPtr xmmsc_mainloop_gmain_init (ClientHandle conn);

			[DllImport("libxmmsclient-glib")]
			public static extern IntPtr xmmsc_mainloop_gmain_shutdown (ClientHandle conn, HandleRef data);
		}
	}

	public class GlibMainloop : Xmms.IMainloop
	{
		private bool is_running;
		private ClientHandle conn;
		private HandleRef data;

		public GlibMainloop ()
		{
			is_running = false;
		}

		~GlibMainloop ()
		{
			Xmms.Glib.API.xmmsc_mainloop_gmain_shutdown (conn, data);
		}

		public ClientHandle Connection
		{
			set {
				IntPtr ptr = Xmms.Glib.API.xmmsc_mainloop_gmain_init (value);
				data = new HandleRef (this, ptr);
				is_running = true;
			}
		}
	
		public bool IsRunning
		{
			get {
				return is_running;
			}
		}
	}
}
