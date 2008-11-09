namespace Xmms.Client
{
	using System;
	using System.Runtime.InteropServices;
	using Mono.Unix.Native;

	public class UnixMainloop : Xmms.IMainloop
	{
		private HandleRef conn;
		private bool is_running;

		public UnixMainloop (HandleRef c)
		{
			conn = c;
			is_running = false;
		}

		public bool IsRunning
		{
			get {
				return is_running;
			}
		}

		public void run ()
		{
			is_running = true;

			Pollfd[] pollfds = new Pollfd [1];
			pollfds[0].fd = Xmms.API.xmmsc_io_fd_get (conn);
			pollfds[0].events = PollEvents.POLLIN;

			while (is_running) {
				if (Xmms.API.xmmsc_io_want_out (conn)) {
					pollfds[0].events |= PollEvents.POLLOUT;
				}

				Syscall.poll (pollfds, (uint) pollfds.Length, 0);

				if (((pollfds[0].revents & PollEvents.POLLERR ) == PollEvents.POLLERR) ||
					((pollfds[0].revents & PollEvents.POLLHUP ) == PollEvents.POLLHUP) ||
					((pollfds[0].revents & PollEvents.POLLNVAL) == PollEvents.POLLHUP)) {
					Xmms.API.xmmsc_io_disconnect (conn);
					is_running = !is_running;
				} else {
					if ((pollfds[0].revents & PollEvents.POLLOUT) == PollEvents.POLLOUT) {
						Xmms.API.xmmsc_io_out_handle (conn);
						pollfds[0].events &= ~PollEvents.POLLOUT;
					}
					if ((pollfds[0].revents & PollEvents.POLLIN) == PollEvents.POLLIN) {
						Xmms.API.xmmsc_io_in_handle (conn);
					}
				}
			}

		}
	}
}
