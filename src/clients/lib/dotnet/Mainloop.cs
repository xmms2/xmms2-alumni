using System.Runtime.InteropServices;

namespace Xmms.Client
{
	public interface IMainloop
	{
		ClientHandle Connection {
			set;
		}

		bool IsRunning {
			get;
		}
	}
}
