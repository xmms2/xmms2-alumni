namespace Xmms.Client
{
	using System.Runtime.InteropServices;

#if false

	public class Bindata
	{
		private Xmms.Client client;

		public Bindata (Xmms.Client c)
		{
			client = c;
		}

		public string Add (byte[] data)
		{
			return Helper.Sync<string,byte[],int>(NativeMethods.xmmsc_bindata_add, client, data, data.Length);
		}

		public void Add (byte[] data, Xmms.Callback<string> cb)
		{
			Helper.ASync<string,byte[],int>(NativeMethods.xmmsc_bindata_add, client, data, data.Length, cb);
		}

		public byte[] Retrieve (string hash)
		{
			return Helper.Sync<byte[],string>(NativeMethods.xmmsc_bindata_retrieve, client, hash);
		}

		public void Retrieve (string hash, Xmms.Callback<byte[]> cb)
		{
			Helper.ASync<byte[],string>(NativeMethods.xmmsc_bindata_retrieve, client, hash, cb);
		}

		public void Remove (string hash)
		{
			Helper.VoidSync<string>(NativeMethods.xmmsc_bindata_remove, client, hash);
		}

		public void Remove (string hash, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string>(NativeMethods.xmmsc_bindata_remove, client, hash, cb);
		}
	}
#endif
}
