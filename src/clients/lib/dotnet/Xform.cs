namespace Xmms.Client
{
	using System.Runtime.InteropServices;

#if false
	public class Xform
	{
		private Xmms.Client client;

		public Xform (Xmms.Client c)
		{
			client = c;
		}

		public Xmms.Dict Browse (string url)
		{
			return Helper.Sync<Xmms.Dict,string>(NativeMethods.xmmsc_xform_media_browse, client, url);
		}

		public void Browse (string url, Xmms.Callback<Xmms.List<Xmms.Dict>> cb)
		{
			Helper.ASync<Xmms.List<Xmms.Dict>,string>(NativeMethods.xmmsc_xform_media_browse, client, url, cb);
		}
	}
#endif
}
