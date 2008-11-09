namespace Xmms.Client
{
	using System;
	using System.Runtime.InteropServices;

#if false
	public class Collection
	{
		private Xmms.Client client;

		public Collection (Xmms.Client c)
		{
			client = c;
		}

		public Xmms.Coll Get (string name, string ns)
		{
			return Helper.Sync<Xmms.Coll,string,string>(NativeMethods.xmmsc_coll_get, client, name, ns);
		}

		public void Get (string name, string ns, Xmms.Callback<Xmms.Coll> cb)
		{
			Helper.ASync<Xmms.Coll,string,string>(NativeMethods.xmmsc_coll_get, client, name, ns, cb);
		}

		public Xmms.List<string> List (string ns)
		{
			return Helper.Sync<Xmms.List<string>,string>(NativeMethods.xmmsc_coll_list, client, ns);
		}

		public void List (string ns, Xmms.Callback<Xmms.List<string>> cb)
		{
			Helper.ASync<Xmms.List<string>,string>(NativeMethods.xmmsc_coll_list, client, ns, cb);
		}

		public void Save (Xmms.Coll coll, string name, string ns)
		{
			Helper.VoidSync<HandleRef,string,string>(NativeMethods.xmmsc_coll_save, client, coll, name, ns);
		}

		public void Save (Xmms.Coll coll, string name, string ns, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<HandleRef,string,string>(NativeMethods.xmmsc_coll_save, client, coll, name, ns, cb);
		}

		public void Remove (string name, string ns)
		{
			Helper.VoidSync<string,string>(NativeMethods.xmmsc_coll_remove, client, name, ns);
		}

		public void Remove (string name, string ns, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,string>(NativeMethods.xmmsc_coll_remove, client, name, ns, cb);
		}

		public Xmms.List<string> Find (uint mid, string ns)
		{
			return Helper.Sync<Xmms.List<string>,uint,string>(NativeMethods.xmmsc_coll_find, client, mid, ns);
		}

		public void Find (uint mid, string ns, Xmms.Callback<Xmms.List<string>> cb)
		{
			Helper.ASync<Xmms.List<string>,uint,string>(NativeMethods.xmmsc_coll_find, client, mid, ns, cb);
		}

		public void Rename (string from, string to, string ns)
		{
			Helper.VoidSync<string,string,string>(NativeMethods.xmmsc_coll_rename, client, from, to, ns);
		}

		public void Rename (string from, string to, string ns, Xmms.VoidCallback cb)
		{
			Helper.VoidASync<string,string,string>(NativeMethods.xmmsc_coll_rename, client, from, to, ns, cb);
		}

		public Xmms.List<Xmms.Dict> QueryInfos (Xmms.Coll coll, string[] order, uint len, uint start, string[] fetch, string[] group)
		{
			return Helper.Sync<Xmms.List<Xmms.Dict>,HandleRef,string[],uint,uint,string[],string[]>(NativeMethods.xmmsc_coll_query_infos, client, coll, order, len, start, fetch, group);
		}

		public void QueryInfos (Xmms.Coll coll, string[] order, uint len, uint start, string[] fetch, string[] group, Xmms.Callback<Xmms.List<Xmms.Dict>> cb)
		{
			Helper.ASync<Xmms.List<Xmms.Dict>,HandleRef,string[],uint,uint,string[],string[]>(NativeMethods.xmmsc_coll_query_infos, client, coll, order, len, start, fetch, group, cb);
		}

		public Xmms.List<int> QueryIds (Xmms.Coll coll, string[] order, uint len, uint start)
		{
			return Helper.Sync<Xmms.List<int>,HandleRef,string[],uint,uint>(NativeMethods.xmmsc_coll_query_ids, client, coll, order, len, start);
		}

		public void QueryIds (Xmms.Coll coll, string[] order, uint len, uint start, Xmms.Callback<Xmms.List<int>> cb)
		{
			Helper.ASync<Xmms.List<int>,HandleRef,string[],uint,uint>(NativeMethods.xmmsc_coll_query_ids, client, coll, order, len, start, cb);
		}
	}
#endif
}
