using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;


namespace Xmms.Client {
	public class IdList : Coll, IList<uint>, IEnumerator<uint>
	{
		private uint iterator;
		private bool is_first;

		public IdList (HandleRef idlist) : base (idlist)
		{
			iterator = 0;
			is_first = true;
		}

		~IdList ()
		{
		}

		public void Dispose ()
		{
		}

		public int Count
		{
			get {
				return (int) NativeMethods.xmmsc_coll_idlist_get_size (raw);
			}
		}

		public bool IsFixedSize
		{
			get {
				return false;
			}
		}
		
		public bool IsReadOnly
		{
			get {
				return false;
			}
		}

		public bool IsSynchronized
		{
			get {
				return false;
			}
		}
		public object SyncRoot
		{
			get {
				return null;
			}
		}
		public void CopyTo (uint[] array, int index)
		{
			if (index < 0)
				throw new ArgumentOutOfRangeException ("index can't be less than zero");
			if (array == null)
				throw new ArgumentNullException ();
			if (array.Rank != 1)
				throw new ArgumentException ("Array is not 1-dimensional");
			if (array.Length < (index + Count) || array.Length < index)
				throw new ArgumentException ("Array is too small");

			for (uint i = 0; i < Count; i++) {
				uint tmp;
				if (!NativeMethods.xmmsc_coll_idlist_get_index (raw, i, out tmp)) {
					throw new Exception ("Unable to get value from IdList index: " + i);
				}
				array[index+i] = tmp;
			}
		}

		IEnumerator IEnumerable.GetEnumerator()
		{
			return (IEnumerator) this;
		}

		IEnumerator<uint> IEnumerable<uint>.GetEnumerator()
		{
			return (IEnumerator<uint>) this;
		}

		object IEnumerator.Current
		{
			get {
				return (object) Current;
			}
		}

		public uint Current
		{
			get {
				uint tmp;
				if (!NativeMethods.xmmsc_coll_idlist_get_index (raw, iterator, out tmp)) {
					throw new Exception ("Unable to get value from IdList index: " + iterator);
				}
				return tmp;
			}
		}

		/* maybe init iterator to -1 instead of having is_filst? */
		bool IEnumerator.MoveNext ()
		{
			if (is_first) {
				is_first = false;
			} else {
				iterator++;
			}

			return (iterator < (Count-1));
		}

		void IEnumerator.Reset ()
		{
			iterator = 0;
			is_first = true;
		}

		public void Add (uint id)
		{
			NativeMethods.xmmsc_coll_idlist_append (raw, id);
		}

		public void Insert (int index, uint id)
		{
			NativeMethods.xmmsc_coll_idlist_insert (raw, id, (uint) index);
		}

		public void RemoveAt (int index)
		{
			NativeMethods.xmmsc_coll_idlist_remove (raw, (uint) index);
		}

		private bool Find (uint id, out uint index)
		{
			bool ret = false;
			uint count = (uint) Count;
			index = 0;

			for (uint i = 0; i < count; i++) {
				uint tmp;
				if (!NativeMethods.xmmsc_coll_idlist_get_index (raw, iterator, out tmp))
					throw new Exception ("Unable to get value from IdList index: " + iterator);

				if (tmp == id) {
					index = i;
					ret = true;
					break;
				}
			}

			return ret;
		}

		public bool Remove (uint id)
		{
			uint index;
			bool ret = false;
			if (Find (id, out index)) {
				NativeMethods.xmmsc_coll_idlist_remove (raw, (uint) index);
				ret = true;
			}
			return ret;
		}

		public int IndexOf (uint id)
		{
			int ret = -1;
			uint index;

			if (Find (id, out index)) {
				ret = (int) index;
			} 

			return ret;
		}

		public bool Contains (uint id)
		{
			uint index;
			return Find (id, out index);
		}

		public void Clear ()
		{
			NativeMethods.xmmsc_coll_idlist_clear (raw);
		}

		public uint this[int index]
		{
			get {
				uint tmp;
				if (!NativeMethods.xmmsc_coll_idlist_get_index (raw, (uint) index, out tmp))
					throw new Exception ("Unable to get value from IdList index: " + index);
				return tmp;
			}
			set {
				if (!NativeMethods.xmmsc_coll_idlist_set_index (raw, (uint) index, value))
					throw new Exception ("Unable to set value to IdList index: " + index);
			}
		}
	}

	public class CollType
	{
		public CollType ()
		{
		}
	}

	public abstract class Unary : Coll
	{
		protected Unary (CollectionType type) : base (type)
		{
		}

		protected Unary (CollectionType type, Coll op) : base (type)
		{
			Operand = op;
		}

		public void RemoveOperand ()
		{
			try {
				NativeMethods.xmmsc_coll_remove_operand (raw, Operand.raw);
			}
			catch (Exception) {
				/* FIXME: throw error if none? */
			}
		}

		public Coll Operand
		{
			get {
				// Find the operand
				NativeMethods.xmmsc_coll_operand_list_save (raw);
				NativeMethods.xmmsc_coll_operand_list_first (raw);
				IntPtr op;
				if (!NativeMethods.xmmsc_coll_operand_list_entry (raw, out op)) {
					op = IntPtr.Zero;
				}
				NativeMethods.xmmsc_coll_operand_list_restore (raw);

				if (op == IntPtr.Zero) {
					/* FIXME: throw proper error if none */
					throw new Exception ("operand not found o_O");
				}

				return new Coll (op);
			}

			set {
				RemoveOperand();
				NativeMethods.xmmsc_coll_add_operand (raw, value.raw);
			}
		}
	}

	public class Filter : Unary
	{
		public Filter (CollectionType type) :
			base (type) {}
		public Filter (CollectionType type, Coll operand) :
			base (type, operand) {}

		public Filter (CollectionType type, Coll operand, string field) : base (type, operand)
		{
			SetAttribute ("field", field);
		}
		public Filter (CollectionType type, Coll operand, string field, string val) : base (type, operand)
		{
			SetAttribute ("field", field);
			SetAttribute ("value", val);
		}
		public Filter (CollectionType type, Coll operand, string field, string val, bool case_sensitive) : base (type, operand)
		{
			SetAttribute ("field", field);
			SetAttribute ("value", val);
			if (case_sensitive) {
				SetAttribute ("case-sensitive", "true");
			}
		}
	}

	public class Reference : Coll
	{
		public Reference () :
			base (CollectionType.Reference) {}

		public Reference (string name, string ns) : base (CollectionType.Reference)
		{
			SetAttribute ("reference", name);
			SetAttribute ("namespace", ns);
		}
	}

	public class Universe : Reference
	{
		public Universe () :
			base ( "All Media", Namespace.Collections) {}
	}

	public class Union : Nary
	{
		public Union () :
			base (CollectionType.Union) {}
		public Union (params Coll[] colls) :
			base (CollectionType.Union, colls) {}

	}

	public class Intersection : Nary
	{
		public Intersection () :
			base (CollectionType.Intersection) {}
		public Intersection (params Coll[] colls) :
			base (CollectionType.Intersection, colls) {}
	}

	public class Complement : Unary
	{
		public Complement () :
			base (CollectionType.Complement) {}
		public Complement (Coll op) :
			base (CollectionType.Complement, op) {}
	}

	public class Has : Filter
	{
		public Has () :
			base (CollectionType.Has) {}
		public Has (Coll op) :
			base (CollectionType.Has, op) {}
		public Has (Coll op, string field) :
			base (CollectionType.Has, op, field) {}
	}

	public class Smaller : Filter
	{
		public Smaller () :
			base (CollectionType.Smaller) {}
		public Smaller (Coll op) :
			base (CollectionType.Smaller, op) {}
		public Smaller (Coll op, string field) :
			base (CollectionType.Smaller, op, field) {}
		public Smaller (Coll op, string field, string val) :
			base (CollectionType.Smaller, op, field, val) {}
	}

	public class Greater : Filter
	{
		public Greater () :
			base (CollectionType.Greater) {}
		public Greater (Coll op) :
			base (CollectionType.Greater, op) {}
		public Greater (Coll op, string field) :
			base (CollectionType.Greater, op, field) {}
		public Greater (Coll op, string field, string val) :
			base (CollectionType.Greater, op, field, val) {}
	}

	public class Match : Filter
	{
		public Match () :
			base (CollectionType.Match) {}
		public Match (Coll op) :
			base (CollectionType.Match, op) {}
		public Match (Coll op, string field) :
			base (CollectionType.Match, op, field) {}
		public Match (Coll op, string field, string val) :
			base (CollectionType.Match, op, field, val) {}
		public Match (Coll op, string field, string val, bool case_sensitive) :
			base (CollectionType.Match, op, field, val, case_sensitive) {}
	}

	public class Contains : Filter
	{
		public Contains () : 
			base (CollectionType.Contains) {}
		public Contains (Coll op) :
			base (CollectionType.Contains, op) {}
		public Contains (Coll op, string field) :
			base (CollectionType.Contains, op, field) {}
		public Contains (Coll op, string field, string val) :
			base (CollectionType.Contains, op, field, val) {}
		public Contains (Coll op, string field, string val, bool case_sensitive) :
			base (CollectionType.Contains, op, field, val, case_sensitive) {}
	}


	public abstract class Nary : Coll, IEnumerator<CollType>, IEnumerable<CollType>
	{
		private bool is_first = true;

		protected Nary (CollectionType type) : base (type)
		{
		}

		protected Nary (CollectionType type, params Coll[] colls) : base (type)
		{
			foreach (Coll c in colls) {
				AddOperand (c);
			}
		}

		public void AddOperand (Coll op)
		{
			NativeMethods.xmmsc_coll_add_operand (raw, op.raw);
		}
		public void RemoveOperand (Coll op)
		{
			NativeMethods.xmmsc_coll_remove_operand (raw, op.raw);
		}

		public void Dispose ()
		{
		}

		IEnumerator IEnumerable.GetEnumerator()
		{
			return (IEnumerator<CollType>) this;
		} 

		object IEnumerator.Current
		{
			get {
				return null;
			}
		}

		bool IEnumerator.MoveNext ()
		{
			if (is_first) {
				is_first = false;
			} else {
				if (!NativeMethods.xmmsc_coll_operand_list_next (raw))
					throw new Exception ("Internal error!");
			}

			IntPtr tmp;
			return NativeMethods.xmmsc_coll_operand_list_entry (raw, out tmp);
		}

		void IEnumerator.Reset ()
		{
			if (!NativeMethods.xmmsc_coll_operand_list_first (raw))
				throw new Exception ("Internal error!");
			is_first = true;
		}

		IEnumerator<CollType> IEnumerable<CollType>.GetEnumerator()
		{
			return (IEnumerator<CollType>) this;
		} 

		CollType IEnumerator<CollType>.Current
		{
			get {
				IntPtr op;
				if (!NativeMethods.xmmsc_coll_operand_list_entry (raw, out op)) {
					throw new Exception ("Access out of the operand list!");
				}

				return new CollType();
			}
		}

		public void Save ()
		{
			if (!NativeMethods.xmmsc_coll_operand_list_save (raw))
				throw new Exception ("Internal error!");
		}

		public void Restore ()
		{
			if (!NativeMethods.xmmsc_coll_operand_list_restore (raw))
				throw new Exception ("Internal error!");
		}
	}

	public class Coll
	{
		/* syntactic sugar */
		public static class Namespace {
			public static string All = "*";
			public static string Collections = "Collections";
			public static string Playlists = "Playlists";
		}

		internal HandleRef raw;

		public Coll (CollectionType type)
		{
			raw = new HandleRef (this, NativeMethods.xmmsc_coll_new (type));
		}

		public Coll (Coll coll) 
		{
			raw = coll.raw;
			NativeMethods.xmmsc_coll_ref (raw);
		}

		public Coll (HandleRef coll)
		{
			raw = coll;
			NativeMethods.xmmsc_coll_ref (raw);
		}

		public Coll (IntPtr coll)
		{
			raw = new HandleRef (this, coll);
			NativeMethods.xmmsc_coll_ref (raw);
		}

		~Coll ()
		{
			NativeMethods.xmmsc_coll_unref (raw);
		}

		public static implicit operator HandleRef (Coll c)
		{
			return c.raw;
		}

		public string this[string key]
		{
			get {
				string val;
				if (!NativeMethods.xmmsc_coll_attribute_get (raw, key, out val )) {
					throw new Exception ("No such attribute: " + key);
				}

				return val;
			}
			set {
				NativeMethods.xmmsc_coll_attribute_set (raw, key, value);
			}
		}

		public IdList GetIdList ()
		{
			return new IdList (raw);
		}

		public void SetAttribute (string key, string val)
		{
			this[key] = val;
		}

		public string GetAttribute (string key)
		{
			return this[key];
		}
	}
}


