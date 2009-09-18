//
//  .NET bindings for the XMMS2 client library
//
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

using System.Collections;
using System.Collections.Generic;

namespace Xmms.Client.Value {
	public class List<T> : Value, IList<T>
		where T : Value, new()
	{
		public List() {
			this.items = new List<T>();
		}

		public override void Deserialize(Message message) {
			CheckIsType(message, ValueType.Integer);

			int length = message.ReadInteger();

			while (length-- > 0) {
				T item = new T();
				item.Deserialize(message);

				items.Add(item);
			}
		}

		public T this[int position] {
			get { return items[position]; }
			set { items[position] = value; }
		}

		public bool IsReadOnly {
			get { return false; }
		}

		public int Count {
			get { return items.Count; }
		}

		public void Add(T item) {
			items.Add(item);
		}

		public void Insert(int position, T item) {
			items.Insert(position, item);
		}

		public void RemoveAt(int position) {
			items.RemoveAt(position);
		}

		public bool Remove(T item) {
			return items.Remove(item);
		}

		public void Clear() {
			items.Clear();
		}

		public int IndexOf(T item) {
			return items.IndexOf(item);
		}

		public bool Contains(T item) {
			return items.Contains(item);
		}

		public void CopyTo(T[] array, int arrayIndex) {
			items.CopyTo(array, arrayIndex);
		}

		public IEnumerator<T> GetEnumerator() {
			return items.GetEnumerator();
		}

		IEnumerator IEnumerable.GetEnumerator() {
			return items.GetEnumerator();
		}

		private readonly IList<T> items;
	}
}
