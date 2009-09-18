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
	public class Dictionary<T> : Value, IDictionary<string, T>
		where T : Value, new()
	{
		public Dictionary() {
			this.items = new Dictionary<string, T>();
		}

		public override void Deserialize(Message message) {
			CheckIsType(message, ValueType.Integer);

			int length = message.ReadInteger();

			while (length-- > 0) {
				string key = message.ReadString();

				T item = new T();
				item.Deserialize(message);

				items[key] = item;
			}
		}

		public T this[string key] {
			get { return items[key]; }
			set { items[key] = value; }
		}

		public bool IsReadOnly {
			get { return false; }
		}

		public int Count {
			get { return items.Count; }
		}

		public ICollection<string> Keys {
			get { return items.Keys; }
		}

		public ICollection<T> Values {
			get { return items.Values; }
		}

		public bool TryGetValue(string key, out T value) {
			return items.TryGetValue(key, out value);
		}

		public void Add(string key, T value) {
			items.Add(key, value);
		}

		public void Add(KeyValuePair<string, T> pair) {
			items.Add(pair);
		}

		public bool Remove(string key) {
			return items.Remove(key);
		}

		public bool Remove(KeyValuePair<string, T> pair) {
			return items.Remove(pair);
		}

		public void Clear() {
			items.Clear();
		}

		public bool ContainsKey(string key) {
			return items.ContainsKey(key);
		}

		public bool Contains(KeyValuePair<string, T> pair) {
			return items.Contains(pair);
		}

		public void CopyTo(KeyValuePair<string, T>[] array, int arrayIndex) {
			items.CopyTo(array, arrayIndex);
		}

		public IEnumerator<KeyValuePair<string, T>> GetEnumerator() {
			return items.GetEnumerator();
		}

		IEnumerator IEnumerable.GetEnumerator() {
			return items.GetEnumerator();
		}

		private readonly IDictionary<string, T> items;
	}
}
