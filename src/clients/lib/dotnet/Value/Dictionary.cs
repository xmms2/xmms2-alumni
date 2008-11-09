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

using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;

namespace Xmms.Client.Value {
	public sealed class Dictionary<T> : Value,
		IEnumerable<KeyValuePair<string, T>>
		where T : Value {

		#region Enumerator

		public struct Enumerator : IEnumerator<KeyValuePair<string, T>> {
			internal Enumerator(
				DictionaryIteratorHandle handle,
				ConstructorInfo valueConstructor
			) {
				this.handle = handle;
				this.valueConstructor = valueConstructor;

				isFirst = true;

				// Just like MSFT's Dictionary<T>.Enumerator.Current,
				// we return an empty KeyValuePair in case the
				// enumerator points to an invalid item.
				currentValue = invalidValue = new KeyValuePair<string, T>(
					null, default(T)
				);
			}

			public KeyValuePair<string, T> Current {
				get { return currentValue; }
			}

			object IEnumerator.Current {
				get { return currentValue; }
			}

			public void Dispose() {
				// FIXME: Hum, what do we do in here?
			}

			public bool MoveNext() {
				if (isFirst)
					isFirst = false;
				else
					NativeMethods.xmmsv_dict_iter_next(handle);

				int s = NativeMethods.xmmsv_dict_iter_valid(handle);

				if (s == 0) {
					currentValue = invalidValue;

					return false;
				}

				string key;
				ValueHandle value;

				s = NativeMethods.xmmsv_dict_iter_pair(
					handle, out key, out value
				);

				if (s == 0)
					throw new InvalidOperationException();

				NativeMethods.xmmsv_ref(value.DangerousGetHandle());

				T value2 = Value.CreateValue<T>(value, valueConstructor);

				currentValue = new KeyValuePair<string, T>(key, value2);

				return true;
			}

			void IEnumerator.Reset() {
				NativeMethods.xmmsv_dict_iter_first(handle);

				isFirst = true;

				currentValue = invalidValue;
			}

			private readonly DictionaryIteratorHandle handle;
			private readonly ConstructorInfo valueConstructor;
			private bool isFirst;
			private KeyValuePair<string, T> currentValue;
			private readonly KeyValuePair<string, T> invalidValue;
		}

		#endregion

		public Dictionary() : base(NativeMethods.xmmsv_new_dict()) {
			valueConstructor = GetValueConstructor(typeof(T));
		}

		public Dictionary(ValueHandle handle) : base(handle) {
			valueConstructor = GetValueConstructor(typeof(T));
		}

		public T this[string key] {
			get {
				if (key == null)
					throw new ArgumentNullException("key");

				ValueHandle value;

				int s = NativeMethods.xmmsv_dict_get(
					Handle, key, out value
				);

				if (s == 0)
					throw new KeyNotFoundException();

				return Value.CreateValue<T>(value, valueConstructor);
			}

			set {
				Add(key, value);
			}
		}

		public int Count {
			get {
				return NativeMethods.xmmsv_dict_get_size(Handle);
			}
		}

		public void Add(string key, T value) {
			if (key == null)
				throw new ArgumentNullException("key");

			int s = NativeMethods.xmmsv_dict_insert(
				Handle, key, value.Handle
			);

			if (s == 0)
				throw new InvalidOperationException();
		}

		public bool Remove(string key) {
			if (key == null)
				throw new ArgumentNullException("key");

			int s = NativeMethods.xmmsv_dict_remove(Handle, key);

			return s != 0;
		}

		public void Clear() {
			int s = NativeMethods.xmmsv_dict_clear(Handle);

			if (s == 0)
				throw new InvalidOperationException();
		}

		public Enumerator GetEnumerator() {
			DictionaryIteratorHandle iterator;

			int s = NativeMethods.xmmsv_get_dict_iter(
				Handle, out iterator
			);

			if (s == 0)
				throw new InvalidOperationException();

			return new Enumerator(iterator, valueConstructor);
		}

		IEnumerator IEnumerable.GetEnumerator() {
			return GetEnumerator();
		}

		IEnumerator<KeyValuePair<string, T>>
		IEnumerable<KeyValuePair<string, T>>.GetEnumerator() {
			return GetEnumerator();
		}

		public Dictionary<Value> ToPropertyDictionary() {
			IntPtr valuePtr = NativeMethods.xmmsv_ref(
				NativeMethods.xmmsv_propdict_to_dict(
					Handle, IntPtr.Zero
				)
			);

			return new Dictionary<Value>(new ValueHandle(valuePtr));
		}

		private readonly ConstructorInfo valueConstructor;
	}
}
