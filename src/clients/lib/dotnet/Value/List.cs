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
	public sealed class List<T> : Value,
		IEnumerable<T>
		where T : Value {

		#region Enumerator

		public struct Enumerator : IEnumerator<T> {
			public Enumerator(
				ListIteratorHandle handle,
				ConstructorInfo valueConstructor
			) {
				this.handle = handle;
				this.valueConstructor = valueConstructor;

				isFirst = true;

				// Just like MSFT's List<T>.Enumerator.Current, we
				// return default(T) in case the enumerator points
				// to an invalid item.
				currentValue = invalidValue = default(T);
			}

			public T Current {
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
					NativeMethods.xmmsv_list_iter_next(handle);

				int s = NativeMethods.xmmsv_list_iter_valid(handle);

				if (s == 0) {
					currentValue = invalidValue;

					return false;
				}

				ValueHandle value;

				s = NativeMethods.xmmsv_list_iter_entry(
					handle, out value
				);

				if (s == 0)
					throw new InvalidOperationException();

				NativeMethods.xmmsv_ref(value.DangerousGetHandle());

				currentValue = Value.CreateValue<T>(
					value, valueConstructor
				);

				return true;
			}

			public void Reset() {
				NativeMethods.xmmsv_list_iter_first(handle);

				isFirst = true;

				currentValue = invalidValue;
			}

			private readonly ListIteratorHandle handle;
			private readonly ConstructorInfo valueConstructor;
			private bool isFirst;
			private T currentValue;
			private readonly T invalidValue;
		}

		#endregion

		public List() : base(NativeMethods.xmmsv_new_list()) {
			valueConstructor = GetValueConstructor(typeof(T));
		}

		public List(ValueHandle handle) : base(handle) {
			valueConstructor = GetValueConstructor(typeof(T));
		}

		public T this[int index] {
			get {
				ValueHandle value;

				int s = NativeMethods.xmmsv_list_get(
					Handle, index, out value
				);

				if (s == 0)
					throw new ArgumentOutOfRangeException("index");

				return Value.CreateValue<T>(value, valueConstructor);
			}

			set {
				int s = NativeMethods.xmmsv_list_set(
					Handle, index, value.Handle
				);

				if (s == 0)
					throw new ArgumentOutOfRangeException("index");
			}
		}

		public int Count {
			get {
				return NativeMethods.xmmsv_list_get_size(Handle);
			}
		}

		public void Add(T item) {
			NativeMethods.xmmsv_list_append(Handle, item.Handle);
		}

		public void Insert(int index, T item) {
			NativeMethods.xmmsv_list_insert(Handle, index, item.Handle);
		}

		public void RemoveAt(int index) {
			NativeMethods.xmmsv_list_remove(Handle, index);
		}

		public void Clear() {
			NativeMethods.xmmsv_list_clear(Handle);
		}

		public Enumerator GetEnumerator() {
			ListIteratorHandle iterator;

			int s = NativeMethods.xmmsv_get_list_iter(
				Handle, out iterator
			);

			if (s == 0)
				throw new InvalidOperationException();

			return new Enumerator(iterator, valueConstructor);
		}

		IEnumerator IEnumerable.GetEnumerator() {
			return GetEnumerator();
		}

		IEnumerator<T> IEnumerable<T>.GetEnumerator() {
			return GetEnumerator();
		}

		private readonly ConstructorInfo valueConstructor;
	}
}
