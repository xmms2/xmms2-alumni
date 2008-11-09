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

namespace Xmms.Client.Value {
	public sealed class Int32 : Value, IEquatable<Int32> {
		public Int32(int value) : base(
			NativeMethods.xmmsv_new_int(value)
		) {
		}

		public Int32(ValueHandle handle) : base(handle) {
		}

		public int ToInt() {
			int x;

			int s = NativeMethods.xmmsv_get_int(Handle, out x);

			if (s == 0)
				throw new InvalidOperationException();

			return x;
		}

		public override string ToString() {
			return ToInt().ToString();
		}

		public override bool Equals(object obj) {
			return Equals(obj as Int32);
		}

		public bool Equals(Int32 obj) {
			if (ReferenceEquals(obj, null))
				return false;

			return ToInt() == obj.ToInt();
		}

		public override int GetHashCode() {
			return ToInt();
		}
	}
}
