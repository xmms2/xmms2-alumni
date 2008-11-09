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
	public sealed class UInt32 : Value, IEquatable<UInt32> {
		public UInt32(uint value) : base(
			NativeMethods.xmmsv_new_uint(value)
		) {
		}

		public UInt32(ValueHandle handle) : base(handle) {
		}

		public uint ToUInt() {
			uint x;

			int s = NativeMethods.xmmsv_get_uint(Handle, out x);

			if (s == 0)
				throw new InvalidOperationException();

			return x;
		}

		public override string ToString() {
			return ToUInt().ToString();
		}

		public override bool Equals(object obj) {
			return Equals(obj as UInt32);
		}

		public bool Equals(UInt32 obj) {
			if (ReferenceEquals(obj, null))
				return false;

			return ToUInt() == obj.ToUInt();
		}

		public override int GetHashCode() {
			return ToUInt().GetHashCode();
		}
	}
}
