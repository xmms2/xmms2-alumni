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
	public sealed class String : Value {
		public String(string value) : base(
			NativeMethods.xmmsv_new_string(value)
		) {
		}

		public String(ValueHandle handle) : base(handle) {
		}

		public override string ToString() {
			string x;

			int s = NativeMethods.xmmsv_get_string(Handle, out x);

			if (s == 0)
				throw new InvalidOperationException();

			return x;
		}
	}
}
