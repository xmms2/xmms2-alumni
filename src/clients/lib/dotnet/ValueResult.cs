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

namespace Xmms.Client {
	public class ValueResult<T> : Result where T : Value.Value, new() {
		public ValueResult(
			Client client, uint cookie
		) : base(client, cookie) {
		}

		public T Value {
			get {
				if (!hasValue)
					throw new InvalidOperationException();

				return value;
			}
		}

		protected override void GetValue(Message message) {
			value = new T();

			value.Deserialize(message, true);

			hasValue = true;
		}

		private T value;
		private bool hasValue;
	}
}
