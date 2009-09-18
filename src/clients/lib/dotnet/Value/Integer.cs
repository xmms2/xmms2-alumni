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

namespace Xmms.Client.Value {
	public class Integer : Value {
		public static implicit operator int(Integer integerValue) {
			return integerValue.value;
		}

		public override void Deserialize(Message message) {
			CheckIsType(message, ValueType.Integer);

			value = message.ReadInteger();
		}

		private int value;
	}
}
