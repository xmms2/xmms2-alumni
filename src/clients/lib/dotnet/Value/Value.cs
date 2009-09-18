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
	public abstract class Value {
		public enum ValueType {
			None = 0,
			Error = 1,
			Integer = 2,
			String = 3,
			Collection = 4,
			Binary = 5,
			List = 6,
			Dictionary = 7,
		}

		public abstract void Deserialize(Message message);

		internal static void CheckIsType(
			Message message, ValueType expectedType
		) {
			ValueType actualType = (ValueType)message.ReadInteger();

			System.Diagnostics.Debug.Assert(
				expectedType == actualType
			);
		}
	}
}
