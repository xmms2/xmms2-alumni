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

		public static Value Deserialize(Message message) {
			ValueType type = (ValueType)message.ReadUnsignedInteger();
			Value value;

			switch (type) {
			case ValueType.None:
				throw new NotImplementedException();
			case ValueType.Error:
				throw new NotImplementedException();
			case ValueType.Integer:
				value = new Integer();
				break;
			case ValueType.String:
				value = new String();
				break;
			case ValueType.Collection:
				throw new NotImplementedException();
			case ValueType.Binary:
				throw new NotImplementedException();
			case ValueType.List:
				value = new UnknownList();
				break;
			case ValueType.Dictionary:
				value = new UnknownDictionary();
				break;
			default:
				throw new NotImplementedException();
			}

			value.Deserialize(message, false);

			return value;
		}

		public abstract void Deserialize(Message message, bool readType);

		internal static void CheckIsType(
			Message message, ValueType expectedType
		) {
			ValueType actualType = (ValueType)message.ReadUnsignedInteger();

			System.Diagnostics.Debug.Assert(
				expectedType == actualType
			);
		}
	}
}
