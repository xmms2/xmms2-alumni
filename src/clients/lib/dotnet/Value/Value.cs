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
using System.Reflection;

namespace Xmms.Client.Value {
	public abstract class Value {
		protected Value(ValueHandle handle) {
			Handle = handle;
		}

		internal static T CreateValue<T>(
			ValueHandle valueHandle,
			ConstructorInfo valueConstructor
		) where T : Value {
			if (valueConstructor == null)
				return (T)fromHandle(valueHandle);
			else
				return (T)valueConstructor.Invoke(
					new object[] { valueHandle }
				);
		}

		private static Value fromHandle(ValueHandle valueHandle) {
			switch (NativeMethods.xmmsv_get_type(valueHandle)) {
			case ValueType.UInt32:
				return new UInt32(valueHandle);
			case ValueType.Int32:
				return new Int32(valueHandle);
			case ValueType.String:
				return new String(valueHandle);
			case ValueType.List:
				return new List<Value>(valueHandle);
			case ValueType.Dictionary:
				return new Dictionary<Value>(valueHandle);
			default:
				throw new ArgumentException();
			}
		}

		protected static ConstructorInfo GetValueConstructor(
			Type type
		) {
			return type.GetConstructor(
				new Type[] { typeof(ValueHandle) }
			);
		}

		internal readonly ValueHandle Handle;
	}
}
