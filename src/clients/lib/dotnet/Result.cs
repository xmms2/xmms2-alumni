//
//  .NET bindings for the XMMS2 client library
//
//  Copyright (C) 2005 Daniel Svensson, <nano@nittioonio.nu>
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
using System.Runtime.InteropServices;

namespace Xmms.Client {

	public delegate void VoidCallback();
	public delegate void Callback<A1>(A1 arg1);

	// Used for Signals/Broadcasts.
	public delegate bool Callback2<A1>(A1 arg1);

	/// <summary>
	/// A result that doesn't hold a value.
	/// </summary>
	public class Result {
		public Result(ResultHandle resultHandle) {
			this.resultHandle = resultHandle;
		}

		public void Fetch() {
			NativeMethods.xmmsc_result_wait(resultHandle);
		}

		public void Fetch(VoidCallback callback) {
			this.callback = callback;

			NativeMethods.xmmsc_result_notifier_set(
				resultHandle, handleSignal, IntPtr.Zero
			);
		}

		private int handleSignal(
			IntPtr valuePtr, IntPtr user_data
		) {
#if DEBUG
			ValueHandle value = new ValueHandle(valuePtr);
			ValueType type = NativeMethods.xmmsv_get_type(value);

			System.Diagnostics.Debug.Assert(type == ValueType.None);
#endif

			callback();

			return 0;
		}

		private readonly ResultHandle resultHandle;
		private VoidCallback callback;
	}

	public class Result<T> where T : Value.Value  {
		public Result(ResultHandle resultHandle) {
			this.resultHandle = resultHandle;

			valueConstructor = typeof(T).GetConstructor(
				new Type[] { typeof(ValueHandle) }
			);
		}

		public T Fetch() {
			NativeMethods.xmmsc_result_wait(resultHandle);

			ValueHandle valueHandle = new ValueHandle(
				NativeMethods.xmmsc_result_get_value(resultHandle)
			);

			return (T)valueConstructor.Invoke(
				new object[] { valueHandle }
			);
		}

		public void Fetch(Callback<T> callback) {
			this.callback = callback;

			NativeMethods.xmmsc_result_notifier_set(
				resultHandle, handleSignal,
				IntPtr.Zero
			);
		}

		private int handleSignal(
			IntPtr valuePtr, IntPtr user_data
		) {
			ValueHandle valueHandle = new ValueHandle(valuePtr);

			T value = (T)valueConstructor.Invoke(
				new object[] { valueHandle }
			);

			callback(value);

			return 1;
		}

		protected readonly ResultHandle resultHandle;
		private readonly ConstructorInfo valueConstructor;
		private Callback<T> callback;
	}

	public abstract class DisconnectableResult<T> : Result<T> where T : Value.Value {
		protected DisconnectableResult(
			ResultHandle resultHandle
		) : base(resultHandle) {
		}

		public void Disconnect() {
			NativeMethods.xmmsc_result_disconnect(resultHandle);
		}
	}

	public class SignalResult<T> : DisconnectableResult<T> where T : Value.Value {
		public SignalResult(
			ResultHandle resultHandle
		) : base(resultHandle) {
		}
	}

	public class BroadcastResult<T> : DisconnectableResult<T> where T : Value.Value {
		public BroadcastResult(
			ResultHandle resultHandle
		) : base(resultHandle) {
		}
	}
}
