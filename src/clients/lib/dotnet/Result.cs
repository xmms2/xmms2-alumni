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
	public abstract class Result {
		public Result(Client client, uint cookie) {
			this.client = client;
			this.cookie = cookie;
		}

		public uint Cookie {
			get { return cookie; }
		}

 		public void Wait() {
			client.WaitFor(this);
		}

		internal void ProcessReply(Message message) {
			if (message.CommandID == 0) {
				// reply
				GetValue(message);
			} else if (message.CommandID == 1) {
				// error
				//isError = true;
			}

			OnProcessed();
		}

		protected abstract void GetValue(Message message);
		protected abstract void OnProcessed();

		private readonly Client client;
		private readonly uint cookie;
	}
}
