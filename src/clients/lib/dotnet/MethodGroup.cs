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

namespace Xmms.Client {
	public abstract class MethodGroup {
		protected MethodGroup(Client client, uint id) {
			Client = client;
			ID = id;
		}

		protected Message CreateMessage() {
			Message message = new Message();

			message.Cookie = Client.GetNextCookie();
			message.PrepareForReadWrite();

			return message;
		}

		protected readonly Client Client;
		protected readonly uint ID;
	}
}
