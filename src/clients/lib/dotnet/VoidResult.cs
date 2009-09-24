//
//  .NET bindings for the XMMS2 client library
//
//  Copyright (C) 2009 Tilman Sauerbeck, <tilman@xmms.org>
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
	public class VoidResult : Result {
		public event EventHandler Processed;

		public VoidResult(
			Client client, uint cookie
		) : base(client, cookie) {
		}

		protected override void GetValue(Message message) {
			// Nothing
		}

		protected override void OnProcessed() {
			OnProcessed(EventArgs.Empty);
		}

		protected void OnProcessed(EventArgs e) {
			if (Processed != null)
				Processed(this, e);
		}
	}
}
