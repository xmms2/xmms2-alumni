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
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Net.Sockets;

namespace Xmms.Client {
	public class Client {
		public Client(string name) {
			this.name = name;

			sendQueue = new Queue<Message>();
			results = new List<Result>();

			mainObject = new Generated.Main(this);
			playbackObject = new Generated.Playback(this);
			playlistObject = new Generated.Playlist(this);
			medialibObject = new Generated.Medialib(this);
		}

		public Socket Socket {
			get { return socket; }
		}

		public Generated.Playback Playback {
			get { return playbackObject; }
		}

		public Generated.Playlist Playlist {
			get { return playlistObject; }
		}

		public Generated.Medialib Medialib {
			get { return medialibObject; }
		}

		public void Connect() {
			Connect(
				System.Environment.GetEnvironmentVariable("XMMS_PATH")
			);
		}

		public void Connect(string ipc_path) {
			System.Text.RegularExpressions.Regex re =
				new System.Text.RegularExpressions.Regex(
					@"^(\w+):\/\/([^:]+)(:(\d+))?$"
				);

			System.Text.RegularExpressions.Match match =
				re.Match(ipc_path);

			string protocol = match.Groups[1].Value;

			switch (protocol) {
			case "tcp":
				string host = match.Groups[2].Value;
				int port = int.Parse(match.Groups[4].Value);

				socket = new Socket(
					AddressFamily.InterNetwork, SocketType.Stream,
					ProtocolType.Tcp
				);

				socket.Connect(host, port);

				break;
			default:
				throw new NotImplementedException();
			}

			Result result = mainObject.Hello(protocolVersion, name);
			result.Wait();
		}

		public bool IOWantOut() {
			return sendQueue.Count > 0;
		}

		public void IOOutHandle() {
			Message message = sendQueue.Peek();

			// write the next chunk of the message.
			bool done = message.WriteChunk(socket);

			// if this message was written completely,
			// we remove it from the queue.
			if (done)
				sendQueue.Dequeue();
		}

		public void IOInHandle() {
			if (readMessage == null)
				readMessage = new Message();

			bool readAllChunks = readMessage.ReadChunk(socket);

			if (readAllChunks) {
				processMessage(readMessage);
				readMessage = null;
			}
		}

		internal void SendMessage(Message message, Result result) {
			message.Assemble();

			sendQueue.Enqueue(message);
			results.Add(result);
		}

		internal void WaitFor(Result result) {
			flushSendQueue();

			bool foundMessage;

			do {
				Message message = new Message();
				bool readAllChunks;

				do {
					readAllChunks = message.ReadChunk(socket);
				} while (!readAllChunks);

				processMessage(message);

				foundMessage = message.Cookie == result.Cookie;
			} while (!foundMessage);
		}

		internal uint GetNextCookie() {
			uint cookie = nextCookie;

			if (nextCookie == uint.MaxValue)
				nextCookie = 0;
			else
				nextCookie++;

			return cookie;
		}

		private void flushSendQueue() {
			while (IOWantOut())
				IOOutHandle();
		}

		private void processMessage(Message message) {
			message.PrepareForReadWrite();

			// find the result that this message refers to
			int foundIndex = -1;
			Result foundResult = null;

			for (int i = 0; i < results.Count; i++) {
				Result r = results[i];

				if (r.Cookie == message.Cookie) {
					foundIndex = i;
					foundResult = r;
					break;
				}
			}

			System.Diagnostics.Debug.Assert(foundResult != null);

			foundResult.ProcessReply(message);

			results.RemoveAt(foundIndex);
		}

		private static readonly int protocolVersion = 16;

		/// <summary>
		/// The client's name.
		/// </summary>
		private readonly string name;
		private readonly Queue<Message> sendQueue;
		private readonly List<Result> results;

		private readonly Generated.Main mainObject;
		private readonly Generated.Playback playbackObject;
		private readonly Generated.Playlist playlistObject;
		private readonly Generated.Medialib medialibObject;

		private Socket socket;
		private uint nextCookie;
		private Message readMessage;
	}

	// FIXME: Kill me.
	public class collection {
	}
}
