using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Net.Sockets;

namespace Xmms.Client {
	public class Client {
		public Client() {
			sendQueue = new Queue<Message>();
			results = new List<Result>();
		}

		public void Connect() {
			//connection = new TcpClient("brimstone", 9667);
			socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
			socket.Connect("brimstone", 9667);

			VoidResult result = hello();
			result.Wait();

			IntegerListResult result2 = listPlaylistEntries();
			result2.Wait();

			Console.WriteLine("playlist contents:");

			foreach (var id in result2.Value) {
				Console.WriteLine(id);
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

		private void flushSendQueue() {
			while (ioWantOut())
				ioOutHandle();
		}

		private void ioOutHandle() {
			Message message = sendQueue.Peek();

			// write the next chunk of the message.
			bool done = message.WriteChunk(socket);

			// if this message was written completely,
			// we remove it from the queue.
			if (done)
				sendQueue.Dequeue();
		}

		private bool ioWantOut() {
			return sendQueue.Count > 0;
		}

		private VoidResult hello() {
			Message message = createMessage();

			message.ObjectID = 1;
			message.CommandID = 32;
			message.Write((int)15); // protocol version
			message.Write("foobar"); // client name

			VoidResult result = new VoidResult(this, message.Cookie);

			SendMessage(message, result);

			return result;
		}

		private IntegerListResult listPlaylistEntries() {
			Message message = createMessage();

			message.ObjectID = 2;
			message.CommandID = 43;
			message.Write("_active");

			IntegerListResult result = new IntegerListResult(
				this, message.Cookie
			);

			SendMessage(message, result);

			return result;
		}

		internal Message createMessage() {
			Message message = new Message();

			message.Cookie = getNextCookie();
			message.PrepareForReadWrite();

			return message;
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

		private int getNextCookie() {
			int cookie = nextCookie;

			if (nextCookie == int.MaxValue)
				nextCookie = 0;
			else
				nextCookie++;

			return cookie;
		}

		private Socket socket;
		private readonly Queue<Message> sendQueue;
		private readonly List<Result> results;
		private int nextCookie;
	}
}
