using System;
using System.IO;
using System.Net.Sockets;

namespace Xmms.Client {
	public class Message {
		public Message() {
			memoryStream = new MemoryStream();
		}

		public int ObjectID {
			get { return objectID; }
			set { objectID = value; }
		}

		public int CommandID {
			get { return commandID; }
			set { commandID = value; }
		}

		public int Cookie {
			get { return cookie; }
			set { cookie = value; }
		}

		public void PrepareForReadWrite() {
			memoryStream.Position = headerLength;
		}

		public void Assemble() {
			memoryStream.Position = 0;

			writeInteger(objectID);
			writeInteger(commandID);
			writeInteger(cookie);
			writeInteger(payloadLength);

			memoryStream.Position = 0;
		}

		public bool WriteChunk(Socket socket) {
			long chunkLength = System.Math.Min(
				4096, memoryStream.Length - memoryStream.Position
			);

			byte[] buffer = new byte[chunkLength];
			int read = memoryStream.Read(buffer, 0, buffer.Length);
			int written = socket.Send(buffer, read, SocketFlags.None);
			int delta = read - written;

			// Could we write all the data? Otherwise, seek back
			// so we'll try to send the remaining data again.
			if (delta > 0)
				memoryStream.Position -= delta;

			// if we reached EOF we're done.
			return memoryStream.Position == memoryStream.Length;
		}

		public bool ReadChunk(System.Net.Sockets.Socket socket) {
			byte[] buffer;
			int read;

			if (memoryStream.Length < headerLength) {
				buffer = new byte[headerLength - memoryStream.Length];
				read = socket.Receive(buffer);
				memoryStream.Write(buffer, 0, read);

				if (memoryStream.Length == headerLength) {
					memoryStream.Position = 0;

					objectID = readInteger();
					commandID = readInteger();
					cookie = readInteger();
					payloadLength = readInteger();
				}
			}

			int totalLength = headerLength + payloadLength;

			buffer = new byte[totalLength - memoryStream.Length];
			read = socket.Receive(buffer);
			memoryStream.Write(buffer, 0, read);

			return memoryStream.Length == totalLength;
		}

		public void Write(int n) {
			writeInteger(n);

			payloadLength += 4;
		}

		public void Write(string s) {
			byte[] bytes = System.Text.ASCIIEncoding.Default.GetBytes(s);

			int zLength = bytes.Length + 1;

			writeInteger(zLength);
			payloadLength += 4;

			memoryStream.Write(bytes, 0, bytes.Length);
			memoryStream.WriteByte(0);

			payloadLength += zLength;
		}

		public int ReadInteger() {
			return readInteger();
		}

		public string ReadString() {
			int length = readInteger();

			if (length == 0)
				return string.Empty;

			byte[] raw = new byte[length - 1];

			memoryStream.Read(raw, 0, raw.Length);
			memoryStream.ReadByte(); // NUL

			return System.Text.Encoding.ASCII.GetString(raw);
		}

		private void writeInteger(int n) {
			memoryStream.WriteByte ((byte)(n >> 24));
			memoryStream.WriteByte ((byte)(n >> 16));
			memoryStream.WriteByte ((byte)(n >> 8));
			memoryStream.WriteByte ((byte)n);
		}

		public int readInteger() {
			int n = 0;

			n |= memoryStream.ReadByte() << 24;
			n |= memoryStream.ReadByte() << 16;
			n |= memoryStream.ReadByte() << 8;
			n |= memoryStream.ReadByte();

			return n;
		}

		private int objectID;
		private int commandID;
		private int cookie;
		private int payloadLength;
		private int transferred;
		private readonly MemoryStream memoryStream;
		private const int headerLength = 16;
	}
}
