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
using System.Net.Sockets;

namespace Xmms.Client {
	public class Message {
		public Message() {
			memoryStream = new MemoryStream();
		}

		public uint ObjectID {
			get { return objectID; }
			set { objectID = value; }
		}

		public uint CommandID {
			get { return commandID; }
			set { commandID = value; }
		}

		public uint Cookie {
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

					objectID = ReadUnsignedInteger();
					commandID = ReadUnsignedInteger();
					cookie = ReadUnsignedInteger();
					payloadLength = ReadUnsignedInteger();
				}
			}

			uint totalLength = headerLength + payloadLength;

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

			uint zLength = (uint)bytes.Length + 1;

			writeInteger(zLength);
			payloadLength += 4;

			memoryStream.Write(bytes, 0, bytes.Length);
			memoryStream.WriteByte(0);

			payloadLength += zLength;
		}

		public void Write(byte[] data) {
			Write(data.Length);

			memoryStream.Write(data, 0, data.Length);
		}

		public void Write(collection c) {
			//throw new NotImplementedException();

			// type:uint32
			// num_attrs:uint32
			// foreach attr:
			//      key:string
			//      value:string
			//
			// num_ids:uint32
			// foreach ids:
			//      id:uint32
			//
			// num_references:uint32
			// foreach reference:
			//      referenced_coll:collection
		}

		public void Write(IList<string> strings) {
			throw new NotImplementedException();
		}

		public void Write(IDictionary<string, string> strings) {
			throw new NotImplementedException();
		}

		public int ReadSignedInteger() {
			return readSignedInteger();
		}

		public uint ReadUnsignedInteger() {
			return unchecked((uint)readSignedInteger());
		}

		public string ReadString() {
			uint length = ReadUnsignedInteger();

			if (length == 0)
				return string.Empty;

			byte[] raw = new byte[length - 1];

			memoryStream.Read(raw, 0, raw.Length);
			memoryStream.ReadByte(); // NUL

			return System.Text.Encoding.ASCII.GetString(raw);
		}

		public void Read(byte[] buffer) {
			memoryStream.Read(buffer, 0, buffer.Length);
		}

		private void writeInteger(int n) {
			memoryStream.WriteByte((byte)((n >> 24) & 0xff));
			memoryStream.WriteByte((byte)((n >> 16) & 0xff));
			memoryStream.WriteByte((byte)((n >> 8) & 0xff));
			memoryStream.WriteByte((byte)(n & 0xff));
		}

		private void writeInteger(uint n) {
			writeInteger(unchecked((int)n));
		}

		private int readSignedInteger() {
			byte[] buffer = new byte[4];
			memoryStream.Read(buffer, 0, buffer.Length);

			int n = 0;

			n |= buffer[0] << 24;
			n |= buffer[1] << 16;
			n |= buffer[2] << 8;
			n |= buffer[3];

			return n;
		}

		private uint objectID;
		private uint commandID;
		private uint cookie;
		private uint payloadLength;
		private readonly MemoryStream memoryStream;
		private const int headerLength = 16;
	}
}
