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

#if UNITTEST
using System;

using NUnit.Framework;

namespace Xmms.Client.Value {

	[TestFixture]
	public class ListTest {
		[Test]
		public void TestNewListIsEmpty() {
			List<String> list = new List<String>();

			Assert.AreEqual(0, list.Count);
		}

		[Test]
		public void TestAddingItemsIncreasesCount() {
			List<Int32> list = new List<Int32>();

			list.Add(new Int32(1));
			Assert.AreEqual(1, list.Count);

			list.Add(new Int32(2));
			Assert.AreEqual(2, list.Count);
		}

		[Test]
		public void TestRemovingItemsDecreasesCount() {
			List<Int32> list = new List<Int32>();

			list.Add(new Int32(1));
			list.Add(new Int32(2));

			list.RemoveAt(0);
			Assert.AreEqual(1, list.Count);

			list.RemoveAt(0);
			Assert.AreEqual(0, list.Count);
		}

		[Test]
		public void TestClearSetsCountToZero() {
			List<Int32> list = new List<Int32>();

			list.Add(new Int32(1));
			list.Add(new Int32(2));

			list.Clear();
			Assert.AreEqual(0, list.Count);
		}

		[Test]
		public void TestEnumerator() {
			List<Int32> list = new List<Int32>();

			// Without the initial MoveNext call, Current will just
			// return null.
			List<Int32>.Enumerator enumerator = list.GetEnumerator();
			Assert.AreEqual(null, enumerator.Current);

			Int32 item0 = new Int32(5);
			Int32 item1 = new Int32(6);

			list.Add(item0);
			list.Add(item1);

			// Test again with a non-empty list.
			enumerator = list.GetEnumerator();
			Assert.AreEqual(null, enumerator.Current);

			Assert.IsTrue(enumerator.MoveNext());
			Assert.AreEqual(item0, enumerator.Current);

			Assert.IsTrue(enumerator.MoveNext());
			Assert.AreEqual(item1, enumerator.Current);

			// After we walked off the end of the list,
			// Current will return null again.
			Assert.IsFalse(enumerator.MoveNext());
			Assert.AreEqual(null, enumerator.Current);

			// Adding another element does not help once we have an
			// invalid enumerator.
			Int32 item2 = new Int32(7);

			list.Add(item2);
			Assert.AreEqual(null, enumerator.Current);
		}

		[Test]
		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		public void TestIndexer1() {
			List<Int32> list = new List<Int32>();

			Int32 x = list[0];
		}

		[Test]
		[ExpectedException(typeof(ArgumentOutOfRangeException))]
		public void TestIndexer2() {
			List<Int32> list = new List<Int32>();

			Int32 x = list[-1];
		}

		[Test]
		public void TestIndexer3() {
			List<Int32> list = new List<Int32>();

			Int32 item0 = new Int32(5);
			Int32 item1 = new Int32(6);
			Int32 item2 = new Int32(7);

			list.Add(item0);
			list.Add(item1);
			list.Add(item2);

			Assert.AreEqual(item0, list[0]);
			Assert.AreEqual(item1, list[1]);
			Assert.AreEqual(item2, list[2]);
			Assert.AreEqual(item2, list[-1]);
			Assert.AreEqual(item1, list[-2]);
			Assert.AreEqual(item0, list[-3]);

			list[-1] = list[0];
			Assert.AreEqual(list[0], list[-1]);
		}

		[Test]
		public void TestRemoveAt() {
			List<Int32> list = new List<Int32>();

			Int32 item0 = new Int32(5);
			Int32 item1 = new Int32(6);

			list.Add(item0);
			list.Add(item1);

			list.RemoveAt(0);
			Assert.AreEqual(item1, list[0]);
		}

		[Test]
		public void TestInsert() {
			List<Int32> list = new List<Int32>();

			Int32 item0 = new Int32(5);

			list.Insert(0, item0);
			Assert.AreEqual(1, list.Count);
			Assert.AreEqual(item0, list[0]);

			Int32 item1 = new Int32(6);

			list.Insert(0, item1);
			Assert.AreEqual(2, list.Count);
			Assert.AreEqual(item1, list[0]);
			Assert.AreEqual(item0, list[1]);

			Int32 item2 = new Int32(7);

			list.Insert(-1, item2);
			Assert.AreEqual(3, list.Count);
			Assert.AreEqual(item1, list[0]);
			Assert.AreEqual(item2, list[1]);
			Assert.AreEqual(item0, list[2]);

			Int32 item3 = new Int32(8);

			list.Insert(-3, item3);
			Assert.AreEqual(4, list.Count);
			Assert.AreEqual(item3, list[0]);
			Assert.AreEqual(item1, list[1]);
			Assert.AreEqual(item2, list[2]);
			Assert.AreEqual(item0, list[3]);
		}
	}
}
#endif
