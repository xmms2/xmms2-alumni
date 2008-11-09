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
using System.Collections.Generic;

using NUnit.Framework;

namespace Xmms.Client.Value {

	[TestFixture]
	public class DictionaryTest {
		[Test]
		public void TestNewDictionaryIsEmpty() {
			Dictionary<Int32> dict = new Dictionary<Int32>();

			Assert.AreEqual(0, dict.Count);
		}

		[Test]
		public void TestAddingItemsIncreasesCount() {
			Dictionary<Int32> dict = new Dictionary<Int32>();

			dict.Add("foo", new Int32(1));
			Assert.AreEqual(1, dict.Count);

			dict.Add("bar", new Int32(2));
			Assert.AreEqual(2, dict.Count);
		}

		[Test]
		public void TestRemovingItemsDecreasesCount() {
			Dictionary<Int32> dict = new Dictionary<Int32>();

			dict["foo"] = new Int32(1);
			dict["bar"] = new Int32(2);

			dict.Remove("foo");
			Assert.AreEqual(1, dict.Count);

			dict.Remove("bar");
			Assert.AreEqual(0, dict.Count);
		}

		[Test]
		public void TestClearSetsCountToZero() {
			Dictionary<Int32> dict = new Dictionary<Int32>();

			dict["foo"] = new Int32(1);
			dict["bar"] = new Int32(2);

			dict.Clear();
			Assert.AreEqual(0, dict.Count);
		}

		[Test]
		public void TestEnumerator() {
			Dictionary<Int32> dict = new Dictionary<Int32>();

			// Without the initial MoveNext call, Current will return
			// an empty KeyValuePair.
			Dictionary<Int32>.Enumerator enumerator = dict.GetEnumerator();
			KeyValuePair<string, Int32> pair = enumerator.Current;
			Assert.AreEqual(null, pair.Key);
			Assert.AreEqual(null, pair.Value);

			Int32 item0 = new Int32(5);
			Int32 item1 = new Int32(6);

			dict["foo"] = item0;
			dict["bar"] = item1;

			// Test again with a non-empty dict.
			enumerator = dict.GetEnumerator();
			pair = enumerator.Current;
			Assert.AreEqual(null, pair.Key);
			Assert.AreEqual(null, pair.Value);

			Assert.IsTrue(enumerator.MoveNext());
			pair = enumerator.Current;
			Assert.AreEqual("foo", pair.Key);
			Assert.AreEqual(item0, pair.Value);

			Assert.IsTrue(enumerator.MoveNext());
			pair = enumerator.Current;
			Assert.AreEqual("bar", pair.Key);
			Assert.AreEqual(item1, pair.Value);

			// After we walked off the end of the dict,
			// Current will return null again.
			Assert.IsFalse(enumerator.MoveNext());
			pair = enumerator.Current;
			Assert.AreEqual(null, pair.Key);
			Assert.AreEqual(null, pair.Value);

			// Adding another element does not help once we have an
			// invalid enumerator.
			Int32 item2 = new Int32(7);

			dict["baz"] = item2;

			pair = enumerator.Current;
			Assert.AreEqual(null, pair.Key);
			Assert.AreEqual(null, pair.Value);
		}

		[Test]
		[ExpectedException(typeof(ArgumentNullException))]
		public void TestIndexer1() {
			Dictionary<Int32> dict = new Dictionary<Int32>();

			Int32 x = dict[null];
		}

		[Test]
		[ExpectedException(typeof(KeyNotFoundException))]
		public void TestIndexer2() {
			Dictionary<Int32> dict = new Dictionary<Int32>();

			Int32 x = dict["crap"];
		}

		[Test]
		public void TestIndexer3() {
			Dictionary<Int32> dict = new Dictionary<Int32>();

			Int32 item0 = new Int32(5);
			Int32 item1 = new Int32(6);
			Int32 item2 = new Int32(7);

			dict["foo"] = item0;
			dict["bar"] = item1;
			dict["baz"] = item2;

			Assert.AreEqual(item0, dict["foo"]);
			Assert.AreEqual(item1, dict["bar"]);
			Assert.AreEqual(item2, dict["baz"]);
		}

		[Test]
		public void TestRemove() {
			Dictionary<Int32> dict = new Dictionary<Int32>();

			Int32 item0 = new Int32(5);
			Int32 item1 = new Int32(6);

			dict["foo"] = item0;
			dict["bar"] = item1;

			Assert.IsFalse(dict.Remove("baz"));
			Assert.IsTrue(dict.Remove("foo"));
		}

		[Test]
		public void TestInsert() {
			Dictionary<Int32> dict = new Dictionary<Int32>();

			Int32 item0 = new Int32(5);

			dict["foo"] = item0;
			Assert.AreEqual(1, dict.Count);
			Assert.AreEqual(item0, dict["foo"]);

			Int32 item1 = new Int32(6);

			dict["foo"] = item1;
			Assert.AreEqual(1, dict.Count);
			Assert.AreEqual(item1, dict["foo"]);
		}
	}
}
#endif
