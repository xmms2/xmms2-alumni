using System;
using System.Collections.Generic;
using System.Collections;

namespace Xmms.Client {
	public abstract class Value {
		public enum ValueType {
			None = 0,
			Error = 1,
			Integer = 2,
			String = 3,
			Collection = 4,
			Binary = 5,
			List = 6,
			Dictionary = 7,
		}

		public abstract void Deserialize(Message message);

#if false
		internal static Value Deserialize(Message message) {
			ValueType type = (ValueType)message.ReadInteger();
			int length;

			switch (type) {
				case ValueType.None:
					return null;
				case ValueType.Error:
					return ErrorValue.Instance;
				case ValueType.Integer:
					return new IntegerValue(message.ReadInteger());
				case ValueType.String:
					return new StringValue(message.ReadString());
				case ValueType.Collection:
					throw new NotImplementedException();
				case ValueType.Binary:
					throw new NotImplementedException();
				case ValueType.List:

					length = message.ReadInteger();
					var list = new List<Value>(length);

					while (length-- > 0)
						list.Add(Value.Deserialize(message));

					return new ListValue(list);
				case ValueType.Dictionary:
					length = message.ReadInteger();
					var dictionary = new Dictionary<string, Value>();

					while (length-- > 0) {
						string key = message.ReadString();
						Value value = Value.Deserialize(message);

						dictionary[key] = value;
					}

					return new DictionaryValue(dictionary);
			}
		}
#endif

		internal static void CheckIsType(
			Message message, ValueType expectedType
		) {
			ValueType actualType = (ValueType)message.ReadInteger();

			System.Diagnostics.Debug.Assert(
					expectedType == actualType
			);
		}
	}

	public class ErrorValue : Value {
		private ErrorValue() {
		}

		public override void Deserialize(Message message) {
			CheckIsType(message, ValueType.Error);

			errorString = message.ReadString();
		}

		private string errorString;
	}

	public class IntegerValue : Value {
		public static implicit operator int(IntegerValue integerValue) {
			return integerValue.value;
		}

		public override void Deserialize(Message message) {
			CheckIsType(message, ValueType.Integer);

			value = message.ReadInteger();
		}

		private int value;
	}

	public class StringValue : Value {
		public static implicit operator string(StringValue stringValue) {
			return stringValue.value;
		}

		public override void Deserialize(Message message) {
			CheckIsType(message, ValueType.Integer);

			value = message.ReadString();
		}

		private string value;
	}

	public class ListValue<T> : Value, IList<T>
		where T : Value, new()
	{
		public ListValue() {
			this.items = new List<T>();
		}

		public override void Deserialize(Message message) {
			CheckIsType(message, ValueType.Integer);

			int length = message.ReadInteger();

			while (length-- > 0) {
				T item = new T();
				item.Deserialize(message);

				items.Add(item);
			}
		}

		public T this[int position] {
			get { return items[position]; }
			set { items[position] = value; }
		}

		public bool IsReadOnly {
			get { return false; }
		}

		public int Count {
			get { return items.Count; }
		}

		public void Add(T item) {
			items.Add(item);
		}

		public void Insert(int position, T item) {
			items.Insert(position, item);
		}

		public void RemoveAt(int position) {
			items.RemoveAt(position);
		}

		public bool Remove(T item) {
			return items.Remove(item);
		}

		public void Clear() {
			items.Clear();
		}

		public int IndexOf(T item) {
			return items.IndexOf(item);
		}

		public bool Contains(T item) {
			return items.Contains(item);
		}

		public void CopyTo(T[] array, int arrayIndex) {
			items.CopyTo(array, arrayIndex);
		}

		public IEnumerator<T> GetEnumerator() {
			return items.GetEnumerator();
		}

		IEnumerator IEnumerable.GetEnumerator() {
			return items.GetEnumerator();
		}

		private readonly IList<T> items;
	}

	public class DictionaryValue<T> : Value, IDictionary<string, T>
		where T : Value, new()
	{
		public DictionaryValue() {
			this.items = new Dictionary<string, T>();
		}

		public override void Deserialize(Message message) {
			CheckIsType(message, ValueType.Integer);

			int length = message.ReadInteger();

			while (length-- > 0) {
				string key = message.ReadString();

				T item = new T();
				item.Deserialize(message);

				items[key] = item;
			}
		}

		public T this[string key] {
			get { return items[key]; }
			set { items[key] = value; }
		}

		public bool IsReadOnly {
			get { return false; }
		}

		public int Count {
			get { return items.Count; }
		}

		public ICollection<string> Keys {
			get { return items.Keys; }
		}

		public ICollection<T> Values {
			get { return items.Values; }
		}

		public bool TryGetValue(string key, out T value) {
			return items.TryGetValue(key, out value);
		}

		public void Add(string key, T value) {
			items.Add(key, value);
		}

		public void Add(KeyValuePair<string, T> pair) {
			items.Add(pair);
		}

		public bool Remove(string key) {
			return items.Remove(key);
		}

		public bool Remove(KeyValuePair<string, T> pair) {
			return items.Remove(pair);
		}

		public void Clear() {
			items.Clear();
		}

		public bool ContainsKey(string key) {
			return items.ContainsKey(key);
		}

		public bool Contains(KeyValuePair<string, T> pair) {
			return items.Contains(pair);
		}

		public void CopyTo(KeyValuePair<string, T>[] array, int arrayIndex) {
			items.CopyTo(array, arrayIndex);
		}

		public IEnumerator<KeyValuePair<string, T>> GetEnumerator() {
			return items.GetEnumerator();
		}

		IEnumerator IEnumerable.GetEnumerator() {
			return items.GetEnumerator();
		}

		private readonly IDictionary<string, T> items;
	}
}
