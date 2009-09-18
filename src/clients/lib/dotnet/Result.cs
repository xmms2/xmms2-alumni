using System;

namespace Xmms.Client {
	public abstract class Result {
		public Result(Client client, int cookie) {
			this.client = client;
			this.cookie = cookie;

			// value = new T();
		}

		public int Cookie {
			get { return cookie; }
		}
/*
		public T Value {
			get {
				if (!hasValue)
					throw new InvalidOperationException();

				return value;
			}
		}
 */
 		public void Wait() {
			client.WaitFor(this);
		}

		internal void ProcessReply(Message message) {
			if (message.CommandID == 0) {
				// reply
				getValue(message);
			} else if (message.CommandID == 1) {
				// error
				//isError = true;
			}

			hasValue = true;
		}

		protected abstract void getValue(Message message);

		protected void checkHasValue() {
			if (!hasValue)
				throw new InvalidOperationException();
		}

		private Client client;
		private int cookie;

		private bool hasValue;
		private readonly object errorValue;
	}

	public class VoidResult : Result {
		public VoidResult(
			Client client, int cookie
		) : base(client, cookie) {
		}

		protected override void getValue(Message message) {
			// empty
		}
	}

	public class IntegerResult : Result {
		public IntegerResult(
			Client client, int cookie
		) : base(client, cookie) {
		}

		public Value.Integer Value {
			get {
				checkHasValue();

				return value;
			}
		}

		protected override void getValue(Message message) {
			value = new Value.Integer();

			value.Deserialize(message);
		}

		private Value.Integer value;
	}

	public class StringResult : Result {
		public StringResult(
			Client client, int cookie
		) : base(client, cookie) {
		}

		public Value.String Value {
			get {
				checkHasValue();

				return value;
			}
		}

		protected override void getValue(Message message) {
			value = new Value.String();

			value.Deserialize(message);
		}

		private Value.String value;
	}

	public class IntegerListResult : Result {
		public IntegerListResult(
			Client client, int cookie
		) : base(client, cookie) {
		}

		public Value.List<Value.Integer> Value {
			get {
				checkHasValue();

				return value;
			}
		}

		protected override void getValue(Message message) {
		Console.Error.WriteLine("before ctor");
			value = new Value.List<Value.Integer>();
		Console.Error.WriteLine("after ctor");

			value.Deserialize(message);
		}

		private Value.List<Value.Integer> value;
	}
}
