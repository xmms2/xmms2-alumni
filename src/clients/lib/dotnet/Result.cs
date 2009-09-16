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

		public IntegerValue Value {
			get {
				checkHasValue();

				return value;
			}
		}

		protected override void getValue(Message message) {
			value = new IntegerValue();

			value.Deserialize(message);
		}

		private IntegerValue value;
	}

	public class StringResult : Result {
		public StringResult(
			Client client, int cookie
		) : base(client, cookie) {
		}

		public StringValue Value {
			get {
				checkHasValue();

				return value;
			}
		}

		protected override void getValue(Message message) {
			value = new StringValue();

			value.Deserialize(message);
		}

		private StringValue value;
	}

	public class IntegerListResult : Result {
		public IntegerListResult(
			Client client, int cookie
		) : base(client, cookie) {
		}

		public ListValue<IntegerValue> Value {
			get {
				checkHasValue();

				return value;
			}
		}

		protected override void getValue(Message message) {
			value = new ListValue<IntegerValue>();

			value.Deserialize(message);
		}

		private ListValue<IntegerValue> value;
	}
}
