class Program {
	public static void Main(string[] arguments) {
		System.Console.WriteLine("hello world");

		var c = new Xmms.Client.Client();
		c.Connect();
	}
}
