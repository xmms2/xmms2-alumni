import unittest
import threading
import xmmsclient

class TestClient(threading.Thread):
	def __init__(self, testcase):
		threading.Thread.__init__(self)
		self.testcase = testcase

	def run(self):
		c = xmmsclient.XMMS('TestConnections')
		c.connect()

		res = c.playback_status()
		res.wait()

class TestConnections(unittest.TestCase):
	def setUp(self):
		self.client_list = []

	def tearDown(self):
		pass

	def testNothing(self):
		self.assertEqual(1, 1)

def _base_test(x):
	def test(self, x):
		self.client_list = []
		print "# %d concurrent connections" % x

		for i in xrange(x):
			current = TestClient(self)
			self.client_list.append(current)
			current.start()

		for thread in self.client_list:
			thread.join() and self.assertEqual(1, 1)


	return lambda s: test(s, x)

if __name__ == '__main__':
	for i in [1, 10, 100, 200, 300, 1000, 2000]:
		setattr(TestConnections, 'test' + str(i) + 'Connections', _base_test(i))

	unittest.main()
