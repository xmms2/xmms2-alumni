import sys

class Indenthelper:
	def __init__(self, f):
		self.indent = 0
		self._file = f

	def enter(self, s = None):
		if s != None:
			self.printline(s)

		self.indent += 1

	def leave(self, s = None):
		self.indent -= 1

		self.printline(s)

	# print the given string without a trailing newline
	def printx(self, s):
		self._file.write('\t' * self.indent)
		self._file.write(s)

	# print the given string including a trailing newline
	def printline(self, s = ''):
		if not s:
			self._file.write('\n')
		else:
			self._file.write('\t' * self.indent)
			self._file.write('%s\n' % s)
