import sys

py_map = {}
py_map['int'] = 'INT32'
py_map['string'] = 'STRING'
py_map['list'] = 'LIST'
py_map['dictionary'] = 'DICT'

# turns strings like "foo_bar" into "FooBar".
def camel_case(s):
	return ''.join(x.capitalize() for x in s.split('_'))

class Indenter:
	indent = 0

	@classmethod
	def enter(cls, s = None):
		if s != None:
			Indenter.printline(s)

		cls.indent += 1

	@classmethod
	def leave(cls):
		cls.indent -= 1

		Indenter.printline()

	# print the given string without a trailing newline
	@classmethod
	def printx(cls, s):
		sys.stdout.write('\t' * cls.indent)
		sys.stdout.write(s)

	# print the given string including a trailing newline
	@classmethod
	def printline(cls, s = ''):
		if not s:
			print ''
		else:
			sys.stdout.write('\t' * cls.indent)
			print s

# entry point
def build(ipc):
	Indenter.printline('# This code is automatically generated from foobar. Do not edit.')
	Indenter.printline()

	for object in ipc.objects:
		Indenter.enter('class XMMSObject%s(XMMSMessenger):' % camel_case(object.name))
		Indenter.printline('ID = %s' % object.id)
		Indenter.printline()

		for method in object.methods:
			emit_method_code(object, method, '')

		for signal in object.signals:
			emit_method_code(object, signal, 'signal_')

		for broadcast in object.broadcasts:
			emit_method_code(object, broadcast, 'broadcast_')

		Indenter.leave()

# FIXME: i'm going to hell for the name_prefix stuff
def emit_method_code(object, method, name_prefix):
	method_name = name_prefix + method.name

	# first, write documentation for this method
	#Indenter.printline('# :call-seq:')
	#Indenter.printx('#  client.%s -> result' % method_name)

	#if method.return_value:
	#	print ' (payload: %s)' % method.return_value.type
	#else:
	#	print ' (payload: nil)'

	#if method.documentation != None:
	#	Indenter.printline('#')
	#	Indenter.printline('# %s' % method.documentation)

	arguments = getattr(method, 'arguments', [])

	# now write the method definition
	Indenter.printx('%s = msg(ipc_cmd=%i, ' % (method_name, method.id))

	if not method.return_value:
		print 'returns=NONE',
	else:
		s = 'returns=%s' % py_map[method.return_value.type[0]]
		print s,

	if not arguments:
		print ')'
	else:
		s = ', '.join(py_map[a.type[0]] for a in arguments)
		print ', args=[%s])' % s

	if method.documentation != None:
		Indenter.printline('%s.__doc__ = \'%s\'' % (method_name, method.documentation))

	Indenter.printline()
