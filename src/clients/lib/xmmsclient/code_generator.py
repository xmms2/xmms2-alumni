import sys

#dictionary mapping of types in the xml to C types
c_map = {}
c_map['none'] = 'void'
c_map['int'] = 'int'
c_map['uint'] = 'unsigned int'
c_map['string'] = 'const char *'
c_map['enum'] = 'unsigned int'
c_map['stringlist'] = 'char **'

class Indenter:
	indent = 0

	@classmethod
	def enter(cls, s = None):
		if s != None:
			Indenter.printline(s)

		cls.indent += 1

	@classmethod
	def leave(cls, s = '}'):
		cls.indent -= 1

		Indenter.printline(s)

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
	Indenter.printline('/* This code is automatically generated from foobar. Do not edit. */')
	Indenter.printline()

	for object in ipc.objects:
		for method in object.methods:
			emit_method_code(object, method, '')

		for signal in object.signals:
			emit_method_code(object, signal, 'signal_')

		for broadcast in object.broadcasts:
			emit_method_code(object, broadcast, 'broadcast_')

# FIXME: i'm going to hell for the name_prefix stuff
def emit_method_code(object, method, name_prefix):
	method_name = 'xmmsc_%s%s_%s' % (name_prefix, object.name, method.name)

	# first, write documentation for this method
	Indenter.printline('/**')
	Indenter.printline(' * %s' % method.documentation)
	Indenter.printline(' *')
	Indenter.printline(' * @param c The connection structure.')

	arguments = getattr(method, 'arguments', [])

	if arguments:
		for a in arguments:
			Indenter.printline(' * @param %s %s' % (a.name, a.documentation))

	Indenter.printline(' */')

	# now write the method definition
	Indenter.printline('xmmsc_result_t *')
	Indenter.printx(method_name)

	c_arguments = ['xmmsc_connection_t *c']

	for a in arguments:
		x = '%s %s' % (c_map[a.type], a.name)
		c_arguments.append(x)

	print ' (%s)' % ', '.join(c_arguments)

	Indenter.enter('{')

	Indenter.printline('xmms_ipc_msg_t *msg;')
	Indenter.printline()
	Indenter.printline('x_check_conn (c, NULL);')
	Indenter.printline()

	if not name_prefix:
		Indenter.printline('msg = xmms_ipc_msg_new (%i, %i);' % (object.id, method.id))
	elif name_prefix == 'signal_':
		Indenter.printline('msg = xmms_ipc_msg_new (6, 32);')
		Indenter.printline('xmmsc_ipc_msg_put_uint32 (msg, %i);' % method.id)
		# FIXME: restartable stuff
	elif name_prefix == 'broadcast_':
		Indenter.printline('msg = xmms_ipc_msg_new (6, 33);')
		Indenter.printline('xmmsc_ipc_msg_put_uint32 (msg, %i);' % method.id)

	for a in arguments:
		s = 'xmms_ipc_msg_put_%s (msg, %s);'
		Indenter.printline(s % (a.type, a.name))

	Indenter.printline()
	Indenter.printline('return xmmsc_send_msg (c, msg);')
	Indenter.leave()

	Indenter.printline()
