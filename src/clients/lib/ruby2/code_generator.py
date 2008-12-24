import sys

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
	def leave(cls, s = 'end'):
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
	Indenter.printline('# This code is automatically generated from foobar. Do not edit.')
	Indenter.printline()

	Indenter.enter('module Xmms::Client')

	for object in ipc.objects:
		Indenter.enter('class %s < MethodGroup' % camel_case(object.name))
		Indenter.enter('def initialize(client)')
		Indenter.printline('super client')
		Indenter.printline()
		Indenter.printline('@id = %i' % object.id)
		Indenter.leave()
		Indenter.printline()

		for method in object.methods:
			emit_method_code(object, method, '')

		for signal in object.signals:
			emit_method_code(object, signal, 'signal_')

		for broadcast in object.broadcasts:
			emit_method_code(object, broadcast, 'broadcast_')

		Indenter.leave()
		Indenter.printline()

	Indenter.leave()

# FIXME: i'm going to hell for the name_prefix stuff
def emit_method_code(object, method, name_prefix):
	method_name = name_prefix + method.name

	# first, write documentation for this method
	Indenter.printline('# :call-seq:')
	Indenter.printx('#  client.%s -> result' % method_name)

	if method.return_value:
		print ' (payload: %s)' % method.return_value.type
	else:
		print ' (payload: nil)'

	if method.documentation != None:
		Indenter.printline('#')
		Indenter.printline('# %s' % method.documentation)

	arguments = getattr(method, 'arguments', [])
	for a in arguments:
		Indenter.printline('# _%s_: %s' % (a.name, a.documentation))

	# now write the method definition
	Indenter.printx('def %s' % method_name)

	if not arguments:
		print
	else:
		s = ', '.join(a.name for a in arguments)
		print '(%s)' % s

	Indenter.enter()

	for a in arguments:
		s = '%s = Message.check_%s %s'
		Indenter.printline(s % (a.name, a.type, a.name))

	if arguments:
		print

	Indenter.printline('message = Message.new')

	if not name_prefix:
		Indenter.printline('message.object_id = @id')
		Indenter.printline('message.command_id = %i' % method.id)
	elif name_prefix == 'signal_':
		Indenter.printline('message.object_id = 6')
		Indenter.printline('message.command_id = 32')
		Indenter.printline('message.write_uint %i' % method.id)
	elif name_prefix == 'broadcast_':
		Indenter.printline('message.object_id = 6')
		Indenter.printline('message.command_id = 33')
		Indenter.printline('message.write_uint %i' % method.id)

	if arguments:
		print

	for a in arguments:
		s = 'message.write_%s %s'
		Indenter.printline(s % (a.type, a.name))

	Indenter.printline()
	Indenter.printline('@client.send_message message')
	Indenter.leave()

	Indenter.printline()
