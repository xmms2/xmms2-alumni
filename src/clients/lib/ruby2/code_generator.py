#!/usr/bin/env python
import sys

sys.path.append('../waftools')

import genipc
from indenter import Indenter

#dictionary mapping of types in the xml to Ruby types
ruby_map = {
	'int': 'Integer',
	'string': 'String',
	'list': 'Array',
	'dictionary': 'Hash',
}

# turns strings like "foo_bar" into "FooBar".
def camel_case(s):
	return ''.join(x.capitalize() for x in s.split('_'))

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
		Indenter.leave('end')
		Indenter.printline()

		for method in object.methods:
			emit_method_code(object, method, '')

		for signal in object.signals:
			emit_method_code(object, signal, 'signal_')

		for broadcast in object.broadcasts:
			emit_method_code(object, broadcast, 'broadcast_')

		Indenter.leave('end')
		Indenter.printline()

	Indenter.leave('end')

# FIXME: i'm going to hell for the name_prefix stuff
def emit_method_code(object, method, name_prefix):
	method_name = name_prefix + method.name

	# first, write documentation for this method
	Indenter.printline('# :call-seq:')
	Indenter.printx('#  client.%s -> result' % method_name)

	if method.return_value:
		print ' (payload: %s)' % method.return_value.type[0]
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
		if len(a.type) > 1:
			subtype = ruby_map[a.type[1]]

			s = '%s = Message.check_%s %s, %s'
			Indenter.printline(s % (a.name, a.type[0], a.name, subtype))
		else:
			s = '%s = Message.check_%s %s'
			Indenter.printline(s % (a.name, a.type[0], a.name))

	if arguments:
		print

	Indenter.printline('message = Message.new')

	if not name_prefix:
		Indenter.printline('message.object_id = @id')
		Indenter.printline('message.command_id = %i' % method.id)
	elif name_prefix == 'signal_':
		Indenter.printline('message.object_id = 0')
		Indenter.printline('message.command_id = 32')
		Indenter.printline('message.write_int %i' % method.id)
	elif name_prefix == 'broadcast_':
		Indenter.printline('message.object_id = 0')
		Indenter.printline('message.command_id = 33')
		Indenter.printline('message.write_int %i' % method.id)

	if arguments:
		print

	for a in arguments:
		s = 'message.write_%s %s'
		Indenter.printline(s % (a.type[0], a.name))

	Indenter.printline()
	Indenter.printline('@client.send_message message')
	Indenter.leave('end')

	Indenter.printline()

ipc = genipc.parse_xml('../src/ipc.xml')
build(ipc)
