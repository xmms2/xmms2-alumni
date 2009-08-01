#!/usr/bin/env python
import sys

sys.path.append('../waftools')

import genipc
from indenter import Indenter

#dictionary mapping of types in the xml to C types
c_map = {}
c_map['none'] = 'void'
c_map['int'] = 'int'
c_map['string'] = 'const char *'
c_map['binary'] = 'xmmsv_t *'
c_map['collection'] = 'xmmsv_t *'
c_map['list'] = 'xmmsv_t *'
c_map['dictionary'] = 'xmmsv_t *'

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
		x = '%s %s' % (c_map[a.type[0]], a.name)
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
		Indenter.printline('msg = xmms_ipc_msg_new (0, 32);')
		Indenter.printline('xmmsc_ipc_msg_put_int32 (msg, %i);' % method.id)
		# FIXME: restartable stuff
	elif name_prefix == 'broadcast_':
		Indenter.printline('msg = xmms_ipc_msg_new (0, 33);')
		Indenter.printline('xmmsc_ipc_msg_put_int32 (msg, %i);' % method.id)

	for a in arguments:
		if a.type[0] == 'list':
			s = 'xmms_ipc_msg_put_value_list (msg, %s);'
			Indenter.printx(s % a.name)
		else:
			s = 'xmms_ipc_msg_put_%s (msg, %s);'
			Indenter.printline(s % (a.type[0], a.name))

	Indenter.printline()
	Indenter.printline('return xmmsc_send_msg (c, msg);')
	Indenter.leave('}')

	Indenter.printline()

ipc = genipc.parse_xml('../src/ipc.xml')
build(ipc)
