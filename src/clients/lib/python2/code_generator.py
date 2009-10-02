#!/usr/bin/env python
import sys

sys.path.append('../waftools')

import genipc
from indenter import Indenter

py_map = {}
py_map['int'] = 'INT32'
py_map['string'] = 'STRING'
py_map['binary'] = 'BIN'
py_map['collection'] = 'COLL'
py_map['list'] = 'LIST'
py_map['dictionary'] = 'DICT'

# turns strings like "foo_bar" into "FooBar".
def camel_case(s):
	return ''.join(x.capitalize() for x in s.split('_'))

def build(ipc):
	Indenter.printline('# This code is automatically generated from foobar. Do not edit.')
	Indenter.printline()
        Indenter.printline('from message import XMMSMessenger, msg')
        Indenter.printline('from xmmstypes import *')
	Indenter.printline()

	for object in ipc.objects:
		Indenter.enter('class XMMSObject%s(XMMSMessenger):' % camel_case(object.name))
		Indenter.printline('ID = %d' % (object.id+1))
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
		s = 'returns=%s' % py_map[method.return_value.type]
		print s

	if not arguments:
		print ')'
	else:
		s = ', '.join('(%s, %s)' % (repr(a.name), py_map[a.type])
                              for a in arguments)
		print ', args=[%s])' % s

	if method.documentation != None:
		Indenter.printline('%s.__doc__ = %s' % (method_name, repr(method.documentation)))

	Indenter.printline()

ipc = genipc.parse_xml('../src/ipc.xml')
build(ipc)
