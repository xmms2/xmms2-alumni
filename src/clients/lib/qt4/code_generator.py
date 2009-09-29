#!/usr/bin/env python
import sys

sys.path.append('../waftools')

import genipc
from indenter import Indenter

#dictionary mapping of types in the xml to Qt4 types
type_map = {
	('int',): 'quint32',
	('string',): 'const QString &',
	('collection',): 'const Coll::Coll &',
	('binary',): 'const QByteArray &',
	('list', 'string'): 'const QStringList &',
	('dictionary', 'string'): 'const QMap<QString, QString> &',
}

# turns strings like "foo_bar" into "FooBar".
def camel_case(s):
	return ''.join(x.capitalize() for x in s.split('_'))
	
#turns strings like "foo_bar" into "fooBar"
def camel_case2(s):
	sp = s.split('_')
	ret = sp[0]
	for x in sp[1:]:
		ret+=x.capitalize()
	return ret

def build(ipc):
	Indenter.printline('/* This code is automatically generated from foobar. Do not edit. */')
	Indenter.printline()
	Indenter.printline('#include <QObject>')
	Indenter.printline('#include <QMap>')
	Indenter.printline('#include <QStringList>')
	Indenter.printline('#include "object.h"')
	Indenter.printline('#include "coll.h"')
	Indenter.printline('#include "result.h"')	
	Indenter.printline('#include "xmmsclient_qt_gen.h"')
	Indenter.printline()

	Indenter.enter('namespace XMMSQt {')
	
	for obj in ipc.objects:
		
		for method in obj.methods:
			emit_method_code(obj, method, '')
			Indenter.printline()

		for method in obj.broadcasts:
			emit_method_code(obj, method, 'broadcast_')
			Indenter.printline()

		for method in obj.signals:
			emit_method_code(obj, method, 'signal_')
			Indenter.printline()
				

	Indenter.leave('}')
	Indenter.printline()

# FIXME: i'm going to hell for the name_prefix stuff
def emit_method_code(obj, method, name_prefix):
	method_name = camel_case2(name_prefix+method.name)
	Indenter.printline('Result')
	Indenter.printx('%s:%s ' % (camel_case(obj.name), method_name))
			
	arguments = getattr(method, 'arguments', [])
			
	if not arguments:
		print '()'
	else:
		s = ', '.join(["%s %s" % (type_map.get(tuple(a.type)), a.name) for a in arguments])
		print '(%s)' % s

	Indenter.enter('{')
	if name_prefix is '':
		Indenter.printline('Message msg (%d, %d);' % (obj.id, method.id))
	elif name_prefix is 'broadcast_':
		Indenter.printline('Message msg (0, 33)')
		Indenter.printline('msg.add (%d)' % method.id)
	elif name_prefix is 'signal_':
		Indenter.printline('Message msg (0, 32)')
		Indenter.printline('msg.add (%d)' % method.id)
			
	for a in arguments:
		Indenter.printline('msg.add (%s);' % a.name)
		
	Indenter.printline('return m_client->queueMsg (msg);')
	Indenter.leave('}')

ipc = genipc.parse_xml('../src/ipc.xml')
build(ipc)
