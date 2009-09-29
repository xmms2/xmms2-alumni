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
	Indenter.printline('#ifndef __XMMSCLIENT_QT_GEN_H__')
	Indenter.printline('#define __XMMSCLIENT_QT_GEN_H__')
	Indenter.printline()
	Indenter.printline('#include <QObject>')
	Indenter.printline('#include <QMap>')
	Indenter.printline('#include <QStringList>')
	Indenter.printline('#include "object.h"')
	Indenter.printline('#include "coll.h"')
	Indenter.printline('#include "result.h"')	
	Indenter.printline()

	Indenter.enter('namespace XMMSQt {')
	
	for obj in ipc.objects:
		Indenter.enter('class %s : public BaseObject {' % camel_case(obj.name))
		Indenter.printline('Q_OBJECT')
		
		Indenter.enter('public:')
		
		Indenter.printline('%s (Client *client) : BaseObject (client)' % camel_case(obj.name))
		Indenter.printline('{')
		Indenter.printline('};')
		Indenter.printline()

		for method in obj.methods:
			emit_method_code(obj, method, '')

		for signal in obj.signals:
			emit_method_code(obj, signal, 'signal_')

		for broadcast in obj.broadcasts:
			emit_method_code(obj, broadcast, 'broadcast_')
			
		Indenter.leave('') # public
		Indenter.leave('};')
		Indenter.printline()

	Indenter.leave('}')
	Indenter.printline('#endif /* __XMMSCLIENT_QT_GEN_H__ */')

# FIXME: i'm going to hell for the name_prefix stuff
def emit_method_code(obj, method, name_prefix):
	method_name = name_prefix + method.name
	arguments = getattr(method, 'arguments', [])


	# first, write documentation for this method
	Indenter.printline('/** %s' % method.documentation)
	Indenter.printline(' *')
	
	if len(arguments) > 0:
		for a in arguments:
			Indenter.printline(' * @param %s %s' % (a.name, a.documentation))
		Indenter.printline(' *')
		
	Indenter.printline(' * @return a Result class')
	Indenter.printline(' *')
	Indenter.printline(' */')

	# now write the method definition
	Indenter.printx('Result %s ' % camel_case2(method_name))

	if not arguments:
		print '();'
	else:
		s = ', '.join(["%s %s" % (type_map.get(tuple(a.type)), a.name) for a in arguments])
		print '(%s);' % s

	Indenter.printline()

ipc = genipc.parse_xml('../src/ipc.xml')
build(ipc)
