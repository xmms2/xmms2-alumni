#!/usr/bin/env python
import sys

sys.path.append('../../../../waftools')

import genipc
from indenter import Indenter

result_value_type_map = {
	('int',): 'Value.Integer',
	('string',): 'Value.String',
	('collection',): 'Value.Collection',
	('binary',): 'Value.Binary',
	('list', 'int'): 'Value.List<Value.Integer>',
	('list', 'string'): 'Value.List<Value.String>',
	('list', 'dictionary', 'unknown'): 'Value.List<Value.Integer>',
	('dictionary', 'string'): 'Value.Dictionary<Value.String>',
	('dictionary', 'int'): 'Value.Dictionary<Value.Integer>',
	('dictionary', 'unknown'): 'Value.UnknownDictionary',
	('dictionary', 'dictionary', 'unknown'): 'Value.Dictionary<Value.UnknownDictionary>',
}

argument_type_map = {
	('int',): 'int',
	('string',): 'string',
	('collection',): 'collection',
	('binary',): 'byte[]',
	('list', 'string'): 'IList<string>',
	('dictionary', 'string'): 'IDictionary<string, string>',
}

# turns strings like "foo_bar" into "FooBar".
def camel_case(s):
	return ''.join(x.capitalize() for x  in s.split('_'))

def build(ipc):
	Indenter.printline('// This code is automatically generated from foobar. Do not edit.')
	Indenter.printline()
	Indenter.printline('using System.Collections.Generic;')
	Indenter.printline()
	Indenter.enter('namespace Xmms.Client.Generated {')

	for object in ipc.objects:
		camel_object_name = camel_case(object.name)

		Indenter.enter('public class %s : MethodGroup {' % camel_object_name)
		Indenter.enter('internal %s(Client client) : base(client, %i) {' % (camel_object_name, object.id))
		Indenter.leave('}')
		Indenter.printline()

		for method in object.methods:
			emit_method_code(object, method)

		Indenter.leave('}')
		Indenter.printline()

	Indenter.leave('}')

def emit_method_code(object, method):
	method_name = camel_case(method.name)

	result_type = 'VoidResult'

	if method.return_value:
		result_type = 'ValueResult<' + result_value_type_map[tuple(method.return_value.type)] + '>'

	Indenter.printx('public %s %s' % (result_type, method_name))

	arguments = getattr(method, 'arguments', [])

	if not arguments:
		print('()')
	else:
		argument_types = [argument_type_map[tuple(a.type)] for a in arguments]
		argument_names = [a.name for a in arguments]

		s =  ', '.join(map(' '.join, zip(argument_types, argument_names)))
		print('(%s)' % s)

	Indenter.enter('{')
	Indenter.printline('Message message = CreateMessage();');
	Indenter.printline('message.ObjectID = ID;');
	Indenter.printline('message.CommandID = %i;' % method.id);

	if arguments:
		print

	for a in arguments:
		s = 'message.Write(%s);'
		Indenter.printline(s % a.name)

	Indenter.printline()
	Indenter.printline('{0} result = new {0}(Client, message.Cookie);'.format(result_type));
	Indenter.printline()
	Indenter.printline('Client.SendMessage(message, result);')
	Indenter.printline()
	Indenter.printline('return result;')

	Indenter.leave('}')
	Indenter.printline()

ipc = genipc.parse_xml('../../../ipc.xml')
build(ipc)
