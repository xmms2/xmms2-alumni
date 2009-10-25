#!/usr/bin/env python
import sys

sys.path.append('../waftools')

import genipc
from indenter import Indenter

argument_type_map = {
	('int',): 'Int',
	('string',): 'String',
	('collection',): 'Collection',
	('binary',): '[Word8]',
	('list', 'string'): '[String]',
	('dictionary', 'string'): '[(String, String)]',
}

argument_write_function_map = {
	('int',): 'messageWriteInt',
	('string',): 'messageWriteString',
	('collection',): 'messageWriteCollection',
	('binary',): 'messageWriteBinary',
	('list', 'string'): 'messageWriteStringList',
	('dictionary', 'string'): 'messageWriteStringDictionary',
}

result_value_type_map = {
	('int',): 'messageWriteInt',
	('string',): 'messageWriteString',
	('collection',): 'messageWriteCollection',
	('binary',): 'messageWriteBinary',
	('list', 'string'): 'messageWriteStringList',
	('dictionary', 'string'): 'messageWriteStringDictionary',
}



# turns strings like "foo_bar" into "FooBar".
def camel_case(s):
	return ''.join(x.capitalize() for x in s.split('_'))

def build(ipc):
	Indenter.printline('-- This code is automatically generated from foobar. Do not edit.')
	Indenter.printline()
	Indenter.printline('module Xmms.Client.Generated where')
	Indenter.printline()
	Indenter.printline('import qualified Data.ByteString.Lazy as BL')
	Indenter.printline('import Network.Socket.ByteString (sendMany)')
	Indenter.printline('import Data.Word (Word8)')
	Indenter.printline()
	Indenter.printline('import Xmms.Client')
	Indenter.printline('import Xmms.Client.Collection')
	Indenter.printline('import Xmms.Client.Message')
	Indenter.printline()
	Indenter.printline('flatten :: [[a]] -> [a]')
	Indenter.printline('flatten = foldl (++) []')
	Indenter.printline()

	for object in ipc.objects:
		for method in object.methods:
			emit_method_code(object, method, '')

		Indenter.printline()

def blah(a):
	f = argument_write_function_map[tuple(a.type)]
	return '%s %s' % (f, a.name)

# FIXME: i'm going to hell for the name_prefix stuff
def emit_method_code(object, method, name_prefix):
	method_name = object.name + name_prefix + camel_case(method.name)

	result_type = 'VoidResult'

	#if method.return_value:
		#result_type = 'ValueResult<' + result_value_type_map[tuple(method.return_value.type)] + '>'

	arguments = getattr(method, 'arguments', [])

	if not arguments:
		argument_types = []
		argument_names = []
	else:
		argument_types = [argument_type_map[tuple(a.type)] for a in arguments]
		argument_names = [a.name for a in arguments]

	argument_types.insert(0, 'Client')
	argument_names.insert(0, 'c')

	print('%s :: %s -> IO (Client, Result)' % (method_name, ('-> '.join(argument_types))))

	# now write the method definition
	Indenter.printx(method_name)

	print ' %s = do' % ' '.join(argument_names)

	Indenter.enter()

	Indenter.printline('let (betterClient, cookie) = methodPrelude c')
	Indenter.printline()

	Indenter.printx('let bls = [')

	if arguments:
		print ', '.join(map(blah, arguments)),

	print ']'

	Indenter.printline()
	Indenter.printline('let payloadLength = foldl (+) 0 (map BL.length bls)')

	Indenter.printline('let header = messageBuildHeader (%i, %i, fromIntegral cookie, fromIntegral payloadLength)'% (object.id, method.id))

	Indenter.printline('let bs = map (BL.toChunks) (header : bls)')
	Indenter.printline('sendMany (clientSocket c) (flatten bs)')
	Indenter.printline()
	Indenter.printline('return (betterClient, %s cookie)' % result_type)
	Indenter.leave(None)

ipc = genipc.parse_xml('../src/ipc.xml')
build(ipc)
