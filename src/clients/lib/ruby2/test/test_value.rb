#  XMMS2 - X Music Multiplexer System
#  Copyright (C) 2003-2008 XMMS2 Team
#
#  PLUGINS ARE NOT CONSIDERED TO BE DERIVED WORK !!!
#
#  This library is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation; either
#  version 2.1 of the License, or (at your option) any later version.
#
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.

require 'test/unit'
require 'stringio'
require 'message'
require 'value'

class TestValue < Test::Unit::TestCase
	def create_method_with_payload(payload)
		header = [
			0, 0, 0, payload.length
		].pack 'NNNN'

		message = Xmms::Client::Message.new
		message.read_chunk StringIO.new(header + payload)
		message
	end

	def test_deserialize_int
		payload = "\0\0\0\2\0\0\0\5"
		message = create_method_with_payload payload

		value = Xmms::Client::Value.deserialize message
		assert_equal 5, value
	end

	def test_deserialize_string
		payload = "\0\0\0\3\0\0\0\7foobar\0"
		message = create_method_with_payload payload

		value = Xmms::Client::Value.deserialize message
		assert_equal 'foobar', value
	end

	def test_deserialize_list
		payload =
			"\0\0\0\6\0\0\0\3\0\0\0\2\0\0\1\0\0\0\0\2\0\0\f\2" +
			"\0\0\0\3\0\0\0\2x\0"

		message = create_method_with_payload payload

		expected = [256, 3074, 'x']

		value = Xmms::Client::Value.deserialize message
		assert_equal expected, value
	end

	def test_deserialize_nested_list
		payload =
			"\0\0\0\6\0\0\0\2" +
			"\0\0\0\6\0\0\0\2\0\0\0\2\0\0\1\0\0\0\0\2\0\0\0\1" +
			"\0\0\0\6\0\0\0\2\0\0\0\2\0\0\0\x2a\0\0\0\3\0\0\0\4xyz\0"

		message = create_method_with_payload payload

		expected = [[256, 1], [42, 'xyz']]

		value = Xmms::Client::Value.deserialize message
		assert_equal expected, value
	end

	def test_deserialize_dict
		payload =
			"\0\0\0\7\0\0\0\2" +
			"\0\0\0\4foo\0\0\0\0\2\0\0\0\x2a" +
			"\0\0\0\4bar\0\0\0\0\3\0\0\0\4baz\0"

		message = create_method_with_payload payload

		expected = {
			'foo' => 42,
			'bar' => 'baz'
		}

		value = Xmms::Client::Value.deserialize message
		assert_equal expected, value
	end

	def test_deserialize_nested_dict
		payload =
			"\0\0\0\7\0\0\0\2" +
			"\0\0\0\5date\0\0\0\0\7\0\0\0\2" +
			"\0\0\0\x0eplugin/vorbis\0\0\0\0\3\0\0\0\x052006\0" +
			"\0\0\0\x0bclient/foo\0\0\0\0\3\0\0\0\x052008\0" +
			"\0\0\0\xclaststarted\0\0\0\0\7\0\0\0\2" +
			"\0\0\0\7server\0\0\0\0\2\x49\241\x63\310" +
			"\0\0\0\x0bclient/bar\0\0\0\0\2\x49\241\x63\352"

		message = create_method_with_payload payload

		expected = {
			'date' => {
				'plugin/vorbis' => '2006',
				'client/foo' => '2008'
			}, 'laststarted' => {
				'server' => 1235313608,
				'client/bar' => 1235313642
			}
		}

		value = Xmms::Client::Value.deserialize message
		assert_equal expected, value
	end
end
