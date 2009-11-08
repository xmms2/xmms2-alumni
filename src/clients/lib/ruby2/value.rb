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

module Xmms
	module Client
	end
end

module Xmms::Client
	class Value

		ID_NONE = 0x00
		ID_ERROR = 0x01
		ID_INT = 0x02
		ID_STRING = 0x03
		ID_COLLECTION = 0x04
		ID_BINARY = 0x05
		ID_LIST = 0x06
		ID_DICTIONARY = 0x07

		def self.deserialize(message)
			type = message.read_int

			case type
			when ID_NONE
				nil
			when ID_ERROR
				"error"
			when ID_INT
				message.read_int
			when ID_STRING
				message.read_string
			when ID_COLLECTION
			when ID_BINARY
			when ID_LIST
				length = message.read_int

				ary = Array.new length

				length.times do |i|
					ary[i] = Value.deserialize message
				end

				ary
			when ID_DICTIONARY
				length = message.read_int

				hash = Hash.new

				length.times do |i|
					key = message.read_string
					value = Value.deserialize message

					hash[key] = value
				end

				hash
			else
				raise "unknown value type 0x%x" % type
			end
		end

		def self.serialize(value, message)
			case value
			when Integer
				message.write_int ID_INT
				message.write_int value
			when String
				message.write_int ID_STRING
				message.write_string value
			when Array
				message.write_int ID_LIST
				message.write_int value.length

				value.each do |item|
					serialize(item, message)
				end
			end
		end
	end

	class ErrorValue < Value
		attr_reader :message

		def initialize(message)
			@message = message
		end
	end
end
