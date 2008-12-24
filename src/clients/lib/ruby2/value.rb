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

module Xmms::Client
	class Value
		def self.deserialize(message)
			type = message.read_int

			case type
			when 0x00 # none
				nil
			when 0x01 # error
				"error"
			when 0x02, 0x03 # uint/int
				message.read_int
			when 0x04 # string
				message.read_string
			when 0x05 # coll
			when 0x06 # bin
			when 0x07 # list
				length = message.read_int

				ary = Array.new length

				length.times do |i|
					ary[i] = Value.deserialize message
				end

				ary
			when 0x08 # dict
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
	end

	class ErrorValue < Value
		attr_reader :message

		def initialize(message)
			@message = message
		end
	end
end
