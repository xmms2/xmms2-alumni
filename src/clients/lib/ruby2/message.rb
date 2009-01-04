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
	class Message
		HEADER_LENGTH = 16

		attr_accessor :object_id, :command_id
		attr_reader :cookie

		def initialize
			@object_id = nil
			@command_id = nil
			@cookie = nil
			@payload_length = 0

			@transferred = 0

			@payload_offset = HEADER_LENGTH
			@raw_data = ''
		end

		# returns false if more data needs to be written
		def write_cb(socket)
			left = HEADER_LENGTH + @payload_length - @transferred
			chunk = @raw_data[@transferred, left]
			written = socket.syswrite chunk

			@transferred += written
			left -= written

			left.zero?
		end

		def read_cb(socket)
			if @transferred < HEADER_LENGTH
				chunk = socket.sysread HEADER_LENGTH - @transferred
				@raw_data << chunk
				@transferred += chunk.size

				if @transferred == HEADER_LENGTH
					items = @raw_data.unpack 'NNNN'

					@object_id = items.shift
					@command_id = items.shift
					@cookie = items.shift
					@payload_length = items.shift
				end
			end

			total_length = HEADER_LENGTH + @payload_length

			chunk = socket.sysread total_length - @transferred
			@raw_data << chunk
			@transferred += chunk.size

			@transferred == total_length
		end

		def assemble(cookie)
			@cookie = cookie

			header = [
				@object_id, @command_id, @cookie, @payload_length
			].pack 'NNNN'

			@raw_data = header << @raw_data
		end

		def self.check_int(n)
			n = n.to_int

			if n > 0x7fffffff || n < -0x80000000
				raise ArgumentError, 'expected 32 bit signed integer'
			end

			n
		end

		def write_int(n)
			chunk = [n].pack 'N'
			@raw_data << chunk

			@payload_length += 4

			self
		end

		def self.check_uint(n)
			n = n.to_int

			if n > 0xffffffff || n < 0
				raise ArgumentError, 'expected 32 bit unsigned integer'
			end

			n
		end

		def write_uint(n)
			write_int n
		end

		def self.check_string(s)
			s.to_str
		end

		def write_string(s)
			z_length = s.length + 1

			write_uint z_length

			@raw_data << s << "\0"

			@payload_length += z_length

			self
		end

		def read_int
			value = @raw_data[@payload_offset, 4].unpack("N").first
			@payload_offset += 4

			value
		end

		def read_string
			length = read_int

			if length.zero?
				""
			else
				s = @raw_data[@payload_offset, length - 1]
				@payload_offset += length

				s
			end
		end
	end
end
