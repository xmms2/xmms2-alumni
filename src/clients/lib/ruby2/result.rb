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
	class Result
		attr_reader :cookie

		def initialize(client, cookie)
			@client = client
			@cookie = cookie

			@notifier = nil
			@value = nil
			@has_value = false
		end

		def notifier(&block)
			@notifier = block
		end

		def value
			unless @has_value
				raise "Result not processed yet"
			end

			@value
		end

		def process_reply(message)
			is_signal = false
			can_be_invoked_again = false

			# check whether this is a signal or a broadcast message
			if message.object_id == 0
				@value = Value.deserialize message

				is_signal = message.command_id == 0x20
				can_be_invoked_again = !is_signal
			else
				case message.command_id
				when 0 # _REPLY
					@value = Value.deserialize message
				when 1 # _ERROR
					error = message.read_string
					@value = ErrorValue.new(error)
				else
					raise "unhandled command_id: 0x%x" % message.command_id
				end
			end

			@has_value = true

			unless @notifier.nil?
				x = @notifier.call @value

				if is_signal && x == true
					m = Message.new
					m.object_id = message.object_id
					m.command_id = message.command_id
					m.write_int signal_id

					@client.send_restart_message m, self

					can_be_invoked_again = true
				end
			end

			can_be_invoked_again
		end

		def wait
			@client.wait_for self

			self
		end
	end

	class SignalResult < Result
		attr_reader :signal_id

		def initialize(client, cookie, signal_id)
			super(client, cookie)

			@signal_id = signal_id
		end

		def cookie=(c)
			@cookie = c
		end
	end
end
