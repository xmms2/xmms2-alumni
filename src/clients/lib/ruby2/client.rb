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

require 'socket'

module Xmms::Client

	class MethodGroup
		def initialize(client)
			@client = client
		end
	end

	class Client
		MAX_COOKIE = 2 ** 29
		PROTOCOL_VERSION = 16

		attr_reader :name, :socket,
		            :playlist, :config, :playback, :medialib, :collection,
		            :mediainfo_reader, :visualization, :xform, :bindata

		def initialize(name)
			@name = name

			@main = Main.new self
			@playlist = Playlist.new self
			@config = Config.new self
			@playback = Playback.new self
			@medialib = Medialib.new self
			@collection = Collection.new self
			@mediainfo_reader = MediainfoReader.new self
			@visualization = Visualization.new self
			@xform = Xform.new self
			@bindata = Bindata.new self

			@socket = nil
			@next_cookie = 0

			# results that we're waiting on
			@results = []

			# messages waiting to be written
			@send_queue = []

			# the message currently being read
			@read_message = nil
		end

		def connect(ipc_path = ENV['XMMS_PATH'])
			ipc_path = ipc_path.to_str

			# parse an expression like 'tcp://host:port'
			re = /^(\w+):\/\/([^:]+)(:(\d+))?$/
			md = re.match(ipc_path)

			protocol = md.captures.first

			case protocol
			when 'tcp'
				host = md.captures[1]
				port = md.captures[3].to_i

				@socket = TCPSocket.new(host, port)
			when 'unix'
				@socket = UNIXSocket.new md.captures[1]
			else
				raise "invalid protocol -- #{protocol}"
			end

			result = @main.hello PROTOCOL_VERSION, @name
			result.wait

			nil
		end

		def on_disconnect(&block)
			@disconnect_block = block
		end

		def quit
			@main.quit
		end

		def stats
			@main.stats
		end

		def plugin_list
			@main.plugin_list 0
		end

		# FIXME: code duplication
		def send_message(message)
			message.assemble get_next_cookie

			@send_queue << message

			result = Result.new(self, message.cookie)
			@results << result
			result
		end

		# FIXME: code duplication
		def send_signal_message(message, signal_id)
			message.assemble get_next_cookie

			@send_queue << message

			result = SignalResult.new(self, message.cookie, signal_id)
			@results << result
			result
		end

		# FIXME: code duplication
		def send_restart_message(message, result)
			cookie = get_next_cookie

			message.assemble cookie

			@send_queue << message

			result.cookie = cookie
			result
		end

		# FIXME: code duplication
		def wait_for(result)
			flush_send_queue

			loop do
				message = Message.new

				begin
					done = message.read_chunk @socket
				end until done

				process_message message

				break if message.cookie == result.cookie
			end
		end

		protected
		def io_in_handle
			@read_message ||= Message.new

			done = @read_message.read_chunk @socket

			if done
				process_message @read_message
				@read_message = nil
			end
		end

		def process_message(message)
			# find the result that this message refers to
			index = @results.find_index do |result|
				result.cookie == message.cookie
			end

			if index.nil?
				raise 'cannot find result for cookie'
			end

			found = @results[index]
			can_be_invoked_again = found.process_reply message

			unless can_be_invoked_again
				@results.delete_at index
			end
		end

		def io_out_handle
			message = @send_queue.first

			# write the next chunk of the message
			done = message.write_chunk @socket

			# if this message was written completely,
			# we remove it from the queue.
			if done
				@send_queue.shift
			end
		end

		def io_want_out
			!@send_queue.empty?
		end

		private
		def get_next_cookie
			cookie = @next_cookie

			if @next_cookie == MAX_COOKIE
				@next_cookie = 0
			else
				@next_cookie += 1
			end

			cookie
		end

		def flush_send_queue
			while io_want_out
				io_out_handle
			end
		end
	end
end
