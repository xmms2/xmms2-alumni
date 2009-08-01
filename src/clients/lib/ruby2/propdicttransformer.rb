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
	class PropDictTransformer
		SOURCE_PREFERENCES = %w[
			server
			client/*
			plugin/id3v2
			plugin/segment
			plugin/*
			*
		].freeze

		def initialize(preferences = SOURCE_PREFERENCES)
			@preferences = preferences
		end

		def transform(propdict)
			flat_dict = {}

			propdict.each do |key, inner_dict|
				_, value = inner_dict.min_by do |inner_key, _|
					find_matching_index inner_key
				end

				flat_dict[key] = value
			end

			flat_dict
		end

		def find_matching_index(key)
			@preferences.find_index do |pref|
				source_match_pattern(key, pref)
			end
		end

		def source_match_pattern(source, pattern)
			# exact match?
			if source == pattern
				true

			# pattern not ending in an asterisk
			elsif pattern[-1] != ?*
				false

			# pattern == '*'
			elsif pattern.length == 1
				true

			# perform actual pattern matching
			else
				lpos = pattern.length - 1

				source[0, lpos] == pattern[0, lpos]
			end
		end
	end
end
