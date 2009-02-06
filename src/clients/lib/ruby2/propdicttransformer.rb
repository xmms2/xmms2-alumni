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
	class PropDictTransformer
		SOURCE_PREFERENCES = %w[
			server
			client/*
			plugin/id3v2
			plugin/segment
			plugin/*
			*
		].freeze

		def self.transform(propdict, preferences = SOURCE_PREFERENCES)
			flat_dict = {}

			propdict.each do |key, value|
				best_value = nil
				best_index = nil

				value.each do |inner_key, inner_value|
					match_index = find_matching_index(inner_key,
					                                  preferences)
					next if match_index.nil?

					if best_index.nil? || match_index < best_index
						best_value = inner_value
						best_index = match_index
					end
				end

				unless best_value.nil?
					flat_dict[key] = best_value
				end
			end

			flat_dict
		end

		def self.find_matching_index(key, preferences)
			preferences.each_with_index do |pref, i|
				if source_match_pattern(key, pref)
					return i
				end
			end

			nil
		end

		def self.source_match_pattern(source, pattern)
			# exact match?
			if source == pattern
				true

			# useless pattern
			elsif pattern.empty?
				false

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
