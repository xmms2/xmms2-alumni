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
require 'propdicttransformer'

class TestPropDictTransformer < Test::Unit::TestCase
	def test_transform
		orig = {
			'date' => {
				'plugin/vorbis' => '2006',
				'client/foo' => '2008'
			}, 'laststarted' => {
				'server' => 1235313608,
				'client/bar' => 1235313642
			}
		}

		transformer = Xmms::Client::PropDictTransformer.new
		transformed = transformer.transform orig

		# client/* has higher priority than plugin/*
		assert_equal '2008', transformed['date']

		# server has higher priority than client/*
		assert_equal 1235313608, transformed['laststarted']
	end
end
