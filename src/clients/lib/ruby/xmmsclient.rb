#  XMMS2 - X Music Multiplexer System
#  Copyright (C) 2003-2011 XMMS2 Team
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

require 'xmmsclient_ext'

module Xmms
	class Collection
		# :call-seq:
		#  c.has_field(field) -> collection
		#
		# Returns a new collection for song whose has _field_
		# in _c_
		def has_field(field)
			c = Xmms::Collection.new(Xmms::Collection::TYPE_HAS)
			c.attributes["field"]=field
			c.operands<< self
			c
		end

		# :call-seq:
		#  c.field_equal(field,value) -> collection
		#
		# Returns a new collection for song whose _field_ is _value_
		# in _c_
		def field_equal(field, value)
			c = Xmms::Collection.new(Xmms::Collection::TYPE_EQUALS)
			c.attributes["field"]=field
			c.attributes["value"]=value
			c.operands<< self
			c
		end

		# :call-seq:
		#  c.field_match(field,value) -> collection
		#
		# Returns a new collection for song whose _field_ match _value_
		# in _c_
		def field_match(field, value)
			c = Xmms::Collection.new(Xmms::Collection::TYPE_MATCH)
			c.attributes["field"]=field
			c.attributes["value"]=value
			c.operands<< self
			c
		end

		# :call-seq:
		#  c.field_smaller(field,value) -> collection
		#
		# Returns a new collection for song whose _field_ is smaller than _value_
		# in _c_
		def field_smaller(field, value)
			c = Xmms::Collection.new(Xmms::Collection::TYPE_SMALLER)
			c.attributes["field"]=field
			c.attributes["value"]=value
			c.operands<< self
			c
		end

		# :call-seq:
		#  c.field_greater(field,value) -> collection
		#
		# Returns a new collection for song whose _field_ is greater than _value_
		# in _c_
		def field_greater(field, value)
			c = Xmms::Collection.new(Xmms::Collection::TYPE_GREATER)
			c.attributes["field"]=field
			c.attributes["value"]=value
			c.operands<< self
			c
		end

		# :call-seq:
		#  c.union(other) -> collection
		#
		# Returns a new collection that is the logical OR of
		# _c_ and _other_.
		def union(other)
			c = Xmms::Collection.new(Xmms::Collection::TYPE_UNION)
			c.operands << self
			c.operands << other
			c
		end

		# :call-seq:
		#  c.intersect(other) -> collection
		#
		# Returns a new collection that is the logical AND of
		# _c_ and _other_.
		def intersect(other)
			c = Xmms::Collection.new(Xmms::Collection::TYPE_INTERSECTION)
			c.operands << self
			c.operands << other
			c
		end

		# :call-seq:
		#  c.complement -> collection
		#
		# Returns a new collection that is the logical complement of
		# _c_.
		def complement
			c = Xmms::Collection.new(Xmms::Collection::TYPE_COMPLEMENT)
			c.operands << self
			c
		end

		alias :or :union
		alias :and :intersect
		alias :not :complement
		alias :| :union
		alias :& :intersect
		alias :~@ :complement
	end
end
