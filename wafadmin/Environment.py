#! /usr/bin/env python
# encoding: utf-8
# Thomas Nagy, 2005 (ita)

"""Environment representation

There is one gotcha: getitem returns [] if the contents evals to False
This means env['foo'] = {}; print env['foo'] will print [] not {}
"""

import os,types, copy, re
import Params
from Params import debug, warning
from Constants import *
re_imp = re.compile('^(#)*?([^#=]*?)\ =\ (.*?)$', re.M)

g_cache_max = {}

g_idx = 0
class Environment(object):
	"""A safe-to-use dictionary, but do not attach functions to it please (break cPickle)
	An environment instance can be stored into a file and loaded easily
	"""
	__slots__ = ("m_idx", "m_table", "m_parent")
	def __init__(self):
		global g_idx
		self.m_idx = g_idx
		g_idx += 1
		self.m_table={}
		#self.m_parent = None <- set only if necessary

		# set the prefix once and for everybody on creation (configuration)
		self.m_table['PREFIX'] = Params.g_options.prefix

	def __contains__(self, key):
		if key in self.m_table: return True
		try: return self.m_parent.__contains__(key)
		except AttributeError: return False # m_parent may not exist

	def set_variant(self, name):
		self.m_table[VARIANT] = name

	def variant(self):
		env = self
		while 1:
			try:
				return env.m_table[VARIANT]
			except KeyError:
				try: env = env.m_parent
				except AttributeError: return DEFAULT

	def copy(self):
		newenv = Environment()
		# FIXME prefix is set each time
		if self['PREFIX']: del newenv.m_table['PREFIX']
		newenv.m_parent = self
		return newenv

	def __str__(self):
		return "environment table\n"+str(self.m_table)

	def __getitem__(self, key):
		x = self.m_table.get(key, None)
		if x: return x
		try:
			u = self.m_parent
		except AttributeError:
			return []
		else:
			return u[key]


		try:
			return self.m_table[key]
		except KeyError:
			try: return self.m_parent[key]
			except: return []

	def __setitem__(self, key, value):
		self.m_table[key] = value

	def get_flat(self, key):
		s = self[key]
		if not s: return ''
		elif isinstance(s, list): return ' '.join(s)
		else: return s

	def _get_list_value_for_modification(self, key):
		"""Gets a value that must be a list for further modification.  The
		list may be modified inplace and there is no need to
		"self.m_table[var] = value" afterwards.
		"""
		try:
			value = self.m_table[key]
		except KeyError:
			try: value = self.m_parent[key]
			except AttributeError: value = []
			if isinstance(value, list):
				value = copy.copy(value)
			else:
				value = [value]
			self.m_table[key] = value
			return value
		else:
			if isinstance(value, list):
				return value # no need to copy the list, it is not borrowed <- TODO ??
			else:
				value = [value]
				self.m_table[key] = value
				return value

	def append_value(self, var, value):
		current_value = self._get_list_value_for_modification(var)

		if isinstance(value, list):
			current_value.extend(value)
		else:
			current_value.append(value)

	def prepend_value(self, var, value):
		current_value = self._get_list_value_for_modification(var)

		if isinstance(value, list):
			current_value = value + current_value
			# a new list: update the dictionary entry
			self.m_table[var] = current_value
		else:
			current_value.insert(0, value)

	# prepend unique would be ambiguous
	def append_unique(self, var, value):
		current_value = self._get_list_value_for_modification(var)

		if isinstance(value, list):
			for value_item in value:
				if value_item not in current_value:
					current_value.append(value_item)
		else:
			if value not in current_value:
				current_value.append(value)

	def store(self, filename):
		"Write the variables into a file"
		file = open(filename, 'w')

		# compute a merged table
		table_list = []
		env = self
		while 1:
			table_list.insert(0, env.m_table)
			try: env = env.m_parent
			except AttributeError: break
		merged_table = dict()
		for table in table_list:
			merged_table.update(table)

		keys = merged_table.keys()
		keys.sort()
		for k in keys: file.write('%s = %r\n' % (k, merged_table[k]))
		file.close()

	def load(self, filename):
		"Retrieve the variables from a file"
		tbl = self.m_table
		file = open(filename, 'r')
		code = file.read()
		file.close()
		for m in re_imp.finditer(code):
			g = m.group
			tbl[g(2)] = eval(g(3))
		debug(self.m_table, 'env')

	def get_destdir(self):
		"return the destdir, useful for installing"
		if self.__getitem__('NOINSTALL'): return ''
		return Params.g_options.destdir

	def sign_vars(self, vars_list):
		" ['CXX', ..] -> [env['CXX'], ..]"

		# ccroot objects use the same environment for building the .o at once
		# the same environment and the same variables are used
		s = str([self.m_idx]+vars_list)
		try: return g_cache_max[s]
		except KeyError: pass

		lst = [self.get_flat(a) for a in vars_list]
		ret = Params.h_list(lst)
		if Params.g_zones: debug("%s %s" % (Params.view_sig(ret), str(lst)), 'envhash')

		# next time
		g_cache_max[s] = ret
		return ret

