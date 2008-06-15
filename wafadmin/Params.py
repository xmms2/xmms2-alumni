#! /usr/bin/env python
# encoding: utf-8
# Thomas Nagy, 2005-2008 (ita)

"Main parameters"

import os, sys, types, inspect, base64, time
from Utils import md5
import Constants, Utils

# updated from the top-level wscript
g_version="1.4.2"

g_rootname = ''
g_progress = '\x1b[K%s%s%s\r'
if sys.platform=='win32':
	# get the first two letters (c:)
	g_rootname = os.getcwd()[:2]
	g_progress = '\x1b[A\x1b[K%s%s%s\r'

g_autoconfig = 0
"reconfigure the project automatically"

# =================================== #
# Constants set on runtime

g_cwd_launch = None
"directory from which waf was called"

g_tooldir=''
"Tools directory (used in particular by Environment.py)"

g_options = None
"Parsed command-line arguments in the options module"

g_commands = {}
"build, configure, .."

g_verbose = 0
"-v: warnings, -vv: developer info, -vvv: all info"

g_build = None
"only one build object is active at a time"

g_platform = sys.platform
"current platform"

g_cache_global = ''
"config cache directory"

g_conf_name = 'conf-runs-%s-%d.pickle' % (sys.platform, Constants.ABI)

g_install = 0
"true if install or uninstall is set"

try: g_cache_global = os.path.abspath(os.environ['WAFCACHE'])
except KeyError: pass

try: g_lockfile = os.environ['WAFLOCK']
except KeyError: g_lockfile = '.lock-wscript'

# =================================== #
# HELPERS

#g_col_names = ['BOLD', 'RED', 'REDP', 'GREEN', 'YELLOW', 'BLUE', 'CYAN', 'NORMAL']
#"color names"

g_col_scheme = [1, 91, 33, 92, 93, 94, 96, 0]

g_colors = {
'BOLD'  :'\033[01;1m',
'RED'   :'\033[01;91m',
'REDP'  :'\033[01;33m',
'GREEN' :'\033[01;92m',
'YELLOW':'\033[00;33m',
'PINK'  :'\033[00;35m',
'BLUE'  :'\033[01;34m',
'CYAN'  :'\033[01;36m',
'NORMAL':'\033[0m'
}
"colors used for printing messages"

g_cursor_on ='\x1b[?25h'
g_cursor_off='\x1b[?25l'

def reset_colors():
	global g_colors
	for k in g_colors.keys():
		g_colors[k]=''
		g_cursor_on=''
		g_cursor_off=''

if (sys.platform=='win32') or ('NOCOLOR' in os.environ) \
	or (os.environ.get('TERM', 'dumb') in ['dumb', 'emacs']) \
	or (not sys.stdout.isatty()):
	reset_colors()

def pprint(col, str, label=''):
	try: mycol=g_colors[col]
	except KeyError: mycol=''
	print "%s%s%s %s" % (mycol, str, g_colors['NORMAL'], label)

g_levels={
'Action' : 'GREEN',
'Build'  : 'CYAN',
'KDE'    : 'REDP',
'Node'   : 'GREEN',
'Object' : 'GREEN',
'Runner' : 'REDP',
'Task'   : 'GREEN',
'Test'   : 'GREEN',
}

g_zones = []

def set_trace(a, b, c):
	Utils.g_trace=a
	Utils.g_debug=b
	Utils.g_error=c

def get_trace():
	return (Utils.g_trace, Utils.g_debug, Utils.g_error)

def niceprint(msg, type='', module=''):
	#if not module:
	#	print '%s: %s'% (type, msg)
	#	return
	def print_pat(color):
		print '%s %s<%s>%s %s' % (type, g_colors[color], module, g_colors['NORMAL'], msg)

	if type == 'ERROR' or type == 'WARNING':
		print_pat('RED')
		return
	if type=='DEBUG':
		print_pat('CYAN')
		return
	if module in g_levels:
		print_pat(g_levels[module])
		return
	print 'TRACE <%s> %s'% (module, msg)

def __get_module():
	try: return inspect.stack()[2][0].f_globals['__name__']
	except (IndexError, KeyError): return "unknown"

def debug(msg, zone=None):
	global g_zones, g_verbose
	if g_zones:
		if (not zone in g_zones) and (not '*' in g_zones):
			return
	elif not g_verbose>2:
		return
	module = __get_module()

	msg = time.strftime('%%X %s' % msg)
	niceprint(msg, 'DEBUG', module)

def warning(msg, zone=0):
	module = __get_module()
	niceprint(msg, 'WARNING', module)

def error(msg):
	if not Utils.g_error: return
	module = __get_module()
	niceprint(msg, 'ERROR', module)

def fatal(msg, ret=1):
	module = __get_module()
	if g_verbose > 0:
		pprint('RED', '%s \n (error raised in module %s)' % (msg, module))
	else:
		pprint('RED', '%s' % msg)
	if g_verbose > 1:
		import traceback
		traceback.print_stack()
	sys.exit(ret)

def view_sig(s):
	"used for displaying signatures"
	if type(s) is types.StringType:
		n = base64.encodestring(s)
		return n[:-2]
	else:
		return str(s)

def hash_sig(o1, o2):
	"hash two signatures"
	m = md5()
	m.update(o1)
	m.update(o2)
	return m.digest()

def h_file(filename):
	f = file(filename,'rb')
	m = md5()
	readBytes = 100000
	while (readBytes):
		readString = f.read(readBytes)
		m.update(readString)
		readBytes = len(readString)
	f.close()
	return m.digest()

# Another possibility, faster (projects with more than 15000 files) but less accurate (cache)
# based on the path, md5 hashing can be used for some files and timestamp for others
#def h_file(filename):
#	st = os.stat(filename)
#	import stat
#	if stat.S_ISDIR(st): raise IOError, 'not a file'
#	m = md5()
#	m.update(st.st_mtime)
#	m.update(st.st_size)
#	return m.digest()

def h_string(str):
	m = md5()
	m.update(str)
	return m.digest()

def h_list(lst):
	m = md5()
	m.update(str(lst))
	return m.digest()

_hash_blacklist_types = (
	types.BuiltinFunctionType,
	types.ModuleType,
	types.FunctionType,
	types.ClassType,
	types.TypeType,
	types.NoneType,
	)

def hash_function_with_globals(prevhash, func):
	"""
	hash a function (object) and the global vars needed from outside
	ignore unhashable global variables (lists)

	prevhash -- previous hash value to be combined with this one;
	if there is no previous value, zero should be used here

	func -- a Python function object.
	"""
	assert type(func) is types.FunctionType
	for name, value in func.func_globals.iteritems():
		if type(value) in _hash_blacklist_types:
			continue
		if isinstance(value, type):
			continue
		try:
			prevhash = hash( (prevhash, name, value) )
		except TypeError: # raised for unhashable elements
			pass
		#else:
		#	print "hashed: ", name, " => ", value, " => ", hash(value)
	return hash( (prevhash, inspect.getsource(func)) )

