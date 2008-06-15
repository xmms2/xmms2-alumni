#! /usr/bin/env python
# encoding: utf-8
# Thomas Nagy, 2005 (ita)

"Module called for configuring, compiling and installing targets"

import os, sys, shutil, cPickle, traceback

import Params, Utils, Configure, Build, Runner, Options
from Params import error, fatal, warning, g_lockfile
from Constants import *

g_gz = 'bz2'
g_dirwatch = None
g_daemonlock = 0
g_excludes = '.svn CVS .arch-ids {arch} SCCS BitKeeper .hg'.split()
"exclude folders from dist"
g_dist_exts = '~ .rej .orig .pyc .pyo .bak config.log .tar.bz2 .zip Makefile Makefile.in'.split()
"exclude files from dist"

g_distclean_exts = '~ .pyc .wafpickle'.split()

def add_subdir(dir, bld):
	"each wscript calls bld.add_subdir"
	try: bld.rescan(bld.m_curdirnode)
	except OSError: fatal("No such directory "+bld.m_curdirnode.abspath())

	old = bld.m_curdirnode
	new = bld.m_curdirnode.find_dir(dir)
	if new is None:
		fatal("subdir not found (%s), restore is %s" % (dir, bld.m_curdirnode))

	bld.m_curdirnode=new
	# try to open 'wscript_build' for execution
	# if unavailable, open the module wscript and call the build function from it
	from Common import install_files, install_as, symlink_as # do not remove
	try:
		file_path = os.path.join(new.abspath(), WSCRIPT_BUILD_FILE)
		file = open(file_path, 'r')
		exec file
		if file: file.close()
	except IOError:
		file_path = os.path.join(new.abspath(), WSCRIPT_FILE)
		module = Utils.load_module(file_path)
		module.build(bld)

	# restore the old node position
	bld.m_curdirnode = old

def call_back(idxName, pathName, event):
	#print "idxName=%s, Path=%s, Event=%s "%(idxName, pathName, event)
	# check the daemon lock state
	global g_daemonlock
	if g_daemonlock: return
	g_daemonlock = 1

	# clean up existing variables, and start a new instance
	Utils.reset()
	main()
	g_daemonlock = 0

def start_daemon():
	"if it does not exist already:start a new directory watcher; else: return immediately"
	global g_dirwatch
	if not g_dirwatch:
		import DirWatch
		g_dirwatch = DirWatch.DirectoryWatcher()
		m_dirs=[]
		for nodeDir in Params.g_build.m_srcnode.dirs():
			tmpstr = "%s" %nodeDir
			tmpstr = "%s" %(tmpstr[6:])
			m_dirs.append(tmpstr)
		g_dirwatch.add_watch("tmp Test", call_back, m_dirs)
		# infinite loop, no need to exit except on ctrl+c
		g_dirwatch.loop()
		g_dirwatch = None
	else:
		g_dirwatch.suspend_all_watch()
		m_dirs=[]
		for nodeDir in Params.g_build.m_srcnode.dirs():
			tmpstr = "%s" % nodeDir
			tmpstr = "%s" % (tmpstr[6:])
			m_dirs.append(tmpstr)
		g_dirwatch.add_watch("tmp Test", call_back, m_dirs)

def configure():
	# disable parallelization while configuring
	jobs_save = Params.g_options.jobs
	Params.g_options.jobs = 1

	Runner.set_exec('normal')
	tree = Build.Build()

	err = 'The %s is not given in %s:\n * define a top level attribute named "%s"\n * run waf configure --%s=xxx'

	src = getattr(Params.g_options, SRCDIR, None)
	if not src: src = getattr(Utils.g_module, SRCDIR, None)
	if not src: fatal(err % (SRCDIR, os.path.abspath('.'), SRCDIR, SRCDIR))

	bld = getattr(Params.g_options, BLDDIR, None)
	if not bld: bld = getattr(Utils.g_module, BLDDIR, None)
	if not bld: fatal(err % (BLDDIR, os.path.abspath('.'), BLDDIR, BLDDIR))

	Params.g_cachedir = os.path.join(bld, CACHE_DIR)
	tree.load_dirs(src, bld, isconfigure=1)
	tree.init_variants()

	conf = Configure.Configure(srcdir=src, blddir=bld)

	# first remove the log file if it exists
	try: os.unlink(os.path.join(bld, Configure.Configure.log_file))
	except (OSError, IOError): pass

	conf.mute_logging()
	try:
		# calling to main wscript's configure()
		conf.sub_config('')
	except Configure.ConfigurationError, e:
		fatal(str(e), 2)
	except Exception:
		Utils.test_full()
		raise
	conf.restore_logging()

	conf.store(tree)
	conf.cleanup()

	# this will write a configure lock so that subsequent run will
	# consider the current path as the root directory, to remove: use 'waf distclean'
	file = open(g_lockfile, 'w')
	file.write

	proj = {}
	proj[BLDDIR] = bld
	proj[SRCDIR] = src
	proj['argv'] = sys.argv
	proj['hash'] = conf.hash
	proj['files'] = conf.files
	cPickle.dump(proj, file)
	file.close()

	# restore -j option
	Params.g_options.jobs = jobs_save

def read_cache_file(filename):
	file = open(g_lockfile, 'r')
	proj = cPickle.load(file)
	file.close()
	return proj

def prepare():
	# some command-line options can be processed immediately
	if '--version' in sys.argv:
		opt_obj = Options.Handler()
		opt_obj.parse_args()
		sys.exit(0)

	# now find the wscript file
	msg1 = 'Waf: *** Nothing to do! Please run waf from a directory containing a file named "%s"' % WSCRIPT_FILE

	# Some people want to configure their projects gcc-style:
	# mkdir build && cd build && ../waf configure && ../waf
	# check that this is really what is wanted
	build_dir_override = None
	candidate = None

	cwd = Params.g_cwd_launch
	lst = os.listdir(cwd)
	xml = 0

	#check if a wscript or a wscript_xml file is in current directory
	if WSCRIPT_FILE in lst or WSCRIPT_BUILD_FILE in lst or 'wscript_xml' in lst:
		# if a script is in current directory, use this directory as candidate (and prevent gcc-like configuration)
		candidate = cwd
	elif 'configure' in sys.argv:
		# gcc-like configuration
		build_dir_override = cwd

	try:
		#check the following dirs for wscript or wscript_xml
		search_for_candidate = True
		if not candidate:
			#check first the calldir if there is wscript or wscript_xml
			#for example: /usr/src/configure the calldir would be /usr/src
			calldir = os.path.abspath(os.path.dirname(sys.argv[0]))
			lst_calldir = os.listdir(calldir)
			if WSCRIPT_FILE in lst_calldir:
				candidate = calldir
				search_for_candidate = False
			if 'wscript_xml' in lst_calldir:
				candidate = calldir
				xml = 1
				search_for_candidate = False
		if "--make-waf" in sys.argv and candidate:
			search_for_candidate = False

		#check all directories above current dir for wscript or wscript_xml if still not found
		while search_for_candidate:
			if len(cwd) <= 3:
				break # stop at / or c:
			dirlst = os.listdir(cwd)
			if WSCRIPT_FILE in dirlst:
				candidate = cwd
				xml = 0
			if 'wscript_xml' in dirlst:
				candidate = cwd
				xml = 1
				break
			if 'configure' in sys.argv and candidate:
				break
			if Params.g_lockfile in dirlst:
				break
			cwd = cwd[:cwd.rfind(os.sep)] # climb up
	except Exception:
		traceback.print_stack()
		fatal(msg1)

	if not candidate:
		# check if the user only wanted to display the help
		if '-h' in sys.argv or '--help' in sys.argv:
			warning('No wscript file found: the help message may be incomplete')
			opt_obj = Options.Handler()
			opt_obj.parse_args()
			sys.exit(0)
		else:
			fatal(msg1)

	# We have found wscript, but there is no guarantee that it is valid
	os.chdir(candidate)

	if xml:
		# the xml module is not provided by default, you will have to import it yourself
		from XMLScripting import compile
		compile(candidate+os.sep+'wscript_xml')
	else:
		# define the main module containing the functions init, shutdown, ..
		Utils.set_main_module(os.path.join(candidate, WSCRIPT_FILE))

	if build_dir_override:
		d = getattr(Utils.g_module, BLDDIR, None)
		if d:
			# test if user has set the blddir in wscript.
			msg = 'Overriding build directory %s with %s' % (d, build_dir_override)
			Params.niceprint(msg, 'WARNING', 'waf')
		Utils.g_module.blddir = build_dir_override

	# fetch the custom command-line options recursively and in a procedural way
	opt_obj = Options.Handler()
	# will call to main wscript's set_options()
	opt_obj.sub_options('')
	opt_obj.parse_args()

	# use the parser results
	if Params.g_commands['dist']:
		# try to use the user-defined dist function first, fallback to the waf scheme
		fun = getattr(Utils.g_module, 'dist', None)
		if fun: fun(); sys.exit(0)

		appname = getattr(Utils.g_module, APPNAME, 'noname')

		get_version = getattr(Utils.g_module, 'get_version', None)
		if get_version: version = get_version()
		else: version = getattr(Utils.g_module, VERSION, None)
		if not version: version = '1.0'

		from Scripting import Dist
		Dist(appname, version)
		sys.exit(0)
	elif Params.g_commands['distclean']:
		# try to use the user-defined distclean first, fallback to the waf scheme
		fun = getattr(Utils.g_module, 'distclean', None)
		if fun:	fun()
		else:	DistClean()
		sys.exit(0)
	elif Params.g_commands['distcheck']:
		# try to use the user-defined dist function first, fallback to the waf scheme
		fun = getattr(Utils.g_module, 'dist', None)
		if fun: fun(); sys.exit(0)

		appname = getattr(Utils.g_module, APPNAME, 'noname')

		get_version = getattr(Utils.g_module, 'get_version', None)
		if get_version: version = get_version()
		else: version = getattr(Utils.g_module, VERSION, None)
		if not version: version = '1.0'

		DistCheck(appname, version)
		sys.exit(0)

	fun = getattr(Utils.g_module, 'init', None)
	if fun: fun()

	Utils.python_24_guard()
	main()

def main():
	import inspect
	if Params.g_commands['configure']:
		configure()
		Params.pprint('GREEN', 'Configuration finished successfully; project is now ready to build.')
		sys.exit(0)

	Runner.set_exec('noredir')

	# compile the project and/or install the files
	bld = Build.Build()
	try:
		proj = read_cache_file(g_lockfile)
	except IOError:
		if Params.g_commands['clean']:
			fatal("Nothing to clean (project not configured)", ret=2)
		else:
			if Params.g_autoconfig:
				warning("Reconfiguring the project")
				configure()
				bld = Build.Build()
				proj = read_cache_file(g_lockfile)
			else:
				fatal("Project not configured (run 'waf configure' first)", ret=2)

	if Params.g_autoconfig:
		reconf = 0
		hash = 0
		try:
			for file in proj['files']:
				mod = Utils.load_module(file)
				hash = Params.hash_function_with_globals(hash, mod.configure)
			reconf = (hash != proj['hash'])
		except Exception, ex:
			if Params.g_verbose:
				traceback.print_exc()
			warning("Reconfiguring the project (an exception occured: %s)" % (str(ex),))
			reconf = 1

		if reconf:
			warning("Reconfiguring the project (the configuration has changed)")

			a1 = Params.g_commands
			a2 = Params.g_options
			a3 = Params.g_zones
			a4 = Params.g_verbose

			oldargs = sys.argv
			sys.argv = proj['argv']
			Options.g_parser.parse_args(args=sys.argv[1:])
			configure()
			sys.argv = oldargs

			Params.g_commands = a1
			Params.g_options = a2
			Params.g_zones = a3
			Params.g_verbose = a4

			bld = Build.Build()
			proj = read_cache_file(g_lockfile)

	Params.g_cachedir = os.path.join(proj[BLDDIR], CACHE_DIR)

	bld.load_dirs(proj[SRCDIR], proj[BLDDIR])
	bld.load_envs()

	try:
		# calling to main wscript's build()
		f = Utils.g_module.build
	except AttributeError:
		fatal("Could not find the function 'def build(bld):' in wscript")
	else:
		f(bld)

	# TODO undocumented hook
	pre_build = getattr(Utils.g_module, 'pre_build', None)
	if pre_build: pre_build()

	# compile
	if Params.g_commands['build'] or Params.g_install:
		try:

			# TODO quite ugly, no?
			if not Params.g_commands['build'] and not Params.g_commands['install']:
				import Task
				def must_run(self):
					return 0
				setattr(Task.Task, 'must_run', must_run)

			#"""
			bld.compile()
			"""
			import cProfile, pstats
			cProfile.run("Params.g_build.compile()", 'profi.txt')
			p = pstats.Stats('profi.txt')
			p.sort_stats('time').print_stats(20)
			#"""

		except Build.BuildError, e:
			if not Params.g_options.daemon: fatal(e.get_message(), 1)
			else: error(e.get_message())
		else:
			if Params.g_options.progress_bar: print ''

			if Params.g_install:
				bld.install()

			if Params.g_commands['install']: msg = 'Compilation and installation finished successfully'
			elif Params.g_commands['uninstall']: msg = 'Uninstallation finished successfully'
			else: msg = 'Compilation finished successfully'
			Params.pprint('GREEN', msg)

	# clean
	if Params.g_commands['clean']:
		try:
			bld.clean()
			Params.pprint('GREEN', 'Cleaning finished successfully')
		finally:
			bld.save()
		#if ret:
		#	msg='Cleanup failed for a mysterious reason'
		#	error(msg)

	# daemon look here
	if Params.g_options.daemon and Params.g_commands['build']:
		start_daemon()
		return

	# shutdown
	fun = getattr(Utils.g_module, 'shutdown', None)
	if fun: fun()


## Note: this is a modified version of shutil.copytree from python
## 2.5.2 library; modified for WAF purposes to exclude dot dirs and
## another list of files.
def copytree(src, dst, symlinks=False, excludes=(), build_dir=None):
	names = os.listdir(src)
	os.makedirs(dst)
	errors = []
	for name in names:
		srcname = os.path.join(src, name)
		dstname = os.path.join(dst, name)
		try:
			if symlinks and os.path.islink(srcname):
				linkto = os.readlink(srcname)
				os.symlink(linkto, dstname)
			elif os.path.isdir(srcname):
				if name in excludes:
					continue
				elif name.startswith('.') or name.startswith(',,') or name.startswith('++'):
					continue
				elif name == build_dir:
					continue
				else:
					## build_dir is not passed into the recursive
					## copytree, but that is intentional; it is a
					## directory name valid only at the top level.
					copytree(srcname, dstname, symlinks, excludes)
			else:
				ends = name.endswith
				to_remove = False
				if name.startswith('.') or name.startswith('++'):
					to_remove = True
				else:
					for x in g_dist_exts:
						if ends(x):
							to_remove = True
							break
				if not to_remove:
					shutil.copy2(srcname, dstname)
			# XXX What about devices, sockets etc.?
		except (IOError, os.error), why:
			errors.append((srcname, dstname, str(why)))
		# catch the Error from the recursive copytree so that we can
		# continue with other files
		except shutil.Error, err:
			errors.extend(err.args[0])
	try:
		shutil.copystat(src, dst)
	except WindowsError:
		# can't copy file access times on Windows
		pass
	except OSError, why:
		errors.extend((src, dst, str(why)))
	if errors:
		raise shutil.Error, errors


def DistDir(appname, version):
	"make a distribution directory with all the sources in it"

	# Our temporary folder where to put our files
	TMPFOLDER=appname+'-'+version

	# Remove an old package directory
	if os.path.exists(TMPFOLDER): shutil.rmtree(TMPFOLDER)

	global g_dist_exts, g_excludes

	# Remove the Build dir
	build_dir = getattr(Utils.g_module, BLDDIR, None)

	# Copy everything into the new folder
	copytree('.', TMPFOLDER, excludes=g_excludes, build_dir=build_dir)

	# TODO undocumented hook
	dist_hook = getattr(Utils.g_module, 'dist_hook', None)
	if dist_hook:
		os.chdir(TMPFOLDER)
		try:
			dist_hook()
		finally:
			# go back to the root directory
			os.chdir('..')
	return TMPFOLDER

def DistTarball(appname, version):
	"""make a tarball with all the sources in it; return (distdirname, tarballname)"""
	import tarfile

	TMPFOLDER = DistDir(appname, version)
	tar = tarfile.open(TMPFOLDER+'.tar.'+g_gz,'w:'+g_gz)
	tar.add(TMPFOLDER)
	tar.close()
	Params.pprint('GREEN', 'Your archive is ready -> %s.tar.%s' % (TMPFOLDER, g_gz))

	if os.path.exists(TMPFOLDER): shutil.rmtree(TMPFOLDER)
	return (TMPFOLDER, TMPFOLDER+'.tar.'+g_gz)

def Dist(appname, version):
	"""make a tarball with all the sources in it"""
	DistTarball(appname, version)
	sys.exit(0)

def DistClean():
	"""clean the project"""

	# remove the temporary files
	# the builddir is given by lock-wscript only
	# we do no try to remove it if there is no lock file (rmtree)
	for (root, dirs, filenames) in os.walk('.'):
		for f in list(filenames):
			to_remove = 0
			if f==g_lockfile:
				# removes a lock, and the builddir indicated
				to_remove = True
				try:
					proj = read_cache_file(os.path.join(root, f))
					shutil.rmtree(os.path.join(root, proj[BLDDIR]))
				except (OSError, IOError):
					# ignore errors if the lockfile or the builddir not exist.
					pass
			else:
				ends = f.endswith
				for x in g_distclean_exts:
					if ends(x):
						to_remove = 1
						break
			if to_remove:
				os.remove(os.path.join(root, f))
	lst = os.listdir('.')
	for f in lst:
		if f.startswith('.waf-'):
			shutil.rmtree(f, ignore_errors=True)
	Params.pprint('GREEN', "distclean finished successfully")
	sys.exit(0)

def DistCheck(appname, version):
	"""Makes some sanity checks on the waf dist generated tarball"""
	import tempfile
	import pproc as subprocess

	waf = os.path.abspath(sys.argv[0])
	distdir, tarball = DistTarball(appname, version)
	retval = subprocess.Popen('bzip2 -dc %s | tar x' % tarball, shell=True).wait()
	if retval:
		Params.fatal('uncompressing the tarball failed with code %i' % (retval))

	instdir = tempfile.mkdtemp('.inst', '%s-%s' % (appname, version))
	cwd_before = os.getcwd()
	os.chdir(distdir)
	try:
		retval = subprocess.Popen(
			'%(waf)s configure && %(waf)s '
			'&& %(waf)s check && %(waf)s install --destdir=%(instdir)s'
			' && %(waf)s uninstall --destdir=%(instdir)s' % vars(),
			shell=True).wait()
		if retval:
			Params.fatal('distcheck failed with code %i' % (retval))
	finally:
		os.chdir(cwd_before)
	shutil.rmtree(distdir)
	if os.path.exists(instdir):
		Params.fatal("distcheck succeeded, but files were left in %s" % (instdir))
	else:
		Params.pprint('GREEN', "distcheck finished successfully")

