import os;
import sys;
import xmmsenv;
from marshal import dump, load;
from xmmsconf import checkFlags;

Help ("""XMMS2 SCons help
Build XMMS2 by running:
	
	scons

Clean up the builddir by running:

	scons -c
""")


##
## Get options
##
opts = Options(None, ARGUMENTS)
opts.Add('CC', 'C compiler to use', 'gcc')
opts.Add('CCFLAGS', 'compilerflags', '-g -Wall -O0')
opts.Add('PREFIX', 'installprefix', '/usr/local')
opts.Add('SYSCONFDIR', 'system configuration dir', '/usr/local/etc')
opts.Add('INSTALLDIR', 'runtime install dir', '')
opts.Add('SHOWCACHE', 'show what flags that lives inside cache', 0)
opts.Add('NOCACHE', 'do not use cache', 0)

## setup base environment...
## ...ok, this should be a bit configurable... later.
##
## SCons-tips 42: start paths with '#' to have them change
##                correctly when we descend into subdirs
base_env = xmmsenv.XmmsEnvironment(options=opts, LINK="gcc", CPPPATH = ['#src'])

if base_env['NOCACHE']:
	print "We don't want any cache"
	checkFlags(base_env)
else:
	try:
		statefile = open('scons.cache','rb+')
		base_env.flag_groups=load(statefile)
		print "Cachefile scons.cache found, not checking libs"
		statefile.close()
		if base_env['SHOWCACHE']:
			for x in base_env.flag_groups.keys():
				print "Module " + x + " has flags"
				if base_env['SHOWCACHE'] == "2":
					print "\t" + base_env.flag_groups[x]
			sys.exit ()
	except IOError:
		print "No cachefile"
		checkFlags(base_env)


Export('base_env')

SConscript('src/xmms/SConscript',build_dir='builddir/xmms',duplicate=0)
SConscript('src/clients/SConscript',build_dir='builddir/clients',duplicate=0)
SConscript('src/plugins/SConscript',build_dir='builddir/plugins', duplicate=0)

