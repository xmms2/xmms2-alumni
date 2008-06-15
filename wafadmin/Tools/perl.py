#! /usr/bin/env python
# encoding: utf-8
# andersg at 0x63.nu 2007

import os
import pproc as subprocess
import Action, Params
from Object import extension, taskgen, feature, before

xsubpp_str = '${PERL} ${XSUBPP} -noprototypes -typemap ${EXTUTILS_TYPEMAP} ${SRC} > ${TGT}'
EXT_XS = ['.xs']

@taskgen
@before('apply_incpaths')
@feature('perlext')
def init_pyext(self):
	self.uselib = self.to_list(self.uselib)
	if not 'PERL' in self.uselib: self.uselib.append('PERL')
	if not 'PERLEXT' in self.uselib: self.uselib.append('PERLEXT')
	self.env['shlib_PATTERN'] = self.env['perlext_PATTERN']


@extension(EXT_XS)
def xsubpp_file(self, node):
	gentask = self.create_task('xsubpp')
	gentask.set_inputs(node)
	outnode = node.change_ext('.c')
	gentask.set_outputs(outnode)

	self.allnodes.append(outnode)

Action.simple_action('xsubpp', xsubpp_str, color='BLUE', prio=10)

def check_perl_version(conf, minver=None):
	"""
	Checks if perl is installed.

	If installed the variable PERL will be set in environment.

	Perl binary can be overridden by --with-perl-binary config variable

	"""
	res = True

	if not getattr(Params.g_options, 'perlbinary', None):
		perl = conf.find_program("perl", var="PERL")
		if not perl:
			return False
	else:
		perl = Params.g_options.perlbinary
		conf.env['PERL'] = perl

	version = os.popen(perl + " -e'printf \"%vd\", $^V'").read()
	if not version:
		res = False
		version = "Unknown"
	elif not minver is None:
		ver = tuple(map(int, version.split(".")))
		if ver < minver:
			res = False

	if minver is None:
		cver = ""
	else:
		cver = ".".join(map(str,minver))
	conf.check_message("perl", cver, res, version)
	return res

def check_perl_module(conf, module):
	"""
	Check if specified perlmodule is installed.

	Minimum version can be specified by specifying it after modulename
	like this:

	conf.check_perl_module("Some::Module 2.92")
	"""
	cmd = [conf.env['PERL'], '-e', 'use %s' % module]
	r = subprocess.call(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE) == 0
	conf.check_message("perl module %s" % module, "", r)
	return r

def check_perl_ext_devel(conf):
	"""
	Check for configuration needed to build perl extensions.

	Sets different xxx_PERLEXT variables in the environment.

	Also sets the ARCHDIR_PERL variable useful as installation path,
	which can be overridden by --with-perl-archdir option.
	"""
	if not conf.env['PERL']:
		return False

	perl = conf.env['PERL']

	conf.env["LINKFLAGS_PERLEXT"] = os.popen(perl + " -MConfig -e'print $Config{lddlflags}'").read()
	conf.env["CPPPATH_PERLEXT"] = os.popen(perl + " -MConfig -e'print \"$Config{archlib}/CORE\"'").read()
	conf.env["CCFLAGS_PERLEXT"] = os.popen(perl + " -MConfig -e'print \"$Config{ccflags} $Config{cccdlflags}\"'").read()

	conf.env["XSUBPP"] = os.popen(perl + " -MConfig -e'print \"$Config{privlib}/ExtUtils/xsubpp$Config{exe_ext}\"'").read()
	conf.env["EXTUTILS_TYPEMAP"] = os.popen(perl + " -MConfig -e'print \"$Config{privlib}/ExtUtils/typemap\"'").read()

	if not getattr(Params.g_options, 'perlarchdir', None):
		conf.env["ARCHDIR_PERL"] = os.popen(perl + " -MConfig -e'print $Config{sitearch}'").read()
	else:
		conf.env["ARCHDIR_PERL"] = getattr(Params.g_options, 'perlarchdir')

	conf.env['perlext_PATTERN'] = '%s.' + os.popen(perl + " -MConfig -e'print $Config{dlext}'").read()

	return True

def detect(conf):
	conf.hook(check_perl_version)
	conf.hook(check_perl_ext_devel)
	conf.hook(check_perl_module)

def set_options(opt):
	opt.add_option("--with-perl-binary", type="string", dest="perlbinary", help = 'Specify alternate perl binary', default=None)
	opt.add_option("--with-perl-archdir", type="string", dest="perlarchdir", help = 'Specify directory where to install arch specific files', default=None)

