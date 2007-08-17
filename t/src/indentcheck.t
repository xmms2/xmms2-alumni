#!/usr/bin/env python

import sys
import re
import glob
import os

files = """
	src/xmms/*.c
	src/xmms/*/*.c
	src/plugins/*/*.c
	src/clients/lib/xmmsclient/*.c
	src/clients/lib/xmmsclient-glib/*.c
	src/clients/lib/xmmsclient-cf/*.c
	src/clients/lib/xmmsclient-ecore/*.c
	src/lib/*/*.c
	src/clients/et/*.c
	src/clients/cli/*.c
	src/clients/cli/launcher/*.c
	src/clients/mdns/*/*.c
	src/client/medialib-updater/*.c
	""".split()
blacklist_patterns = ['src/plugins/equalizer/iir*.c', 'src/clients/mdns/avahi/find-avahi.c']
todo = ['src/plugins/flac/flac.c', 'src/plugins/ofa/ofa.c']


fun_no_space = re.compile('^.*?(\w+\()')
non_indent_tab = re.compile('^\t*[^\t]+\t')

def leading(x, c):
    """
    Count number of leading /c/ chars in /x/
    """
    a = 0
    while a < len(x) and x[a] == c:
        a += 1
    return a

def all_same(x):
    """
    Check if all elements in /x/ are equal
    """
    if len(x) < 2:
        return True
    for a in x[1:]:
        if a != x[0]:
            return False
    return True

def check(filename):
    data = file(filename).read()

    errors = 0
    balance = 0
    curr = []
    multilines = []

    if data[-1] != '\n':
        print "# File %s does not end with newline" % (filename)
        errors += 1

    NORM, COMM, STR = range(3)
    state = NORM
    pos = 0
    fdata = ""
    while pos < len(data):
        c = data[pos]
        if state == NORM:
            if c == '"':
                state = STR
            elif c == "'":
                if data[pos+1] == '\\':
                    pos += 1
                pos += 2
                fdata += "CHR"
            elif c == "/" and data[pos+1] == "*":
                state = COMM
            #elif c == '\\' and data[pos+1] == '\n':
            #    pos += 1
            else:
                fdata += c
        elif state == COMM:
            if c == '*' and data[pos+1] == '/':
                fdata += "/*COMM*/"
                pos += 1
                state = NORM
            elif c == '\n':
                fdata += '/*COMM*/\n'
        elif state == STR:
            if c == '"' and data[pos-1] != '\\':
                fdata += "STR"
                state = NORM
            elif c == '\n':
                print "# Runaway string in %s" % (filename)
                errors += 1
                state = NORM
        pos += 1

    for i,li in enumerate(fdata.split("\n")):
        if '\r' in li:
            print "# Line %d in file %s has \\r" % (i+1, filename)
            errors += 1

        if not li:
            continue

        if not li.replace(" ", "").replace("\t", ""):
            print "# line %d in file %s only contains whitespace" % (i+1, filename)
            errors += 1
            continue

        if li[-1] == ' ':
            print "# Line %d in file %s has extra whitespace at end:" % (i+1, filename)
            print "# %s<--" % li
            errors += 1

        if non_indent_tab.match(li):
            print "# Line %d in file %s has tab not used for indentation:" % (i+1, filename)
            print "# %s" % li.replace("\t","|---->")
            errors += 1

        if li[0] != '#':
            m = fun_no_space.match(li)
            if m:
                print "# Line %d in file %s has no space between function and left parenthesis:" % (i+1, filename)
                print "# %s<--" % m.group(1)
                errors += 1

        balance += li.count("(")
        balance -= li.count(")")
        if balance > 0:
            curr.append(li)
        elif balance == 0:
            if curr:
                curr.append(li)
                multilines.append((i+1-(len(curr) - 1),curr))
                curr = []
        else:
            print "# Negative balance in file %s line %d" % (filename, i)
            print "# '%s'" % li
            errors += 1
            return errors

    for lineno, ml in multilines:
        tabs = [leading(x, "\t") for x in ml]
        if not all_same(tabs):
            print "# not same indentation in file %s line %d:" % (filename, lineno)
            print "# " + "\n# ".join(ml).replace("\t","|---->")
            print "#"
            errors += 1

    return errors


if __name__ == '__main__':
    sources = []
    blacklist = []

    for p in blacklist_patterns:
        blacklist += glob.glob(p)

    for p in files:
        sources += [x for x in glob.glob(p) if x not in blacklist]

    print "1..%d" % len(sources)
    for i, src in enumerate(sources):
        if check(src):
            print "not",
        print "ok %d - %s" % (i+1, src),
        if src in todo:
            print " # TODO indentcheck.py bug",
        print
        sys.stdout.flush()
