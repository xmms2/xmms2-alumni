#!/usr/bin/env python

import sys
import re
import os

def count_and_strip(x, c):
    if not x: return 0, ''
    if x[0] == c:
        a,b = count_and_strip(x[1:], c)
        return a+1, b
    else:
        return 0, x

def all_same(x):
    if len(x) < 2:
        return True
    return (x[0] == x[1]) and all_same(x[1:])

def count(s, c):
    return len([x for x in s if x == c])

def check(filename):
    lines = file(filename).readlines()

    errors = 0
    balance = 0
    curr = []
    multilines = []
    for i,li in enumerate(lines):
        if '\r' in li:
            print "# Line %d in file %s has \\r" % (i+1, filename)
            errors += 1
        if li[-1] != '\n':
            print "# Line %d in file %s does not end with newline" % (i+1, filename)
            errors += 1
        else:
            li = li[:-1]
        if len(li) and li[-1] == ' ':
            print "# Line %d in file %s has extra whitespace at end:" % (i+1, filename)
            print "# %s<--" % li
            errors += 1

        balance += count(li, "(")
        balance -= count(li, ")")
        if balance > 0:
            curr.append(li)
        elif balance == 0:
            if curr:
                curr.append(li)
                multilines.append((i-len(curr),curr))
                curr = []
        else:
            print "# Negative balance in file %s line %d" % (filename, i)
            errors += 1
            return errors

    for lineno, ml in multilines:
        d = [count_and_strip(x, "\t") for x in ml]
        tabs = [x[0] for x in d if x]
        if not all_same(tabs):
            print "# not same indentation in %s:%d:" % (filename, lineno)
            print "# " + "\n# ".join(ml).replace("\t","|---->")
            print "#"
            errors += 1
    
    return errors

def find_sources(sources, dir, files):
    for file in files:
        path = os.path.join(dir, file)
        if os.path.isfile(path) and path.endswith('.c'):
            sources.append(path)

if __name__ == '__main__':
    sources = []
    os.path.walk('src/xmms', find_sources, sources)
    os.path.walk('src/plugins', find_sources, sources)

    print "1.." + str(len(sources))
    for i in range(0, len(sources)):
        if check(sources[i]):
            print "not",
        print "ok %d - %s" % (i+1, sources[i])
