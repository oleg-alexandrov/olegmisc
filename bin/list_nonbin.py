#!/usr/bin/python

import sys
import os
import re # Perl-style regular expressions
import errno
import inspect
import os
import os.path as P
import platform
import subprocess
import sys
import urllib2
import logging

def run(*args, **kw):
    need_output      = kw.pop('output', False)
    raise_on_failure = kw.pop('raise_on_failure', True)
    want_stderr      = kw.pop('want_stderr', False)
    kw['stdout']     = kw.get('stdout', subprocess.PIPE)
    kw['stderr']     = kw.get('stderr', subprocess.PIPE)

    p = subprocess.Popen(args, **kw)
    out, err = p.communicate()
    msg = None
    if p.returncode != 0:
        msg = '%s: command returned %d (%s)' % (args, p.returncode, err)
    elif need_output and len(out) == 0:
        msg = '%s: failed (no output). (%s)' % (args,err)
    if msg is not None:
        if raise_on_failure: raise Exception(msg)
        logger.warn(msg)
        return False, msg
    if want_stderr:
        return out, err
    return out

def is_binary(filename):
    ret = run('file', filename, output=True)
    return (ret.find('ELF') != -1) or (ret.find('Mach-O') != -1)

if __name__ == '__main__':

    
    f = open("files.txt", 'r')
    for line in f:
        line = line.rstrip()

        if not os.path.isfile(line): continue
        if os.path.isdir(line): continue
        if is_binary(line): continue
        
        print(line)
        
