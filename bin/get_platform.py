#!/usr/bin/python2.6

from __future__ import with_statement, print_function

import errno
import inspect
import os
import os.path as P
import platform
import subprocess
import sys
import urllib2
import logging

from collections import namedtuple
from functools import wraps, partial
from glob import glob
from hashlib import sha1
from shutil import rmtree
from urlparse import urlparse

def get_platform(pkg=None):
    system  = platform.system()
    machine = platform.machine()
    p = namedtuple('Platform', 'os bits osbits system machine prettyos dist_name dist_version')

    if system == 'Linux':
        dist = platform.linux_distribution(full_distribution_name=0)
        name = dist[0]
        ver  = dist[1]
    elif system == 'Darwin':
        name = 'Darwin'
        ver  = platform.mac_ver()[0]

    if system == 'Linux' and machine == 'x86_64':
        return p('linux', 64, 'linux64', system, machine, 'Linux', name, ver)
    elif system == 'Linux' and machine == 'i686':
        return p('linux', 32, 'linux32', system, machine, 'Linux', name, ver)
    elif system == 'Darwin' and machine == 'i386':
        # Force 64 bit no matter what
        return p('osx', 64, 'osx64', system, 'x86_64', 'OSX', name, ver)
    elif system == 'Darwin' and machine == 'x86_64':
        return p('osx', 64, 'osx64', system, machine, 'OSX', name, ver)
    else:
        message = 'Cannot match system to known platform'
        if pkg is None:
            raise Exception(message)
        else:
            raise PackageError(pkg, message)


if __name__ == '__main__':
  arch = get_platform()
  val = '%s-%s-%s-%s%s' % ("", "", arch.machine, arch.dist_name, arch.dist_version)

print ("platform is " + val)




