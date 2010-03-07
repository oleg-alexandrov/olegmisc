#!/usr/bin/python

# Calling a C module from Python.

# The C module must be built before being used, see the build script
# myfuns_build.py for instructions.

import sys
import myfuns # The C module is implemented here

if len(sys.argv) > 1:
    status = myfuns.myfun1(sys.argv[1], 6)
else:
    print "Usage:", sys.argv[0], "command_name"
