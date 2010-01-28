#!/usr/bin/python

import sys
import os
import re # Perl-style regular expressions
import glob
import subprocess

files = glob.glob('*') + glob.glob('.*')

for file in files:
    print "file is", file

print "Recursive now"
process = subprocess.Popen(['find', '.'], shell=False, stdout=subprocess.PIPE)
vals = (process.communicate()[0]).split("\n")

for val in vals:
    print "val is --" + val + '--'

