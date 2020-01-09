#!/usr/bin/python

import sys
import os
import re # Perl-style regular expressions

file_list = 'a.txt b.txt c.txt'

files = file_list.split(" ")

file_hash = {}

for file in files:
    print "file is ", file

    file_hash[file] = 1

for f in ['a.txt', 'd.txt']:
    
    if f in file_hash:
        print f, " is present in file hash"
    else:
        print f, " is not present in file hash"

    if f in files:
        print f, " is present in array"
    else:
        print f, " is not present in array"

for q in [1, 2, 3, 'a.txt', 'd.txt']:

    if file_hash.has_key(q):
        print "key ", q, " is present"
    else:
        print "key ", q, " is not present"

        
