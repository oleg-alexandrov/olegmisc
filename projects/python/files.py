#!/usr/bin/python

import sys
import os
import re # perl-style regular expressions

FILE = open(sys.argv[1], "r")
data = FILE.read()
FILE.close()

print "All data is \n", data
print "End of data"

vals = data.split("\n")

for val in vals:
    
    matches = re.match('^.*?(other.*?)$', val)

    if matches:
        
        print "Match found in ", val
        print "Match is ", matches.group(0)
        print "Group is", matches.group(1) # first match

        print "Before it is: ", val
        val = re.compile('o([^ ]*?)r').sub("OTHER", val)
        print "After sub is: ", val
        



