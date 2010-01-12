#!/usr/bin/python

import sys
import os
import re # perl-style regular expressions

val = 'xy ac abc abbc abbbc abbbbc abbbbbc zw';

matches = re.match('^.*?(ab*c).*?$', val);
if matches:
    print "Match found in ", val
    print "All groups are ", matches.groups()
    print "Match is ", matches.group(0)
    print "Group is", matches.group(1) # first match
    
    #print "Before it is: ", val
    #val = re.compile('o([^ ]*?)r').sub("OTHER", val)
    #print "After sub is: ", val
    

print "\nString is ", val
print '0,  ', re.compile('ab*c').findall(val)       # any number of b's
print '1, 2', re.compile('ab{1,2}c').findall(val)   # number of b's within range
print '0, 3', re.compile('ab{0,3}c').findall(val)   # number of b's within range
print '2, 4', re.compile('ab{2,4}c').findall(val)   # number of b's within range
