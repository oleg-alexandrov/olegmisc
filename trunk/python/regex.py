#!/usr/bin/python

import re

# Controlling the number of matches
val = 'xy ac abc abbc abbbc abbbbc abbbbbc zw';
print "\nString is ", val
print '0,  ', re.findall('ab*c',     val)   # any number of b's
print '1, 2', re.findall('ab{1,2}c', val)   # number of b's within range
print '2,  ', re.findall('ab{2,}c', val)    # number of b's within range

# Raw strings
val = r'a \section';
print "Val is ", val
matches = re.match(r'^.*?(\\s).*?$', val); # raw match
if matches:
    print "Found match for ", val
    print "Match is ", matches.groups()
    print "Group is", matches.group(1) # first match
    

# Replacement
val  = 'xy ac abc abbc abbbc abbbbc abbbbbc zw';
print "Val before", val
val2 = re.sub('[axybc]+', 'w', val)
print "Val after ", val2

# Case insensitive modifier
val  = 'xy ac abc abbc aBCc abCbbc abbbbbc zw';
print "\nVal is ", val
out = re.findall('ab*', val);       print "Case sensitive outputs are   ", out
out = re.findall('ab*', val, re.I); print "Case insensitive outputs are ", out

# Grouping
p = re.compile('^.*?(ab)(ab)', re.I)
m = p.match('xAbabAbAbAb')
print "m=", m
print "groups are ", m.groups()
