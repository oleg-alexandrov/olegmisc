#!/usr/bin/python

import sys
import os
import re # Perl-style regular expressions

a = [3, 4, 5]

print "a is", a

for s in a:
    s = s + 3


print "length is", len(a)
    
print "range of 10 is", range(10)

print "a is", a
for i in range(len(a)):
    a[i] += 2

print "a is", a

b = {}

b["l"] = "q"
b["c"] = "f"

keys = b.keys()
print "keys are", keys
keys.sort()
print "keys are", keys

for k in keys:
    print "pair:", k, "=>", b[k]
    
