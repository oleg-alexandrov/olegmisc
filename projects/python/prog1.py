#!/usr/bin/python

import sys
import os
import re # Perl-style regular expressions
import subprocess

def fun1(x):

    return x*x

class pair:
    
    num = 0
    str = ""

    def __init__(self, n=0, s=""):
        print "Calling initializer 2"
        self.num = n
        self.str = s
        
    def printData(self):
        print "(" + str(self.num) + ", \"" + self.str + "\")"
        
print "First program!"

x = 1.1;
y = fun1(x);
print "x is", x, "and y is", y

z = pair(4, "something")
z.printData()

w = pair()
w.printData()

x = "xXxaXx"

y = re.sub("x", "w", x, re.I)
print "x and y are", x, y

pattern = "x"
if re.search(pattern, x):
    print x, "has", pattern
else:
    print x, "does not have", pattern
    

x = "axXxwxvVvV323dkd"
pattern="([xX]+).*?(v+\d+)"
m = re.search(pattern, x, re.I)
if m:
    print x, "has", pattern
    print "it is", m.group(1), m.group(2)
    
else:
    print x, "does not have", pattern


pattern = "(x+|w+)"
vals = re.findall(pattern, x, re.I)
print "matches in", x, "are", vals

file = "inData.txt"
f = open(file, 'r')
data = f.read()
f.close()

print "data is ", data

data = re.sub("\n", "--xuxa\n", data)
print "data is ", data

file2 = "outData.txt"
print "Writing to ", file2
g = open(file2, "w")
g.write(data)
g.close()
    

cmd = "ls"
args = '-al'
output = subprocess.Popen([cmd, args], stdout=subprocess.PIPE).communicate()[0]
print "output of", cmd, args, "is", output

