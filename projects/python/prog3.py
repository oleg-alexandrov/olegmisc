#!/usr/bin/python

import sys
import os
import re # Perl-style regular expressions

class employee:

    def __init__(self, name, id):
        self.name = name
        self.id   = id
        self.type = "employee"
        
    def printData(self):
        print self.type, "name:", self.name + ", id:", self.id

class manager(employee):

    def __init__(self, name, id):
        employee.__init__(self, name, id)
        self.type = "manager"
    
if __name__ == "__main__":

    A = employee("Nick", 3422)
    A.printData()

    B = manager("Dora", 9201)
    B.printData()

people = []

fh = open("data2.txt", "r"); text = fh.read(); fh.close()
lines = text.split("\n")

data = {}

for line in lines:
    m = re.search("^(\w+)\s+(\d+)\s+(\w+)", line)
    if not m:
        continue
    
    name = m.group(1)
    id   = m.group(2)
    type = m.group(3)
    
    if type == "employee":
        A = employee(name, id)
    elif type == "manager":
        A = manager(name, id)
    else:
        print "Unknown employee type: ", type
        continue
    
    people.append(A)

    data[name] = id

print "\nAll the employees parsed so far:"
for person in people:
    person.printData()

A = 'AapaapaEaA'

B = re.sub("[aA]", "x", A)
print "Before:", A
print "After:", B

keys = data.keys()
print "Keys are", keys

for name in keys:
    print "Data:", name, data[name]
