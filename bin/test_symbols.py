#!/usr/bin/python

import sys, os, re
import lief


name = sys.argv[1]

print("--name is " + name)

# https://github.com/lief-project/LIEF/issues/118

library = lief.parse(name)

# for v in library.imported_functions:
#     print("imported " + v.name)

# for v in library.exported_functions:
#     print("exported " + v.name)

print("needed is ")
print(library[lief.ELF.DYNAMIC_TAGS.NEEDED].value)

for entry in library.dynamic_entries:
    print("name is ", entry.name)
