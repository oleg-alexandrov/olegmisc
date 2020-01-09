#!/usr/bin/python

import sys
import os
import re # perl-style regular expressions

def expandAliases(aliasName):
    
   HOME     = os.getenv("HOME")
   fileName = HOME + '/.bash_aliases'

   FILE = open(fileName, "r")
   data = FILE.read()
   FILE.close()
   
   lines = data.split("\n")
   
   out = ""
   
   for line in lines:

       matches = re.match('^(alias ' + aliasName + '=.*?)$', line); # raw match
       if matches:
           out = matches.group(1)
       # end if
    
       if out == "":
           out = aliasName
       #end if

   #end for
   
   out = re.sub('^alias', 'a', out);
   
   print out

# end expandAliases()

# Main program
numArgs   = len(sys.argv)
if numArgs >= 2 and sys.argv[1] == "a":
    aliasName = sys.argv[numArgs-1]
    expandAliases(aliasName)
# end if
