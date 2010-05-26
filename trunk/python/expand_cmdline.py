#!/usr/bin/python

import sys
import os
import string
import re # perl-style regular expressions

def expandAliases(aliasName):

   # Given the name of an alias, return the line showing its definition
   # in the alias file
   
   HOME     = os.getenv("HOME")
   fileName = HOME + '/.bash_aliases'

   FILE = open(fileName, "r")
   data = FILE.read()
   FILE.close()
   
   allAliases     = data.split("\n")
   aliasExpansion = "a " + aliasName
   
   for line in allAliases:

       matches = re.match('^(alias ' + aliasName + '=.*?)$', line)
       if matches:
           aliasExpansion = matches.group(1)
       # end if
    
   #end for
   
   aliasExpansion = re.sub('^alias', 'a', aliasExpansion)
   
   return aliasExpansion

# end expandAliases()

# Main program
numArgs = len(sys.argv)
if numArgs <= 2:
   print "Error: Need to have the existing command line and the cursor postion as inputs"
   sys.exit(0)

cmdLine = sys.argv[1]
cursor  = int(sys.argv[2])
sep     = '__sep__'

lineLen = len(cmdLine)
if cursor < 0:
   cursor = 0
elif (cursor > lineLen):
   cursor = lineLen
  
before = cmdLine[0:cursor]
after  = cmdLine[cursor:lineLen]

words    = re.split("\s+", before)
numWords = len(words)

if ( numWords == 1 and words[0] == "pl" ) or \
   ( numWords >= 2 and words[numWords-2] != "a" and words[numWords-1] == "pl" ):

   # Expand the string "pl"
   words[numWords-1] = "perl -pi -e \"s###g\""
   cmdLine = " ".join(words) + after
   cursor  = cursor + 13

elif ( numWords >= 2 and words[numWords-2] == "for" ):

   # Expand "for var" into a full for loop.
   var = words[numWords-1]
   words[numWords-2] = "for ((" + var + " = ; " + var + " < ; " + var + "++)); " + \
                       "do ; done" 
   words[numWords-1] = ""
   cmdLine = " ".join(words) + after
   cursor  = cursor + 5
   
elif (numWords == 2 and words[0] == "a"):

   # Expand the current alias
   aliasName = words[1]
   cmdLine   = expandAliases(aliasName)

print str(cursor) + sep + cmdLine
