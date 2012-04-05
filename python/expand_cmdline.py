#!/usr/bin/python

import sys
import os
import string
import re # perl-style regular expressions

# Edit the content of the command line as passed from zsh

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

def expandCmdLine(cmdLine, cursor):

   # Intelligently replace the current string with another string.
   # We take into account where the cursor is positioned in the string.
   
   lineLen = len(cmdLine)
   if cursor < 0:
      cursor = 0
   elif (cursor > lineLen):
      cursor = lineLen
  
   before = cmdLine[0:cursor]
   after  = cmdLine[cursor:lineLen]

   #marker = ' xtk45TSfl ';
   #words = re.sub(r'\b', marker, before).split(marker);
   words    = re.split(" ", before)
   #print words
   
   numWords = len(words)

   if    ( numWords >= 3 and words[numWords-3] == "r"):
      # Given the input 'hi there r hi ho', will replace it with 'ho there'  
      fr = words[numWords-2]
      to = words[numWords-1]
      for i in range(len(words)):
         words[i] = re.sub(fr, to, words[i])
         #print words[i]
      words[numWords-3] = ""
      words[numWords-2] = ""
      words[numWords-1] = ""
      cmdLine = " ".join(words) + after
      cmdLine = re.sub('\s*$', '', cmdLine)
      cursor  = len(cmdLine)
      
   elif ( numWords == 1 and words[0] == "pl" ) or \
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
      
   return str(cursor) + sep + cmdLine
   
# Main program

numArgs = len(sys.argv)
if numArgs <= 2:
   print "Error: Need to have the existing command line and the cursor postion as inputs"
   sys.exit(0)

cmdLine = sys.argv[1]
cursor  = int(sys.argv[2])
sep     = '__sep__'

print expandCmdLine(cmdLine, cursor)

