#!/usr/bin/env python

import sys
import os
import string
import re # perl-style regular expressions

# Edit the content of the command line as passed from zsh

def expandAlias(aliasName):
   
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

# end expandAlias()

def expandCmdLine(cmdLine, cursor, part):

   # Intelligently replace the current string with another string.
   # We take into account where the cursor is positioned in the string.

   lineLen = len(cmdLine)
   if cursor < 0:
      cursor = 0
   elif (cursor > lineLen):
      cursor = lineLen

   before = cmdLine[0:cursor]
   after  = cmdLine[cursor:lineLen]

#    before = re.sub('\"\"', '', before)
#    after = re.sub('\"\"', '', after)
#    before = re.sub('\'\'', '', before)
#    after = re.sub('\'\'', '', after)

   #marker = ' xtk45TSfl ';
   #words = re.sub(r'\b', marker, before).split(marker);
   words    = re.split(" ", before)

   numWords = len(words)

   for k in range(numWords):
      if ( words[k] == "foo" ):
         # Replace foo with someOtherFooBar
         words[k] = "someOtherFooBar"
      cmdLine = " ".join(words) + after
      cursor  = len(cmdLine)

   lWord = words[numWords-1]
   
   if    ( numWords >= 2 and lWord[-2:] == "ld" and
           (len(lWord) == 2 or not re.match('\w', lWord[-3]))):
      words[numWords-1] = lWord[:-2] + "LD_LIBRARY_PATH"
      cmdLine = " ".join(words) + after
      cursor  = len(cmdLine)

   if ( numWords >= 3 and lWord[-3:] == " ba"):
      words[numWords-1] = lWord[:-3] + "--bundle-adjust-prefix"
      cmdLine = " ".join(words) + after
      cursor  = len(cmdLine)

   if    ( numWords >= 2 and words[0] == "hp"):
      # If the first word is hp, expand the abbrevation
      # of the second word
      expansion = expandAlias(words[1])
      matches = re.match('^.*?\'(.*?)\'', expansion)
      if matches:
         words[1] = matches.group(1)
         cmdLine = " ".join(words) + after
         cursor  = len(cmdLine)

   if  ( numWords >= 2 and words[numWords-1] == "p"):
      words[numWords-1] = words[numWords-2] # replace with the preceding word
      cmdLine = " ".join(words) + after
      cursor  = len(cmdLine)

   elif  ( numWords >= 1 and words[numWords-1] == "21"):
      words[numWords-1] = "2>&1&" # expand '21' into '2>&1&'
      cmdLine = " ".join(words) + after
      cursor  = len(cmdLine)

   elif  ( numWords >= 1 and words[numWords-1][:1] == "o") and \
            len(words[numWords-1]) <= 2:
      # Replace o4 with > output4.txt 2>&1&
      suff = words[numWords-1][1:]
      words[numWords-1] = "> output" + suff + ".txt 2>&1&"
      cmdLine = " ".join(words) + after
      cursor  = len(cmdLine)

   elif  ( numWords >= 1 and words[numWords-1] == "ss"):
      words[numWords-1] = "*/*h */*cc" # expand 'ss' into '*/*h */*cc'
      cmdLine = " ".join(words) + after
      cursor  = len(cmdLine)

   elif  ( numWords >= 1 and words[numWords-1] == "rs"):
      words[numWords-1] = "rsync -avz" # expand 'rs' into 'rsync -avz'
      cmdLine = " ".join(words) + after
      cursor  = len(cmdLine)

   elif  ( numWords >= 3 and words[numWords-3] == "r"):
      # Given the input 'hi there r hi ho', will replace it with 'ho there'
      fr = words[numWords-2]
      to = words[numWords-1]
      for i in range(len(words)):
         words[i] = re.sub(fr, to, words[i])
         #print(words[i])
      words[numWords-3] = ""
      words[numWords-2] = ""
      words[numWords-1] = ""
      cmdLine = " ".join(words) + after
      cmdLine = re.sub('\s*$', '', cmdLine)
      cursor  = len(cmdLine)

   elif  ( numWords >= 3 and words[numWords-3] == "d"):
      # Given the input 'hi 2 d 2 i', will replace it with 'hi ${i}'.
      # Need this because typing dollars and curly braces is so annoying.
      fr = words[numWords-2]
      to = words[numWords-1]
      for i in range(len(words)):
         words[i] = re.sub(fr, '${' + to + '}', words[i])
         #print(words[i])
      words[numWords-3] = ""
      words[numWords-2] = ""
      words[numWords-1] = ""
      cmdLine = " ".join(words) + after
      cmdLine = re.sub('\s*$', '', cmdLine)
      cursor  = len(cmdLine)

   elif  ( numWords >= 2 and words[numWords-1] == "s"):
      # Remove extra spaces
      words[numWords-1] = ""
      cmdLine = " ".join(words)
      cmdLine = re.sub('\s+', ' ', cmdLine)
      cmdLine = re.sub('\s*$', '', cmdLine)
      cursor  = len(cmdLine)

   elif ( numWords == 1 and words[0] == "pl" ) or \
          ( numWords >= 2 and words[numWords-2] != "a" and words[numWords-1] == "pl" ):
      # Expand the string "pl"
      words[numWords-1] = "perl -pi -e \"s###g\""
      cmdLine = " ".join(words) + after
      cursor  = cursor + 13 # advance to between the first two ##

   elif ( numWords == 1 and words[0] == "plo" ) or \
          ( numWords >= 2 and words[numWords-2] != "a" and words[numWords-1] == "pl" ):
      words[numWords-1] = "perl -p -e \"s###g\""
      cmdLine = " ".join(words) + after
      cursor  = cursor + 11 # advance to between the first two ##

   elif ( numWords == 1 and words[0] == "pli" ) or \
          ( numWords >= 2 and words[numWords-2] != "a" and words[numWords-1] == "pli" ):
      words[numWords-1] = "perl -pi -e \"s###g\""
      cmdLine = " ".join(words) + after
      cursor  = cursor + 12 # advance to between the first two ##

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
      cmdLine   = expandAlias(aliasName)

   elif ( numWords >= 1 and lWord >= "1" and lWord <= "9" and len(lWord) == 1):
      # Read the file, paste the last item on specified line from the end.
      # This is useful to do paste without using the mouse.
      f = open('/tmp/output_llt.txt', 'r')
      a=f.readlines()
      pos = int(words[numWords-1])
      words[numWords-1] = a[-pos].split(" ")[-1]
      cmdLine = " ".join(words) + after
      cursor  = len(cmdLine)

   if part == 1:
      return str(cursor) # return cursor position
   else:
      return cmdLine    # return the processed command line
   
# Main program

numArgs = len(sys.argv)
if numArgs <= 3:
   print("Error: Need to have as inputs the existing " +  \
         "command line, the cursor postion, and a flag.")
   sys.exit(0)

cmdLine = sys.argv[1]
cursor  = int(sys.argv[2])
part    = int(sys.argv[3])

print(expandCmdLine(cmdLine, cursor, part))
