#!/usr/bin/python

import sys
import os
import re # Perl-style regular expressions

# If a function declaration changed in the cpp file, update
# the corresponding declaration in the h file.
    
def get_namespace(text):

  # Identify the namespace in the header file Look at the last of all
  # namespaces if there's more than one.

  namespace = ""

  m = re.match(r"^.*\n\s*(?:class|struct|namespace)\s+(\w+)\s*:*.*?\{",
              text, re.S)
  if m:
      namespace = m.group(1)
  else:
    print "Can't identify the namespace!"
    sys.exit(1);
  
  return namespace

def balanced_parens(text):

    text = re.sub("//.*?($|\n)", "", text)

    lp = re.findall(r"\(", text)
    rp = re.findall(r"\)", text)

    return len(lp) == len(rp)

def extract_blocks(text):

    # A block consists of several consecutive lines such
    # that the parentheses in the block are balanced.
    
    lines = text.split("\n")

    blocks = []
    block  = ""
    lcount = 0
    
    for line in lines:
        
        lcount = lcount + 1
        block  = block + line
        
        if lcount < len(lines):
            block = block + "\n"

        if balanced_parens(block):
            blocks.append(block)
            block = ""

    # last block
    if block != "":
        blocks.append(block)

    return blocks

def parse_cpp(text, namespace):

    # Identify the function declarations. Map each function name
    # to the full declaration, e.g.,
    # myfun --> void namesp::myfun(double x)
    # Strip the namespace 

    cpp_map = {}

    blocks = extract_blocks(text)

    for block in blocks:

        p = re.match("^(\w[^\n]*?\s)" + namespace + "::(\w+)(\s*\(.*?\)).*?\{",
                     block, re.S)

        if not p: continue
        
        fun_name           = p.group(2)
        cpp_map[fun_name]  = p.group(1) + p.group(2) + p.group(3)
        #print "--", block
            
    
    return cpp_map

def parse_update_h(text, cpp_map, namespace):

    blocks = extract_blocks(text)
    h_map  = {}
    count  = -1
    
    for block in blocks:

        count = count + 1
        
        p = re.match("""
        ^(\s*(?:static|virtual)?\s*)  # leading spaces, static, virtual
        (\w[^\n]*?\s)                 # Type 
        (\w+)                         # function name 
        (\s*\(.*?\))                  # list of arguments
        (.*)$                         # newline, const, etc. 
        """, block, re.S | re.X)

        if not p: continue

        fun_name         = p.group(3)
        h_map[fun_name]  = "".join(p.group())
        

        if cpp_map.has_key(fun_name):
            print "\n-------\nOverwriting\n'" + block + "'\nwith\n"
            block = p.group(1) + cpp_map[fun_name] + p.group(5)
            print "'" + block + "'\n"
        
        blocks[count] = block
        #print "--", block,


    text = "".join(blocks)
    #print text
    
    return text

if __name__ == '__main__':

    if len(sys.argv) < 3:
        print "Usage:", sys.argv[0], "file.cpp file.h"
        sys.exit(1)

    cpp_file = sys.argv[1]
    h_file   = sys.argv[2]

    hh = open(cpp_file, 'r'); cpp_text = hh.read(); hh.close()
    hh = open(h_file,   'r'); h_text   = hh.read(); hh.close()

    namespace = get_namespace(h_text)
    cpp_map   = parse_cpp(cpp_text, namespace)
    h_text    = parse_update_h(h_text, cpp_map, namespace)
