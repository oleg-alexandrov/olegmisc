#!/usr/bin/python

import sys
import os
import re # Perl-style regular expressions

def get_namespace(text):

  # identify the namespace in the h class
  # Look at the last of all namespaces (this is a bit hackish)

  namespace = ""

  m = re.match(r"^.*\n\s*(?:class|struct|namespace)\s+(\w+)\s*:*.*?\{",
              text, re.S)
  if m:
      namespace = m.group(1)
  else:
    print "Can't identify the namespace!"
    sys.exit(1);
  
  return namespace;

def balanced_parens(text):

    text = re.sub("//.*?($|\n)", "", text)

    lp = re.findall(r"\(", text)
    rp = re.findall(r"\)", text)

    return len(lp) == len(rp)

def extract_blocks(text):

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

    cpp_map = {}

    blocks = extract_blocks(text)

    for block in blocks:

        p = re.match("^([^\n]*?)" + namespace + "::(\w+)(\s*\(.*?\))",
                     block, re.S)
        if p:

            block_sans_namespace = p.group(1) + p.group(2) + p.group(3)
            fun_name             = p.group(2)
            cpp_map[fun_name]    = block_sans_namespace
            
    
    return cpp_map
    
if __name__ == '__main__':

    if len(sys.argv) < 3:
        print "Usage:", sys.argv[0], "file.cpp file.h"
        sys.exit(1)

    cpp_file = sys.argv[1]
    h_file   = sys.argv[2]

    hh = open(cpp_file, 'r'); cpp_text = hh.read(); hh.close()
    hh = open(h_file,   'r'); h_text   = hh.read(); hh.close()

    namespace = get_namespace(h_text)

    cpp_map = parse_cpp(cpp_text, namespace)
    
    
