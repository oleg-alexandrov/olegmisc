#!/usr/bin/python

import sys
import os
import re # Perl-style regular expressions

# If a function declaration changed in the cpp file, update
# the corresponding declaration in the h file.
    
def get_namespace(text):

  # Identify the namespace in the header file. Look at the last of all
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
    # that the parentheses in each block are balanced.
    
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

def parse_cpp(cpp_text, namespace):

    # Identify the function declarations. Map each function name
    # to the full declaration, e.g.,
    # myfun --> void namesp::myfun(double x)
    # Strip the namespace 

    cpp_map     = {}
    cpp_map_dup = {} # Needed if there are multiple functions with same name
    
    blocks = extract_blocks(cpp_text)

    for block in blocks:

        p = re.match("^(\w[^\n]*?\s)" # everything up to the namespace
                     + namespace
                     + "::(\w+)(\s*\(.*?\)).*?\{", # fun name, and up to {
                     block, re.S)

        if not p: continue
        
        fun_name           = p.group(2)
        cpp_map[fun_name]  = p.group(1) + p.group(2) + p.group(3)

        if not cpp_map_dup.has_key(fun_name): cpp_map_dup[fun_name] = {}
          
        cpp_map_dup[fun_name][cpp_map[fun_name]] = 0

          
    return (cpp_map, cpp_map_dup)

def num_of_commas(text):
  text = re.sub("//.*?($|\n)", "", text)
  commas = re.findall(",", text)
  return len(commas)
  
def parse_update_h(h_text, cpp_map, cpp_map_dup, namespace):

    blocks = extract_blocks(h_text)
    h_map  = {}
    count  = -1

    # Overwrite with the corresponding block from the cpp file
    for block in blocks:

        count = count + 1
        
        p = re.match("""
        ^(\s*(?:static|virtual)?\s*)  # leading spaces, static, virtual
        (\w[^\n]*?\s)                 # type 
        (\w+)                         # function name 
        (\s*\(.*?\))                  # list of arguments
        (.*)$                         # newline, const, etc. 
        """, block, re.S | re.X)

        if not p: continue

        if re.match("=", block): continue # skip functions with default args
        
        fun_name         = p.group(3)
        h_map[fun_name]  = "".join(p.group())
        
        # Overwriting with appropriate cpp function
        #if cpp_map.has_key(fun_name):
        #  blocks[count] = p.group(1) + cpp_map[fun_name] + p.group(5)
          #print "\n-------\nOverwriting\n'" + block + "'\nwith\n"
          #print "'" + blocks[count] + "'\n"
          
        if not cpp_map_dup.has_key(fun_name): continue

        # Find the function in the cpp file with the same name and
        # with the closest signature. 
        max_error_sig  = 1e+100
        best_match     = ""
        for key in cpp_map_dup[fun_name]:

          # Skip any cpp function used earlier
          if cpp_map_dup[fun_name][key] != 0: continue
          
          error_sig = abs(num_of_commas(block) - num_of_commas(key))
          if error_sig < max_error_sig:
            max_error_sig  = error_sig
            best_match     = key
            
        if best_match == "": continue # could not find a match
        
        # We found the cpp function with the closest signature
        blocks[count] = p.group(1) + best_match + p.group(5)
        cpp_map_dup[fun_name][best_match] = 1 # mark that we used this one 
          
        print "\n-------\nOverwriting\n\"" + block + "\"\nwith\n"
        print "\"" + blocks[count] + "\"\n"
    
    h_text = "".join(blocks)

    # See what new functions were declared in the cpp file and are
    # missing in the h file
    new_chunk = ""
    for key in cpp_map:
        
        if h_map.has_key(key): continue
        new_chunk = new_chunk + cpp_map[key] + "\n"

    new_chunk = re.sub("\s*$", "", new_chunk)    
    
    if new_chunk == "":
        return h_text # Nothing else to do

    # Append the new chunk after the last public:/private:/namespace/class tag
    p = re.match("""
    ^(
    .*\n\s*
    (?:public:|private:|class\s*\w+\s*\{|namespace\s*\w+\s*\{)
    .*?[\n]
    )           # end of group 1
    ([ \t]*)    # indentation level (group 2)
    (.*?)$      # group 3, whatever is left
    """, h_text, re.S | re.X)

    if not p:
        print "Could not find a place to append the new blocks to"
        sys.exit(1)

    h_text = p.group(1) + p.group(2) + new_chunk + ";\n" \
             + p.group(2) + p.group(3)

    return h_text

if __name__ == '__main__':

    if len(sys.argv) < 3:
        print "Usage:", sys.argv[0], "file.cpp file.h"
        sys.exit(1)

    cpp_file = sys.argv[1]
    h_file   = sys.argv[2]

    fh = open(cpp_file, 'r'); cpp_text = fh.read(); fh.close()
    fh = open(h_file,   'r'); h_text   = fh.read(); fh.close()

    namespace = get_namespace(h_text)
    (cpp_map, cpp_map_dup) = parse_cpp(cpp_text, namespace)
    
    h_text  = parse_update_h(h_text, cpp_map, cpp_map_dup, namespace)

    fh = open(h_file, 'w'); fh.write(h_text); fh.close()
