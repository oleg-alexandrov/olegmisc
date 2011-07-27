#!/usr/bin/python

import sys
import os
import re # Perl-style regular expressions

# If a function declaration changed in a cpp file, this script updates
# the corresponding declaration in the corresponding h file.
    
def get_namespace(text):

  # Identify the namespace/class name in the given text. Look at the
  # last of all namespaces if there's more than one.

  namespace = ""

  m = re.match(r"^\s*(?:class|struct|namespace)\s+(\w+)\s*:*.*?\{",
              text, re.S)
  if m:
      namespace = m.group(1)
      return namespace
  else:
    return ""
  
def balanced_parens(text, lparen, rparen):

    text = re.sub("//.*?($|\n)", "", text)

    lp = re.findall(lparen, text)
    rp = re.findall(rparen, text)

    return len(lp) == len(rp)

def len_woc(text):
  # Length after stripping the comments
  text = re.sub("//.*?($|\n)", "", text)
  return len(text)

def extract_blocks(text, lparen, rparen):

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

        if balanced_parens(block, lparen, rparen):
            blocks.append(block)
            block = ""

    # last block
    if block != "":
        blocks.append(block)

    return blocks

def indent_block(text):

  # Indent a block of text by the opening paranthesis
  
  lines        = text.split("\n")
  indent_level = 0

  for count in range(len(lines)):

    if indent_level > 0:
      lines[count] = re.sub("^[ \t]*", "", lines[count])
      lines[count] = " " * indent_level + lines[count]

    p = re.match("^(.*?\()", lines[count])
    if indent_level == 0 and p:
      indent_level = len(p.group(1))
      
  text = "\n".join(lines)

  # Strip whitespaces after the last newline (those whitespaces are an artifact
  # which cause problems later)
  text = re.sub("[ ]*$", "", text)

  return text

def parse_cpp(cpp_text, namespace):

    # Identify the function declarations. Map each function name
    # to the set of all functions with that name, e.g.,
    # myfun --> { void namesp::myfun(double x), double namesp::myfun(int s) }
    # Strip the namespace 

    cpp_map = {} # Needed if there are multiple functions with same name
    
    blocks = extract_blocks(cpp_text, '\(', '\)')
    count  = 1 # Used to keep the order of blocks. Must be > 0.
    
    for block in blocks:

        p = re.match("^(\w[^\n]*?\s)" # everything up to the namespace
                     + namespace
                     + "::(\w+)(\s*\(.*\)).*?\{", # fun name, and up to {
                     block, re.S)

        if not p: continue
        
        fun_name = p.group(2)
        fun_decl = p.group(1) + p.group(2) + p.group(3)

        if not cpp_map.has_key(fun_name): cpp_map[fun_name] = {}
          
        cpp_map[fun_name][fun_decl] = count
        count = count + 1

        #print "In parse_cpp:", fun_name, fun_decl, count
        
    return cpp_map

def parse_update_h(h_text, cpp_map, namespace):

    # Update the h file by overwriting each block with the
    # corresponding block from the cpp file.

    # This routine needs serious cleanup, and the ability to handle
    # multiple functions in the h file with the same name
    # (may need to change the output format of extract_blocks()).
    
    h_blocks_arr = extract_blocks(h_text, '\(', '\)')

    # Copy h_blocks_arr into a map structure as besides elements
    # i and i +1 we'll also want to have elements in between
    h_blocks = {}
    count    = -1
    for h_block in h_blocks_arr:
      count = count + 1
      h_blocks[count] = h_blocks_arr[count]
      
    hsorted_keys = h_blocks.keys()
    hsorted_keys.sort()
    
    h_map    = {}
    done_cpp = {}
    
    for count in hsorted_keys:

        h_block = h_blocks[count]
        
        p = re.match("""
        ^(\s*(?:static|virtual)?\s*)  # leading spaces, static, virtual
        (\w[^\n]*?\s)                 # type 
        (\w+)                         # function name 
        (\s*\(.*\))                   # list of arguments
        (.*)$                         # newline, const, etc. 
        """, h_block, re.S | re.X)

        if not p: continue

        fun_name         = p.group(3)
        h_map[fun_name]  = h_block # Will have problems with multiple funs with same name

        if not cpp_map.has_key(fun_name): continue
        if re.search("=", h_block):       continue # skip functions with default args

        # Find the function in the cpp file with the same name and
        # with the closest length (in terms of number of characters)
        min_error_sig       = 1e+100
        best_cpp_block      = ""
        best_cpp_with_extra = ""
        for cpp_block in cpp_map[fun_name]:

          # Skip any cpp function used earlier
          if done_cpp.has_key(cpp_block): continue

          # Add the extra info present in the h file and not the cpp
          # file, like "virtual", "static", etc.
          cpp_with_extra = p.group(1) + cpp_block + p.group(5) 

          error_sig = abs(len_woc(h_block) - len_woc(cpp_with_extra))
          
          if error_sig < min_error_sig:
            min_error_sig       = error_sig
            best_cpp_block      = cpp_block
            best_cpp_with_extra = cpp_with_extra
            
        if best_cpp_with_extra == "": continue # could not find a match
        
        # We found the cpp function with the closest signature
        h_blocks[count] = best_cpp_with_extra
        h_blocks[count] = indent_block(h_blocks[count])
        
        # mark that we used this one
        done_cpp[best_cpp_block] = 1
          
        #print "\n-------\nOverwriting\n\"" + h_block + "\"\nwith\n"
        #print "\"" + h_blocks[count] + "\n\"\n"

    # For a given new function in the cpp file find the closest
    # function in the cpp file before it which is present in the h
    # file. Use that information to decide where to insert the new
    # function in the h file.
    closestIndexBefore = -1
    closestFunBefore   = ""

    for key in cpp_map:
      
      if h_map.has_key(key): continue
      
      for cpp_block in cpp_map[key]:

        if h_map.has_key(key): continue
        index = cpp_map[key][cpp_block]
        #print "found new: ", key, cpp_block, index
        for key2 in cpp_map:
          for cpp_block2 in cpp_map[key2]:
            if cpp_map[key2][cpp_block2] < index and \
               cpp_map[key2][cpp_block2] > closestIndexBefore and \
               h_map.has_key(key2):
              closestIndexBefore = cpp_map[key2][cpp_block2]
              closestFunBefore   = key2 

        #print "closest index is ", closestFunBefore, "--", closestIndexBefore
        
        h_map[key] = cpp_block
        
        if closestIndexBefore >= 0:
          
          for count in h_blocks.keys():
            if h_blocks[count] == h_map[closestFunBefore]:
              closestIndexBefore = count
              #print "Corresp index in the h file is: ", closestIndexBefore
              
          indexToAdd = closestIndexBefore + 0.1
          
        else:
          # The case when the new function is the first in the file
          indexToAdd = 0.1

        h_blocks[indexToAdd] = "  " + cpp_block + ";\n"
        #print "New index is ", indexToAdd, h_blocks[indexToAdd]
         
      # End adding new blocks to the h file map
      
    # Regenerate the header file after including information from the cpp file
    h_text = ""
    hsorted_keys = h_blocks.keys()
    hsorted_keys.sort()
    for count in hsorted_keys:
      #print "count is ", count
      #print "fun is ", h_blocks[count]
      h_text += h_blocks[count]
    if not re.search("\n\s*$", h_text): h_text = h_text + "\n" # newline at the end

    # If the above operation does not succeed, insert the new functions
    # at the beginning of the namespace or in private sections of the class.
    new_blocks = {}
    for fun_name in cpp_map:
      
      if h_map.has_key(key): continue
      
      for cpp_block in cpp_map[key]:
        new_blocks[cpp_map[key][cpp_block]] = cpp_block # Needed for sorting
        
    # See first if we can match the namespace
    p = re.match("""
    ^(
    \s*
    (?:|namespace\s*\w+\s*\{)
    .*?[\n]
    )           # end of group 1
    ([ \t]*)    # indentation level (group 2)
    (.*)$       # group 3, whatever is left
    """, h_text, re.S | re.X)
    
    if p:
      groups = [p.group(1), p.group(2), p.group(3)]
    else:
     
      # See if we can match a class/struct
      p = re.match("""
      ^(
      .*\n\s*
      (?:public:|private:|(?:class|struct)\s*\w+\s*\{)
      .*?[\n]
      )           # end of group 1
      ([ \t]*)    # indentation level (group 2)
      (.*)$       # group 3, whatever is left
      """, h_text, re.S | re.X)
      
      if p:
        groups = [p.group(1), p.group(2), p.group(3)]
        
      else:
        print "Could not find a place to append the new blocks to"
        sys.exit(1)
      
    new_chunk    = ""
    indent_level = groups[1]
    
    sorted_keys = new_blocks.keys()
    sorted_keys.sort()
    for num in sorted_keys:
      new_chunk = new_chunk + indent_level + indent_block(new_blocks[num]) + ";\n"
      
    if new_chunk == "":
      return h_text # Nothing else to do
   
    h_text = groups[0] + new_chunk + groups[1] + groups[2]
   
    return h_text
 
if __name__ == '__main__':

    if len(sys.argv) < 2:
        print "Usage:", sys.argv[0], "file.cpp"
        sys.exit(1)

    cpp_file = sys.argv[1]
    
    h_file = cpp_file;
    h_file = re.sub(r"\.cpp", ".h", h_file)

    if (not os.path.exists(h_file)) or (not os.path.isfile(h_file)):
      print "File", h_file, "does not exist"
      sys.exit(1)
    
    fh = open(cpp_file, 'r'); cpp_text = fh.read(); fh.close()
    fh = open(h_file,   'r'); h_text   = fh.read(); fh.close()

    # Need this to deal with pecularities in my cpp style.
    #cpp_text = re.sub("\n\s*\/\*\s+\*\/\s*\(", "(", cpp_text)
    #h_text   = re.sub("\n\s*\/\*\s+\*\/\s*\(", "(", h_text)
    
    # Identify all scopes (blocks between { and }). For each of those
    # scopes which is a namespace/class/struct, find all cpp files
    # with it and update that scope.
    
    scopes = extract_blocks(h_text, '\{', '\}')
    count    = -1
    for scope in scopes:

      count = count + 1
      namespace = get_namespace(scope)
      if namespace == "": continue
      #print "namespace is ", namespace
      
      cpp_map       = parse_cpp(cpp_text, namespace)
      scopes[count] = parse_update_h(scope, cpp_map, namespace)
      
    h_text = "".join(scopes)

    fh = open(h_file, 'w'); fh.write(h_text); fh.close()

    h_file = re.sub(r"^.*/", "", h_file)
    print "Updated ", h_file

      
