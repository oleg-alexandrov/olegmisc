#!/usr/bin/python

import sys
import os
import re # Perl-style regular expressions

# If a function declaration changed in a cpp file, this script updates
# the corresponding declaration in the corresponding h file.
    
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

def len_woc(text):
  # Length after stripping the comments
  text = re.sub("//.*?($|\n)", "", text)
  return len(text)

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
    
    blocks = extract_blocks(cpp_text)
    count  = 1 # Used to keep the order of blocks
    
    for block in blocks:

        p = re.match("^(\w[^\n]*?\s)" # everything up to the namespace
                     + namespace
                     + "::(\w+)(\s*\(.*?\)).*?\{", # fun name, and up to {
                     block, re.S)

        if not p: continue
        
        fun_name = p.group(2)
        fun_decl = p.group(1) + p.group(2) + p.group(3)

        if not cpp_map.has_key(fun_name): cpp_map[fun_name] = {}
          
        cpp_map[fun_name][fun_decl] = count
        count = count + 1
        
    return cpp_map

def parse_update_h(h_text, cpp_map, namespace):

    # Update the h file by overwriting each block with the
    # corresponding block from the cpp file.

    h_blocks = extract_blocks(h_text)
    h_map    = {}
    count    = -1

    for h_block in h_blocks:

        count = count + 1
        
        p = re.match("""
        ^(\s*(?:static|virtual)?\s*)  # leading spaces, static, virtual
        (\w[^\n]*?\s)                 # type 
        (\w+)                         # function name 
        (\s*\(.*?\))                  # list of arguments
        (.*)$                         # newline, const, etc. 
        """, h_block, re.S | re.X)

        if not p: continue

        fun_name         = p.group(3)
        h_map[fun_name]  = h_block

        if not cpp_map.has_key(fun_name): continue
        if re.search("=", h_block):       continue # skip functions with default args

        # Find the function in the cpp file with the same name and
        # with the closest length (in terms of number of characters)
        max_error_sig       = 1e+100
        best_cpp_block      = ""
        best_cpp_with_extra = ""
        for cpp_block in cpp_map[fun_name]:

          # Skip any cpp function used earlier
          if cpp_map[fun_name][cpp_block] < 0: continue

          # Add the extra info present in the h file and not the cpp
          # file, like "virtual", "static", etc.
          cpp_with_extra = p.group(1) + cpp_block + p.group(5) 

          error_sig = abs(len_woc(h_block) - len_woc(cpp_with_extra))
          
          if error_sig < max_error_sig:
            max_error_sig       = error_sig
            best_cpp_block      = cpp_block
            best_cpp_with_extra = cpp_with_extra
            
        if best_cpp_with_extra == "": continue # could not find a match
        
        # We found the cpp function with the closest signature
        h_blocks[count] = best_cpp_with_extra
        h_blocks[count] = indent_block(h_blocks[count])
        cpp_map[fun_name][best_cpp_block] = -1 # mark that we used this one 
          
        #print "\n-------\nOverwriting\n\"" + h_block + "\"\nwith\n"
        #print "\"" + h_blocks[count] + "\n\"\n"
    
    h_text = "".join(h_blocks)

    # Add a newline at the end if missing
    if not re.search("\n\s*$", h_text): h_text = h_text + "\n"

    # See what new functions were declared in the cpp file and are
    # missing in the h file. Keep them in the same order as in the cpp file.
    new_blocks = {}
    for key in cpp_map:
        
        if h_map.has_key(key): continue
        for cpp_block in cpp_map[key]:
          new_blocks[cpp_map[key][cpp_block]] = cpp_block # Needed for sorting

    # Append the new blocks after the last public:/private:/namespace/class tag
    p = re.match("""
    ^(
    .*\n\s*
    (?:public:|private:|class\s*\w+\s*\{|namespace\s*\w+\s*\{)
    .*?[\n]
    )           # end of group 1
    ([ \t]*)    # indentation level (group 2)
    (.*)$       # group 3, whatever is left
    """, h_text, re.S | re.X)

    if not p:
        print "Could not find a place to append the new blocks to"
        sys.exit(1)

    new_chunk    = ""
    indent_level = p.group(2)

    sorted_keys = new_blocks.keys()
    sorted_keys.sort()
    for num in sorted_keys:
      new_chunk = new_chunk + indent_level + indent_block(new_blocks[num]) + ";\n"
      
    if new_chunk == "":
        return h_text # Nothing else to do

    h_text = p.group(1) + new_chunk + p.group(2) + p.group(3)

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

    namespace = get_namespace(h_text)
    cpp_map = parse_cpp(cpp_text, namespace)
    
    h_text  = parse_update_h(h_text, cpp_map, namespace)

    fh = open(h_file, 'w'); fh.write(h_text); fh.close()

    h_file = re.sub(r"^.*/", "", h_file)
    print "Updated ", h_file