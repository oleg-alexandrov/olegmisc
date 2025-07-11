#!/usr/bin/env python

# Find the text between #beg and #end. Break the lines by a newline
# and spaces before each dash option.
import sys, re

# Ensure there is at least one arg
if len(sys.argv) < 2:
    print("Usage: break_lines.py filename")
    sys.exit(1)
    
# Read the header file
filename = sys.argv[1]

# Read the file in a single string
with open(filename, 'r') as f:
    data = f.read()

# Match #beg and #end across multiple lines
m = re.search(r'^(.*?)(#beg\s*)(.*?)(\s*#end)(.*?)$', data, re.DOTALL)

if not m:
    print("No match for #beg and #end")
    sys.exit(1)

beg = m.group(1)
text = m.group(3)
end = m.group(5)

# In text, replace a space followed by dash with newline, space, and dash
text = re.sub(r' -', '\n  -', text)
# remove empty lines, so newline followed by spaces followed by newline
text = re.sub(r'\n\s*\n', '\n', text)

# Put a continuation character at the end of each line
text = re.sub(r'\n', ' \\\n', text)


# Replace \ followed by spaces followed by \ with a single \
text = re.sub(r'\\\s*\\\n', '\\\n', text)
print("text is\n", text)
