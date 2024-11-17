#!/usr/bin/env python

# Clean the style in C++ files. 

import os, re, sys, textwrap, glob, datetime

# Check that there exists one input argument
if len(sys.argv) < 2:
    print("Usage: clean_style.py file")
    sys.exit(1)

# Read the text from the file as one string
filename = sys.argv[1]
text = ''
with open(filename, 'r', encoding='utf-8') as f:
    text = f.read()

# No space or tab after open parentheses
text = re.sub(r'\([ \t]+', '(', text)
# No space or tab before close parentheses
text = re.sub(r'[ \t]+\)', ')', text)
# No space followed by newline
text = re.sub(r'[ \t]+\n', '\n', text)
# No space/tab before comma
text = re.sub(r'[ \t]+,', ',', text)
# Awlays a space between if/for/while and the following parenthesis
text = re.sub(r'(if|for|while)\s*\(', r'\1 (', text)
# A space between } and else
text = re.sub(r'}\s*else', '} else', text)
# Same for catch
text = re.sub(r'}\s*catch', '} catch', text)
# A space between else and {
text = re.sub(r'else\s*{', 'else {', text)
# No ) { on its own line
text = re.sub(r'\s*\)\s*{', ') {', text)

# Update the copyright year
year = str(datetime.datetime.now().year)
text = re.sub(r'(Copyright\s+\(C\)\s*\d\d\d\d)(.*?)(\d\d\d\d)', r'\1' + '-' + year, text, 
              flags=re.IGNORECASE)

# Overwrite the file with the cleaned text
print("Writing: " + filename)
with open(filename, 'w', encoding='utf-8') as f:
    f.write(text)
