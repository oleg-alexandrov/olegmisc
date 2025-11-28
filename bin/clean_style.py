#!/usr/bin/env python

# Clean the style in C++ files 

import os, re, sys, textwrap, glob, datetime, uuid

# Create a dictionary to store comments temporarily
comment_map = {}
    
def hide_comments(match):
    # Generate a unique unique placeholder key
    placeholder = f"__COMMENT_{uuid.uuid4().hex}__"
    comment_map[placeholder] = match.group(0)
    return placeholder

# Regex to capture C++ comments (// and /* */) while ignoring strings
# Group 1: Strings (double or single quotes) - we want to keep these
# Group 2: Comments - we want to hide these
pattern_mask = r'(\".*?\"|\'.*?\')|(/\*.*?\*/|//[^\r\n]*)'
    
# Callback to process matches
def replacer(match):
    if match.group(2): # It's a comment
        return hide_comments(match)
    else: # It's a string, return as is
        return match.group(1)

# Check that there exists one input argument
if len(sys.argv) < 2:
    print("Usage: clean_style.py file")
    sys.exit(1)

# Read the text from the file as one string
filename = sys.argv[1]
text = ''
with open(filename, 'r', encoding='utf-8') as f:
    text = f.read()

# Mask the comments. This prevents them from being modified by the regexes below.
text = re.sub(pattern_mask, replacer, text, flags=re.MULTILINE|re.DOTALL)

# No space or tab after open parentheses
text = re.sub(r'\([ \t]+', '(', text)
# No space or tab before close parentheses
text = re.sub(r'[ \t]+\)', ')', text)
# No space followed by newline
text = re.sub(r'[ \t]+\n', '\n', text)
# No space/tab before comma
text = re.sub(r'[ \t]+,', ',', text)
# Always a space between if/for/while and the following parenthesis
text = re.sub(r' (if|for|while)\s*\(', r' \1 (', text)
# A space between } and else
text = re.sub(r'}\s*else', '} else', text)
# Same for catch
text = re.sub(r'}\s*catch', '} catch', text)

# A space between else and {
text = re.sub(r'else\s*{', 'else {', text)

# No ) { on its own line. This can choke on comments, this is why they are masked.
text = re.sub(r'\s*\)\s*{', ') {', text)

# Replace the tab character with 4 spaces
text = re.sub(r'\t', '    ', text)

# Restore the comments
for placeholder, original_comment in comment_map.items():
    text = text.replace(placeholder, original_comment)

# Update the copyright year
year = str(datetime.datetime.now().year)
text = re.sub(r'(Copyright\s+\(C\)\s*\d\d\d\d)(.*?)(\d\d\d\d)', r'\1' + '-' + year, text, 
              flags=re.IGNORECASE)

# Overwrite the file with the cleaned text
print("Writing: " + filename)
with open(filename, 'w', encoding='utf-8') as f:
    f.write(text)
