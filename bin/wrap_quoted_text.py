#!/usr/bin/env python

# The tag that will identify the text to process
tag = 'xwr'

# Search the current directory for a file and a block of lines in that file
# having the text in the tag. These lines start and end with quotes. Remove the
# quotes, wrap the lines to desired width, add back the codes, and replace the
# text in the file. 

# The application is wrapping nicely the text for boost program options.

import os, re, sys, textwrap

def wrap_text(text):

    # Indent with 5 spaces
    indent = '     '

    # Wipe any double quotes.
    text = text.replace('"', '')

    # Replace newlines with spaces.
    text = text.replace('\n', ' ')

    # Wipe any leading and training parentheses and spaces.
    text = re.sub(r'^[\s\(]*', '', text)
    text = re.sub(r'[\s\)]*$', '', text)

    # Keep only one space between words.
    text = re.sub(r'\s+', ' ', text)

    # Wrap the text to 84 characters. Later, after indent, quotes, and parenthesis are added,
    # this will grow to about 90.
    #text = wrap.wrap(text, 84)
    w = textwrap.TextWrapper(width=84)
    text = w.fill(text)

    # Split by newline
    lines = text.split('\n')

    # To each line, add the indent, starting quote, trailing space, ending quote
    for i in range(len(lines)):
        # Remove any trailing spaces
        lines[i] = lines[i].rstrip()
        # No space at the end of the last line
        spc = ' ' 
        if i == len(lines) - 1:
            spc = ''
        lines[i] = indent + '"' + lines[i] + spc + '"'

    # Join the lines with newlines
    text = '\n'.join(lines)

    # Add the trailing parenthesis
    text = text + ')'

    return text

def process_file(file, beg_line):

    # Read the lines in the file
    lines = open(file, 'r').readlines()

    # Collect the text in the block of lines starting from the line number. Stop
    # when when reaching the string po:: or spaces and a starting parenthesis or colon,
    # as that means the end of the block.
    text = ''
    end_line = 0
    for i in range(beg_line, len(lines)):
        if re.search(r'po::', lines[i]) or \
           re.search(r'^\s+[\(;]', lines[i]):
            end_line = i
            break
        # Remove the tag and any spaces around it
        lines[i] = re.sub(r'\s*' + tag + r'\s*', '', lines[i])
        text = text + lines[i]

    # Wrap the text            
    text = wrap_text(text)

    # Replace the text. Join earlier line, current text, and later lines
    text = ''.join(lines[0:beg_line]) + text + "\n" + ''.join(lines[end_line:])

    # Overwrite the file
    open(file, 'w').write(text)
        
# Main program

# Do several attempts, as long as the tag can be found. Each attempt will
# rewrite a file, and can change the line numbers, hence the need to search
# again.

while True:
    
    # In the current directory recursively search for the tag
    cmd = ['grep', '-r', '-i', '-n', '-E', tag, '.']
    output = os.popen(' '.join(cmd) + ' 2>/dev/null').read()
    lines = output.split('\n')
    if len(lines) == 1:
        break # No more tags found
        
    # Match the file and the line number
    line = lines[0]        
    beg_line = 0
    m = re.match(r'^(.*):(\d+):', line)
    if not m:
        continue
        
    # Process the file
    file = m.group(1)
    beg_line = int(m.group(2))
    # Make the line number start from 0
    beg_line = beg_line - 1

    # Process the file    
    process_file(file, beg_line)
    
