#!/usr/bin/python

# This script is used to merge the given spreadsheets side by side

import sys
import os
import re # Perl-style regular expressions

if len(sys.argv) < 2:
    print "Usage:", sys.argv[0], "file1.csv file2.csv ..."
    sys.exit(1)

cols         = []
max_num_rows = 0
for filename in sys.argv[1:]:
    
    handle = open(filename, 'r')
    text = handle.read()
    handle.close()
    lines = text.split("\n")
    lines = [filename] + lines
    max_num_rows = max(max_num_rows, len(lines))
    cols.append(lines)


# Print each spreadsheet row
for count in range(max_num_rows):

    row = ""
    for ptr in cols:
        if len(ptr) < count + 1:
            row = row + ","
        else:
            row = row + ptr[count] + ","
            
    print row
    
