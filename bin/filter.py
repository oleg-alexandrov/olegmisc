#!/usr/bin/env python

# Read all lines from standard input. Print only those lines for which the value
# of column col is at least equal to val. The first column is column 1. 

import sys

# Ensure that the correct number of arguments are provided
if len(sys.argv) < 3:
    print("Usage: filter.py col val operator")
    sys.exit(1)
  
# Column value
col = int(sys.argv[1])
# Value to compare
val = float(sys.argv[2])

# If the operator exists, read it
op = 'ge'
if len(sys.argv) > 3:
    op = sys.argv[3]

# Read the lines from standard input. Iterate over them.
for line in sys.stdin:
    # Split the line into columns
    columns = line.split()
    
    # if col - 1 is negative, add the number of columns to it
    if col - 1 < 0:
        col = col + len(columns)
        
    # If the column is at least equal to the value, print the line
    if float(columns[col - 1]) >= val and op == 'ge':
        print(line, end="")
        continue
        
    # also treat le
    if float(columns[col - 1]) <= val and op == 'le':
        print(line, end="")
        continue

