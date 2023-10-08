# shebang
#!/usr/bin/python

# A script to open a file, read it line by line, add up all the numbers, 
# and print the result

import sys, os, re

# Check if the user has provided a file name
if len(sys.argv) < 2:
    print("Usage: {} <filename>".format(sys.argv[0]))
    sys.exit(1)

# Check if the file exists
if not os.path.isfile(sys.argv[1]):
    print("Error: File '{}' not found".format(sys.argv[1]))
    sys.exit(1)

# Open the file
fh = open(sys.argv[1])

# Read the file line by line
total = 0
for line in fh:
    # convert the line to a float
    line = line.strip()
    val = float(line)
    total += val

# Print the total
print("Total: {}".format(total))
