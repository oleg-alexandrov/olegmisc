#!/usr/bin/env python

import re, sys

# Get log_file as first argument from sys
log_file = sys.argv[1]

# print the log file
print("Log file: " + log_file)

# Read the log file as array, with each element on a line
with open(log_file, 'r') as f:
    lines = f.readlines()

# Iterate over lines using a for loop with l over range of lines
#Search for:
# a pattern like: /path/to/file.cpp:117: Failure
# Match the file name, line number

for l in range(len(lines)):
    match = re.search(r"(/.+?\.cpp):(\d+):\s*Failure", lines[l])
    if match:
        file_name, line_number = match.groups()
        # print the file name and line number
        print("File name: " + file_name + " Line number: " + line_number)
        # In line line[l+2], find the pattern
        # evaluates to <number>
        # and extract the new value <number>
        match = re.search(r"evaluates to (.+)", lines[l+2])
        if not match:
            continue
            
        new_value = match.group(1)
        # print the new value
        print("New value: " + new_value)
            
        # Open the cpp test file and read the lines
        with open(file_name, 'r') as code:
            code_lines = code.readlines()
            
            # Print the relevant line from that file
            print("Old line: " + code_lines[int(line_number) - 1])
            # Match on that line the string
            # EXPECT_NEAR(<something>, <old number>, 1e-8);
            # Grouping the part before and after the number
            match = re.search(r"(^.*?EXPECT_NEAR\(.+?,\s*)(.+?,)(\s*.+?\);)", 
                              code_lines[int(line_number) - 1])
            if not match:
                continue
                
            # print group 1, group 2, group 3
            print("Group 1: " + match.group(1) + " Group 2: " + match.group(2) + " Group 3: " + match.group(3))
            # Reassemble this line, by replacing group2 with new_value
            code_lines[int(line_number) - 1] = match.group(1) + new_value + match.group(3) + "\n"
            
            # Overwrite the file
            with open(file_name, 'w') as code:
                code.writelines(code_lines)
