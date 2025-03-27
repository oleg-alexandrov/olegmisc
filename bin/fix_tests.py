#!/usr/bin/env python

# Take as input a text file that shows failures after running ctest. Modify the
# unit tests to reflect the new values so that the tests pass.

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
    
    if not match:
        continue

    file_name, line_number = match.groups()
    
    # Open the cpp test file and read the lines
    code_lines = []
    with open(file_name, 'r') as code:
        code_lines = code.readlines()
            
    # print the file name and line number
    print("File name: " + file_name + " Line number: " + line_number)
    
    # Check that the line number is within the range of the file
    if int(line_number) > len(code_lines):
        print("File " + file_name + " has less than " + line_number + " lines.")
        sys.exit(1)

    # The case when numbers are expected to be equal
    match = re.search("Expected equality of these values:", lines[l+1])
    if match:
        
        # Match Which is: <number>
        match = re.search(r"Which is:\s*(.*?)\n", lines[l+3]) 
        if not match:
            continue
        new_val = match.group(1)
        if "\"" in new_val or "\'" in new_val:
            continue # skip strings
        old_val = lines[l+4]
        
        match = re.search(r"(^.*?EXPECT_EQ\(.*,\s*)(.*?\);)", 
                    code_lines[int(line_number) - 1])
        if not match:
            continue
        code_lines[int(line_number) - 1] = match.group(1) + new_val + ");\n"

        # Overwrite the file
        with open(file_name, 'w') as code:
            code.writelines(code_lines)
           
        continue
    
    # The case when numbers are expected to be near    
    # In line line[l+2], find the pattern: 'evaluates to <number>'
    # and extract the new value <number>
    match = re.search(r"evaluates to (.+)", lines[l+2])
    if match:
        
        new_val = match.group(1)

        # Match on that line the string
        # EXPECT_NEAR(<something>, <old number>, 1e-8);
        # Grouping the part before and after the number
        match = re.search(r"(^.*?EXPECT_NEAR\(.+?,\s*)(.+?,)(\s*.+?\);)", 
                            code_lines[int(line_number) - 1])
        if not match:
            continue
            
        # Reassemble this line, by replacing group2 with new_val
        code_lines[int(line_number) - 1] = match.group(1) + new_val + match.group(3) + "\n"
        
        # Overwrite the file
        with open(file_name, 'w') as code:
            code.writelines(code_lines)
        
        continue
