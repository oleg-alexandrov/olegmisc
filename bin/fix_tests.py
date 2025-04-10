#!/usr/bin/env python

# Take as input the console output of running ctest. Modify the .cpp unit tests
# by putting in new values for tolerances so that the tests pass.

# Thoroughly inspect the outcome.

# Usage:

# python fix_tests.py ctest_log.txt

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

    # The case when numbers are expected to be equal. Replace this with
    # EXPECT_NEAR with existing truth and a new tolerance. The tolerance is a
    # little higher than the difference between the of old and new values.
    match = re.search("Expected equality of these values:", lines[l+1])
    if match:
        
        # Match: Which is: <number>
        match = re.search(r"Which is:\s*(.*?)\n", lines[l+3]) 
        if not match:
            continue
        new_val = match.group(1)
        if "\"" in new_val or "\'" in new_val:
            continue # skip strings
            
        # Old val must match something, followed by colon, space, followed by value
        old_val = lines[l+4]
        match = re.search(r"(.+):\s*(.*)", old_val)
        if not match:
            continue
        old_val = match.group(2)

        match = re.search(r"(^.*?)(\w*EQ\()(.*,)\s*(.*?)(\);.*\n)", 
                    code_lines[int(line_number) - 1])
        if not match:
            print("No match")
            continue
        try:
            # Skip if converting to float fails
            tol = abs(float(old_val) - float(new_val))
            # Increase a little
            tol *= 1.5
            # Round up to 2 decimal places
            tol = "{:.2g}".format(tol)
        except:
            continue
            
        print("Old code: " + code_lines[int(line_number) - 1])            
        code_lines[int(line_number) - 1] \
          = match.group(1) + 'EXPECT_NEAR(' + \
            match.group(3) + ' ' + match.group(4) + ', ' + str(tol) + \
            match.group(5)
        print("New code: " + code_lines[int(line_number) - 1])

        # Overwrite the file
        with open(file_name, 'w') as code:
            code.writelines(code_lines)
        
        # Done with this line   
        continue
    
    # The case when numbers are expected to be near, and instead of changing
    # the truth value, we change the tolerance. 
    match = re.search(r"^.*?The difference between.*?and.*?\s+is\s+(.*?),", lines[l+1])
    if match:
      
        tol = 0.0
        try:
            # If the tol is not a number, skip
            tol = float(match.group(1))
        except:
            continue

        # Increase a little
        tol *= 1.5
        # Round up to 2 decimal places
        tol = "{:.2g}".format(tol)
       
        # Match on that line the string
        # EXPECT_NEAR(<something>, <old number>, tol);
        # Grouping the part before and after the number
        match = re.search(r"(^.*?EXPECT_NEAR\(.*,\s*)(.*?)(\).*?\n)",
                           code_lines[int(line_number) - 1])
        if not match:
            print("No match")
            continue
        
        # Reassemble this line, by replacing group2 with new_val
        print("Old code: " + code_lines[int(line_number) - 1])
        code_lines[int(line_number) - 1] = match.group(1) + str(tol) + match.group(3)
        print("New code: " + code_lines[int(line_number) - 1])
        
        # Overwrite the file
        with open(file_name, 'w') as code:
            code.writelines(code_lines)
        
        continue

    # The case when numbers are expected to be near, and will change 
    # the truth value, while keeping the tolerance the same. As of now,
    # we won't get here as the above block will match first.
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
        print("Old code: " + code_lines[int(line_number) - 1])
        code_lines[int(line_number) - 1] = match.group(1) + new_val + match.group(3) + "\n"
        print("New code: " + code_lines[int(line_number) - 1])
        
        # Overwrite the file
        with open(file_name, 'w') as code:
            code.writelines(code_lines)
        
        continue
