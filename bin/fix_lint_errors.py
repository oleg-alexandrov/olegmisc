#!/usr/bin/env python
#
# Copyright (c) 2021, United States Government, as represented by the
# Administrator of the National Aeronautics and Space Administration.
#
# All rights reserved.
#
# The "ISAAC - Integrated System for Autonomous and Adaptive Caretaking
# platform" software is licensed under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with the
# License. You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.

# A tool to fix some lint errors that clang-format-8 skips over.

import sys, os, re, shutil

def parse_lint_errors(error_file):
    if not os.path.exists(error_file):
        print("File " + error_file + " does not exist.")
        sys.exit(1)

    errors = {}
    
    with open(error_file, "r") as f:
        for line in f.readlines():
            m = re.match("^(.*?)\s*:\s*(\d+)\s*:\s*(.*?)\n", line)
            if not m:
                continue
            
            code_file = m.group(1)
            line_num  = int(m.group(2)) - 1 # make the lines start from 0
            error_msg = m.group(3)

            if code_file not in errors:
                errors[code_file] = {}

            errors[code_file][line_num] = error_msg

    return errors

def fix_errors(errors):

    num_fixes = {}
    for code_file in errors:

        if not os.path.exists(code_file):
            print("File " + code_file + " does not exist.")
            continue

        shutil.copyfile(code_file, code_file + ".bk")

        lines = []
        with open(code_file, "r") as f:
            lines = f.readlines()

        num_fixes[code_file] = 0
        
        for line_num in errors[code_file]:

            error = errors[code_file][line_num]
            line  = lines[line_num]

            if error.startswith("Line ends in whitespace"):
                line = line.rstrip() + "\n" # wipe whitespace, keep the newline
                num_fixes[code_file] += 1
                
            if error.startswith("Redundant blank line at the start of a code block") or \
                   error.startswith("Redundant blank line at the end of a code block"):
                if re.match("^\s*\n", line):
                    line = ""
                    num_fixes[code_file] += 1
                else:
                    # Need to be careful here, as this operation removes lines,
                    # so the error file text will not be accurate anymore.
                    print("Expecting a blank line. Your error file may be old.")
                    sys.exit(1)

            if error.startswith("At least two spaces is best between code and comments"):
                # This may need several iterations
                found = True
                while found:
                    found = False
                    
                    m1 = re.match("^(.*?[^\s])(\/\/.*?\n)", line)   # no  space after comment
                    if m1:
                        line = m1.group(1) + "  " + m1.group(2) # ensure two spaces
                        num_fixes[code_file] += 1
                        found = True
                        
                    m2 = re.match("^(.*?[^\s])\s(\/\/.*?\n)", line) # one space after comment
                    if m2:
                        line = m2.group(1) + "  " + m2.group(2) # ensure two spaces
                        num_fixes[code_file] += 1
                        found = True

            if error.startswith("Missing space before {"):
                # This may need several iterations
                found = True
                while found:
                    found = False
                    m = re.match("^(.*?[^\s])(\{.*?\n)", line)
                    if m:
                        line = m.group(1) + "  " + m.group(2) # insert a space
                        num_fixes[code_file] += 1
                        found = True
                
            # Save back the processed line
            lines[line_num] = line
            
        with open(code_file, 'w') as f:
            f.writelines(lines)

    return num_fixes
    
def main():
    if len(sys.argv) < 2:
        print("Usage: " + sys.argv[0] + " errors.txt")
        sys.exit(1)

    errors = parse_lint_errors(sys.argv[1])

    num_fixes = fix_errors(errors)

    print("Number of fixed errors:")
    for code_file in num_fixes:
        print("  " + code_file + ": " + str(num_fixes[code_file]))

    print("\nThe original files were preserved with the added .bk extension.")
    
if __name__ == "__main__":
    main()
