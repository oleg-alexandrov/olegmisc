#!/usr/bin/env python

# Search the current directory for a header with function with this name. 
# Update the corresponding .cc file with the function signature.

import os, re, sys, textwrap

# Find the matching closing parenthesis. For that, split into individual characters.
# Go over each character. If it is an opening parenthesis, increment the count. If it is
# a closing parenthesis, decrement the count. Stop when the count is zero.
def split_by_closing_parenthesis(a):
    count = 0
    num = len(a)
    i = 0
    while i < num:

        if a[i] == '/':
            if i + 1 < num and a[i + 1] == '/':
                # Skip the comment
                while i < num and a[i] != '\n':
                   i = i + 1
            # Go forward either way
            i = i + 1
            continue
        
        if a[i] == '(':
            count = count + 1
            i = i + 1
            continue
            
        if a[i] == ')':
            count = count - 1
            i = i + 1
            if count == 0:
                break
            continue

        i = i + 1

    i = i - 1 # want to keep the closing parenthesis in the after part
    before = a[:i]
    after = a[i:]
    
    return (before, after)

# Split text into comments and non-comments. Comments start with // and go to
# the end of the line. Put all resulting blocks in a list in the order they
# appear in the text.
def split_by_comments(text):
    blocks = []
    block = ''

    # Unlikely text to serve as a separator
    sep = ' xxOddSeparatorxx '
    
    # Given a comment, put a separator before it starts and at the end of the line
    text = re.sub(r'(//.*?)(\n|$)', sep + r'\1' + r'\2' + sep, text)
    
    # Split by the separator
    blocks = text.split(sep)
    
    return blocks

# Find if a given string has a balanced number of parentheses, brackets, angles, and braces.
def is_balanced(s):
    stack = []
    for c in s:
        if c in '([{<':
            stack.append(c)
        elif c in ')]}>':
            if not stack:
                return False
            if c == ')' and stack[-1] != '(':
                return False
            if c == ']' and stack[-1] != '[':
                return False
            if c == '}' and stack[-1] != '{':
                return False
            if c == '>' and stack[-1] != '<':
                return False
            stack.pop()
    return not stack

def split_into_balanced_args(args):

    # Swap comma and newline as we want to keep each newline together with the preceding
    # text.
    args = args.replace(',\n', '\n,')
    
    bargs = []
        
    # Split the arguments by comma
    args = args.split(',')

    # make each arg balanced
    num = len(args)
    i = 0
    while True:
        # Stop if processed all arguments
        if i >= num:
            break
        # Append tokens till balanced
        arg = ''
        isBalanced = False
        for j in range(i, num):
            if arg != "":
                arg = arg + ',' # put back the comma
            arg = arg + args[j]
            if is_balanced(arg):
                bargs.append(arg + ',') # put back the comma
                i = j + 1
                break
    
    # Wipe the comma from the last argument
    if len(bargs) > 0 and len(bargs[-1]) > 0:
        bargs[-1] = bargs[-1][:-1]
    
    # Swap back the comma and newline
    for i in range(len(bargs)):
        bargs[i] = bargs[i].replace('\n,', ',\n')
        
    return bargs

# Main code
    
a = """void transformAppendNvm(// Append from these
                        std::vector<std::map<int, int>>  const& nvm_pid_to_cid_fid,
                        std::vector<Eigen::Matrix2Xd>    const& nvm_cid_to_keypoint_map,
                        std::map<int, int>               const& cid2cid,
                        std::vector<Eigen::Vector2d>     const& keypoint_offsets,
                        int cid_shift,
                        size_t num_out_cams,
                        // Outputs, append to these 
                        std::vector<int> & fid_count,
                        std::vector<std::map<std::pair<float, float>, int>>
                        & merged_keypoint_map,
                        std::vector<std::map<int, int>> & pid_to_cid_fid) {
"""

print("a = ", a)

(before, after) = split_by_closing_parenthesis(a)

# Match the part until opening parenthesis. Match over multiple lines.
m = re.match(r'^(.*?\()(.*)$', before, re.DOTALL)
if not m:
    print("No match")
    sys.exit(1)
before = m.group(1)
args = m.group(2)

# This is important to handle comments properly
blocks = split_by_comments(args)

# Iterate over blocks. Non-comment blocks are split by comma with balanced parentheses.
args = []
for block in blocks:
    if block.startswith('//'):
        args.append(block)
    else:
        args.extend(split_into_balanced_args(block))

# Now we have: before, args, after    
print("before = " + before)
for barg in args:
    print("barg = ", barg)
print("after = " + after)

