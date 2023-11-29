#!/usr/bin/env python

# Read the last command from $HOME/.zsh_history (before this one). Remove the leading ':
# 1701285387:0;' if present, also any leading '#'. 

# If this command is invoked with some numbers as arguments, replace those with
# the corresponding arguments from command from that last line (indices start
# from 1). Create and run a new command with the result.

import os, re, sys, subprocess

def read_n_to_last_line(filename, n = 1):
    """Returns the nth before last line of a file (n=1 gives last line)"""
    num_newlines = 0
    with open(filename, 'rb') as f:
        try:
            f.seek(-2, os.SEEK_END)    
            while num_newlines < n:
                f.seek(-2, os.SEEK_CUR)
                if f.read(1) == b'\n':
                    num_newlines += 1
        except OSError:
            f.seek(0)
        last_line = f.readline().decode()
    return last_line

def is_integer(n):
    try:
        float(n)
    except ValueError:
        return False
    else:
        return float(n).is_integer()
            
histFile = os.path.join(os.environ['HOME'], '.zsh_history')

# Read the next to last line from the history file, as the last
# one is the current command
line = read_n_to_last_line(histFile, 2)
    
# Replace anything from starting ':' to first ';' with nothing
m = re.match(r':.*?;\s*\#?\s*(.*?)$', line)
if m:
    line = m.group(1)

# Break up this line into tokens
tokens = line.split()

# Process all input arguments
cmd = []

# Get the array of input arguments. Eliminate the first which is a command,
# and keep the rest, which are arguments.
args = sys.argv[1:]
for arg in args:
    # If this argument is a number, replace it with the corresponding token from the
    # last line
    if is_integer(arg):
        cmd.append(tokens[int(arg)])
    else:
        cmd.append(arg)

print(" ".join(cmd))
# Run this command
subprocess.run(cmd)
