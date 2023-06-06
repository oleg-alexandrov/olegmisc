#!/usr/bin/python

# Fix paths of the form /home/.../bin to be relative to /usr/. This way
# the build environment paths won't leak. This will apply only to text files.

import os, sys, re

filename = sys.argv[1]
print("Fixing paths in: " + filename)

# Read all lines from the file
with open(filename, 'r') as f:
    lines = f.readlines()

# Iterate through all lines and replace the paths
for count in range(len(lines)):
    while True:
        m = re.match("^(.*?)(\/home|\/Users)([\/\w\s]+\/)(bin|lib|libexec|include|share|plugins|appdata)(.*?\n)", lines[count])
        if m:
            lines[count] = m.group(1) + "/usr/" + m.group(4) + m.group(5)
        else:
            break

# Write all lines back to the file
with open(filename, 'w') as f:
    f.writelines(lines)
