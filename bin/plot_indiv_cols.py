#!/usr/bin/python

# Plot indices vs each column. Columns start from 1. Assume 2 or 3 columns.

# Restrict the range. This last things should be an option, such as -min -1 -max
# 3.

import sys, os, re
import matplotlib.pyplot as plt

if len(sys.argv) < 4:
    print("Usage: plot_cols.py file.txt col1 col2 (columns start from 1)")
    sys.exit(1)
    
filename = sys.argv[1]
col1 = int(sys.argv[2])
col2 = int(sys.argv[3])
col3 = -1

# If there is another argument, it is the third column
if len(sys.argv) > 4:
    col3 = int(sys.argv[4])

X, Y, Z = [], [], []
for line in open(filename, 'r'):
    count = 0
    for s in line.split():
        if count == col1 - 1:
            X.append(float(s))
        if count == col2 - 1:
            Y.append(float(s))
        
        # See if col3 is present, and then create Z
        if col3 != -1 and count == col3 - 1:
            Z.append(float(s))
            
        count += 1

# Let I be just the indices from 0 to len(X) - 1
I = range(len(X))
plt.plot(I, X, 'b.')
plt.plot(I, Y, 'r.')

# If there is Z, and is not empty, plot it in green
if col3 != -1 and len(Z) > 0:
    plt.plot(I, Z, 'g.')
    
# Restrict the y axis range from -1 to 3
plt.ylim(-1, 3)

plt.show()
