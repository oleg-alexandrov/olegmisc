#!/usr/bin/python

import sys, os, re
import matplotlib.pyplot as plt

if len(sys.argv) < 4:
    print("Usage: plot_cols.py file.txt col1 col2 (columns start from 1)")
    sys.exit(1)
    
filename = sys.argv[1]
col1 = int(sys.argv[2])
col2 = int(sys.argv[3])

style = '.'
if len(sys.argv) > 4:
    style = sys.argv[4]

X, Y = [], []
for line in open(filename, 'r'):
    count = 0
    for s in line.split():
        if count == col1 - 1:
            X.append(float(s))
        if count == col2 - 1:
            Y.append(float(s))
        count += 1

plt.plot(X, Y, style)
plt.show()


