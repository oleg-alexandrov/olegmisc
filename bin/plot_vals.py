#!/usr/bin/python

# Plot given arrays in given files with the index as the x variable
# and array value as the y avariable.

import sys, os, re
import matplotlib.pyplot as plt

if len(sys.argv) < 2:
    print("Usage: plot_cols.py file.txt")
    sys.exit(1)

filenames = sys.argv[1:]

colors = ['b', 'r', 'g', 'c', 'm']

count = 0
for filename in filenames:
    print("Plot filename: ", filename)
    values = []
    for line in open(filename, 'r'):
        values += [float(s) for s in line.split()]
        
    plt.plot(values, colors[count])
    count += 1
        
plt.show()


