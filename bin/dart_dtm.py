#!/usr/bin/env python

import sys, os, re
import numpy as np

# Let file1 be the first argument
file1 = sys.argv[1]
print("file1: " + file1)

# Read the file as a numpy matrix of floats
data = np.loadtxt(file1, dtype='float', delimiter=' ')

# print data dimensions
print("data.shape: " + str(data.shape))

# Find the min and max of first column
minx = np.min(data[:,0])
maxx = np.max(data[:,0])
print("minx: " + str(minx))
print("maxx: " + str(maxx))

# same for y
miny = np.min(data[:,1])
maxy = np.max(data[:,1])
print("miny: " + str(miny))
print("maxy: " + str(maxy))

shift_x = 364368.55011
shift_y = 4305710.65095
shift_z = 1.01000

# The x axis in DART is actually pointing toward south, and y axis of DART is pointing toward east. Therefore, when you consider the traditional coordinate system: x points to the east, and y points to the north. The DART coordinate system is 90 clockwise rotation of the horizontal coordinate frame. 

x_out = shift_x + data[:, 1]
y_out = shift_y + (maxx - minx) - data[:, 0]
z_out = shift_z + data[:, 2]

print("Final shift " + str(shift_x) + " " + str(shift_y + (maxx - minx)) + " " + str(shift_z))

# Save this to a text file with double precision as 3 columns
outFile = "dtm.txt"
print("Will save file: " + outFile)
np.savetxt(outFile, np.c_[x_out, y_out, z_out], fmt='%.17g', 
           delimiter=' ', newline='\n', header='', footer='', comments='# ')



