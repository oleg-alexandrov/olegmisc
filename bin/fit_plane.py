#!/usr/bin/python

import sys, os, re
import numpy as np
from numpy.linalg import svd

# Fit a plane to given points
# Points me 1 per line.
# ignore points like (0, 0, 0).

def planeFit(points):
    """
    p, n = planeFit(points)

    Given an array of points, stored as an np array with d rows, 
    representing points in d-dimensional space,
    fit an d-dimensional plane to the points.
    Return a point, p, on the plane (the point-cloud centroid),
    and the normal, n.
    """
    points = np.reshape(points, (np.shape(points)[0], -1)) 
    assert points.shape[0] <= points.shape[1], \
           "There are only {} points in {} dimensions.".format(points.shape[1], points.shape[0])

    ctr = points.mean(axis=1) # find the average of each row
    x = points - ctr[:,np.newaxis]
    M = np.dot(x, x.T) # Could also use np.cov(x) here.
    return ctr, svd(M)[0][:,-1]

inFile = sys.argv[1]
inNormal_str = sys.argv[2]

inNormal = np.array(inNormal_str.split()).astype(float)
print("Reading: " + inFile)

outFile = inFile
m = re.match('^(.*?\.)(ply|txt)', outFile)
if not m:
    print("Could not match image in: " + outFile)
    sys.exit(1)
outFile = m.group(1) + "csv"

with open(inFile, 'r') as f:
    lines = f.readlines()

points = np.empty((0,3), float) # empty array with 3 columns (x, y, z, intensity)
count = 0
for line in lines:
    # Keep only lines with numbers. This is fragile.
    if not re.match('^[\s\d\.\-\+e]*?$', line):
        continue

    # Make the values in the line into floats
    vals = np.array(line.split()).astype(float)

    if vals.size < 3:
        continue
    
    # Skip invlalid values
    if vals[0] == 0 and vals[1] == 0 and vals[2] == 0:
        continue

    points = np.append(points, [vals[0:3]], axis = 0) # append a new row
    count = count + 1

    if count % 100 == 0:
        print("Count ", count, len(lines))
    #if count == 5:
    #    break


if inNormal.size >= 3:
    normal = inNormal
else:
    # Find the best fit plane to the xyz values (without intensity)
    [ctr, normal] = planeFit(np.transpose(points[:, 0:3]))

print("Normal: ", normal)

# Shift the points along the normal so they are on the Moon surface
radius = 1737400.0

print("Writing: " + outFile)
with open(outFile, 'w') as f:
    for point in points:
        point = point + radius * normal
        f.write('%0.16g %0.16g %0.16g\n' % (point[0], point[1], point[2]))

