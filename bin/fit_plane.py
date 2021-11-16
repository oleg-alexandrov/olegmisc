#!/usr/bin/python

import sys, os, re
import numpy as np
from numpy.linalg import svd

# Find a best fit plane for a set of points, then shift those points
# along the normal to that plane so that they are on the moon surface.

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

if len(sys.argv) < 3:
    print("usage: fit_plane.py in.txt out.txt")

inFile = sys.argv[1]
outFile = sys.argv[2]

print("Reading: " + inFile)

points = np.loadtxt(inFile)

print("points shape is ", points.shape)

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

