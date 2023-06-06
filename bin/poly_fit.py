#!/usr/bin/python

import sys, os, re
import matplotlib.pyplot as plt

import numpy as np
import os, sys, glob, shutil
from pyproj import Proj, transform, Transformer
from scipy.spatial.transform import Rotation as R

def poly_fit(X, Y):
    """
    Fit a polynomial of degree 1 and return the fitted Y values.
    """
    fit = np.poly1d(np.polyfit(X, Y, 1))
    return fit(X)

Y = np.array([1, 3, 3])
X = np.array(range(len(Y)))

f, ax = plt.subplots(1, 1, sharex=True, sharey = False, figsize = (12, 9))

print("ax is ", ax) # will be a 3x3 matrix

deg = 1
F = poly_fit(X, Y)

ax.plot(X, Y - F, label='orig', color = 'b')
#ax.plot(X, resid, label='fit', color = 'r')

plt.tight_layout()
plt.show()


