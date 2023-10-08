#!/usr/bin/env python3

import sys, json, os, re
import numpy as np
from scipy.spatial.transform import Rotation as R

q = "-0.7403814635982879 -0.1829843251900557 -0.6393566224605007 -0.09785261575807645"

# split the string into a list of floats
q = [float(x) for x in q.split(' ')]
print(q)

# Convert to rotation matrix
r = R.from_quat(q)

# Print the matrix with 17 digits precision
np.set_printoptions(formatter={'float': lambda x: "{0:0.17f}".format(x)})

print(r.as_matrix())

