#!/usr/bin/env python3

import sys, json, os, re
import numpy as np
from scipy.spatial.transform import Rotation as R

# Check if we have at least one input file
if len(sys.argv) < 2:
    print("Usage: read_csm_state.py <file>")
    sys.exit(1)

# Load the first file
file = sys.argv[1]

q = "-0.3845985099212119, 0.7963418244354942, -0.42036560659942196, 0.2030183282598694"

# split the string into a list of floats
q = [float(x) for x in q.split(',')]
print(q)

# Convert to rotation matrix
r = R.from_quat(q)
print(r.as_matrix())

# Now start with matrix
T = "-0.62173508911404707 -0.44167681406654419 0.64681301075378461 -0.78322760355106746 0.35060814009635666 -0.5134476147902779 1.2703908662455742e-13 -0.82583020289536579 -0.56391885585232837"
T = [float(x) for x in T.split(' ')]
# Reshape to 3x3 matrix using the reshape function
T = np.reshape(T, (3, 3))
print(T)
# Convert to quaternion using scipy
r = R.from_matrix(T)
print("Final quaternion:")
print(r.as_quat())

# read into string
with open(file, 'r') as f:
    data = f.read()

# Find first occurrence of open brace. This is needed because the CSM
# state has some text before the JSON object.
pos = data.find('{')
# do substring from pos to the end, if pos was found
if pos != -1:
    data = data[pos:]

# parse the json from data
j = json.loads(data)
# print the json 
# print(json.dumps(j, indent=4, sort_keys=True))

# Print all keys in the json
print("will print all keys in the json")
# for key in j.keys():
#     print(key)

# Read the quaternions
quats = j['m_quaternions']

# Reshape to Nx4 matrix using the reshape function
quats = np.reshape(quats, (-1, 4))
# Iterate over the rows and convert to rotation matrix
for i in range(quats.shape[0]):
    r = R.from_quat(quats[i, :])
    # print the rotation matrix
    print(r.as_matrix())

# print the quaternions
#print("will print the quaternions")
#print(quats)

