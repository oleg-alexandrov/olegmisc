#! /usr/bin/env python
# Replace colmap camera pose with the version from build_map
#sbaglapl@uw.edu

import os, sys, re
from pyquaternion import Quaternion
import numpy as np

input_folder  = sys.argv[1] # folder continining cameras output from build_map
cam_file_in   = sys.argv[2] # colmap camera file to be updated.
cam_file_out  = sys.argv[3] # output updated file.

# Must save the poses beforehand as follows, for this script to succedd:
#build_map -output_map mymap.map -save_poses

print("Reading camera folder: " + input_folder)
print("Reading image list: " + cam_file_in)
with open(cam_file_in,'r') as f:
    lines = f.readlines()

for i in range(len(lines)):
    line = lines[i]
    if re.match('^\#', line):
        continue

    m = re.match('^(\d+)\s+(.*?)(\d+)\s+([^\s]*?\.jpg)', line)
    if not m:
        lines[i] = '\n'
        continue
    
    image_id = m.group(1)
    old_trans = m.group(2)
    cam_id = m.group(3)
    image = m.group(4)

    trans_file = input_folder + "/" + image
    m = re.match('^(.*?)\.jpg', trans_file)
    if not m:
        print("Could not match image in: " + trans_file)
        sys.exit(1)

    trans_file = m.group(1) + "_cam2world.txt"
    if not os.path.exists(trans_file):
        print("Cannot find: " + trans_file)

    print("Loading " + trans_file)
    transform = np.loadtxt(trans_file, usecols=range(4))
    rotation     = transform[0:3, 0:3]
    translation  = transform[0:3, 3]

    # Convert from camera to world to world to camera
    inv_r = np.linalg.inv(rotation)
    inv_t = -np.matmul(inv_r, translation)
    #inv_t = translation
    #inv_r = rotation
    q = Quaternion(matrix=inv_r) 
    new_trans = str(q.w) + ' ' + str(q.x) + ' ' + str(q.y) + ' ' + str(q.z) + \
                ' ' + str(inv_t[0]) + ' ' + str(inv_t[1]) + ' ' + str(inv_t[2])

    lines[i] = image_id + ' ' + new_trans + ' ' + cam_id + ' ' + image + '\n'

print("Writing: " + cam_file_out)
with open(cam_file_out, 'w') as f:
    f.write("".join(lines))
        
