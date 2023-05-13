#!/usr/bin/env python

import sys, os, re
from scipy.spatial.transform import Rotation as R
import numpy as np

#roll=30; pitch = 5; yaw=90 # image 11 (ZYX dir)
#roll=30; pitch = 20; yaw=90 # image 12 (ZYX dir)
#roll=30; pitch = 10; yaw=90 # image 13 (ZYX dir)
#roll=30; pitch = 15; yaw=90 # image 14 (ZYX dir)
#roll=30; pitch = 10; yaw=15 # image 15 (ZYX dir)
#roll=30; pitch = 10; yaw=45 # image 16 (ZYX dir)
#roll=5; pitch = 10; yaw=45 # image 17 (ZYX dir)
# roll=15; pitch = 10; yaw=45 # image 18 (ZYX dir)
roll=0; pitch = 0; yaw=0 # image x1
#roll=10; pitch = 0; yaw=0 # image x2
#roll=0; pitch = 10; yaw=0 # image x3
#roll=0; pitch = 0; yaw=10 # image x4
# Find the rotation matrix ZYX for the given roll, pitch, yaw using scipy
r = R.from_euler('ZYX', [yaw, pitch, roll], degrees=True)
#r = R.from_euler('ZYX', [roll, pitch, yaw], degrees=True)

M = r.as_matrix()
#print("M before inv ", M.flatten())

# Camera to world
M = np.linalg.inv(M)

a13 = M[0,2]; a23 = M[1,2]; a33 = M[2,2]
#print("a13, a23, a33: ", a13, a23, a33)

d = 450000.0

u = -d *a13 / a33 + 300.0
v = -d * a23 / a33 + 300.0

#R = 6357000.0

# TODO: Bad notation below. Let R be 356752.314245. Let d be 450000.0. Let z be 1.01.
# Then add these properly.

# Semi-minor axis 
#6356752.314245 m is the semi-minor axis of the WGS84 ellipsoid
#6356752.314245 + 450000 = 6806752.314245
# Add 1.01, get 6806753.324245
R = -6806753.324245

# Add 300, 300 to u, v based on the DART explanation
#print("adding 300 to u and v")
print("C=" + str(u) + " " + str(v) + " " + str(R))
# print with spaces, in double precision
print("R=" + " ".join( [ "%.17g" % x for x in M.flatten() ] ) )

# Go from DART to NED
X = [[0, -1, 0], [1, 0, 0], [0, 0, 1]]

NedToEcef = [[-0.1459270483945784,0.97261575312521398,-0.18089746631591913], 
  [0.61066840158910385,0.23241901121180411,0.75701090251589909], 
  [0.77832473932636581,0,-0.62786192761828186]]
print("NedToEcef=" + " ".join( [ "%.17g" % x for x in np.array(NedToEcef).flatten() ] ) )

NedToEcef2 = np.matmul(NedToEcef, X)
# Camera to ECEF
M2 = np.matmul(NedToEcef2, M)

# Apply rotation to camera center as well
C = [u, v, R]
#print("C=", C)
C2 = np.matmul(NedToEcef2, C)
print("C2=" + str(C2[0]) + " " + str(C2[1]) + " " + str(C2[2]))

# Ground point
C0 = [300.0, 300.0, R + 450000]
C02 = np.matmul(NedToEcef2, C0)
print("C02=" + str(C02[0]) + " " + str(C02[1]) + " " + str(C02[2]))

# Ground poinnt in ECEF
G = [1155314.4474994419,-4834703.6053221142,3983040.8671830427]
print("G=" + str(G[0]) + " " + str(G[1]) + " " + str(G[2]))
# Convert to ECEF
C3 = np.array(C2) - np.array(C02) + np.array(G)
#C3=C2
print("C=" + str(C3[0]) + " " + str(C3[1]) + " " + str(C3[2]))

print("R=" + " ".join( [ "%.17g" % x for x in M2.flatten() ] ) )

# Transform from South Pole to Maryland
# First rotation: (x, y, z) -> ()
#Final shift 364368.55011 4306309.65095 1.01

# M = "0.696364 -0.550979 0.459891 0.696364 0.673766 -0.247216 -0.173648 0.492404 0.852869"

# # ZYX_samples/ima_camera006_roll_30_pitch_10_yaw_45.json
# M = [
#            [
#                 0.696364240320019,
#                 -0.5509785337113081,
#                 0.45989074810530794
#             ],
#             [
#                 0.696364240320019,
#                 0.673766337680281,
#                 -0.24721603308123952
#             ],
#             [
#                 -0.17364817766693033,
#                 0.49240387650610395,
#                 0.8528685319524433
#             ]
#         ]


# # Make this into an array of floats
# #M = np.array(M.split(" ")).astype(np.float64)
# # Reshape into a 3x3 matrix
# #M = M.reshape(3,3)
# print("M is", M)

# C = [242952.68197153776, -130138.87858294338, 450000]

# Z = np.array([0, 0, 1])

# Dir = np.matmul(M, Z)
# print("Dir is", Dir)

# # Find the value s at which the ray C + s * Dir intersects the plane z = 0
# # C[2] + s * Dir[2] = 0
# s = -C[2] / Dir[2]
# print("s is", s)
# # Print the intersection point
# print("Intersection point is", C[0] + s * Dir[0], C[1] + s * Dir[1], C[2] + s * Dir[2])


# # Concatenate M into one single array
# M = np.array(M)

# # Flatten M
# M = M.flatten()
# # Print with spaces, in double precision

# C[2] = -C[2] - 6356752.314245 - 1.01
# print("C=" + str(C[0]) + " " + str(C[1]) + " " + str(C[2]))
# print("R=" + " ".join( [ "%.17g" % x for x in M ] ) )

# # Let M be the transpose of M
# M = M.reshape(3,3)
# M = M.transpose()
# print("M is", M)

# Dir = np.matmul(M, Z)
# print("Dir is", Dir)

# # Find the value s at which the ray C + s * Dir intersects the plane z = 0
# # C[2] + s * Dir[2] = 0
# s = -C[2] / Dir[2]
# print("s is", s)
# # Print the intersection point
# print("Intersection point2 is", C[0] + s * Dir[0], C[1] + s * Dir[1], C[2] + s * Dir[2])
