#!/usr/bin/python

import sys, os, cv2
import numpy as np
from matplotlib import pyplot as plt

in_file = sys.argv[1]
img = cv2.imread(in_file)

in_dir = os.path.dirname(in_file)
print("indir is ", in_dir)

out_dir = os.path.dirname(in_file) + "_sample"
print("out_dir is ", out_dir)

out_file = out_dir + "/" + os.path.basename(in_file)

try:
    os.mkdir(out_dir)
except OSError:
    pass

try:
    V = np.loadtxt(sys.argv[2])
    rows, cols = V.shape
except:
    sys.exit(1)
    
if rows == 0 or cols == 0:
    sys.exit(1)
    

for row in range(rows):
    x = V[row][3]
    y = V[row][4]
    center_coordinates = (int(x), int(y)) 
  
    # Radius of circle 
    radius = 3
   
    color = (0, 100, 255)  # red
   
    # Line thickness of 2 px 
    thickness = 2
   
    img = cv2.circle(img, center_coordinates, radius, color, thickness)


#plt.imshow(img, cmap = 'CMRmap', interpolation = 'bicubic')
# plt.xticks([]), plt.yticks([])  # to hide tick values on X and Y axis
#plt.show()

print("Writing: " + out_file)
cv2.imwrite(out_file, img)

#cv2.imshow('image', img, cmap = 'bgr')
#cv2.waitKey(0)
#cv2.destroyAllWindows()



