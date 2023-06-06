#!/usr/bin/python

from matplotlib import cm

cmap = cm.turbo
num = cmap.N
for i in range(num):
   rgba = cmap(i)
   print(i / 255.0,
         round(255 * rgba[0]),
         round(255 * rgba[1]),
         round(255 * rgba[2]))
