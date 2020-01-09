#! /usr/bin/env python

from osgeo import gdal, osr
import numpy as np
import argparse
from os import system

parser=argparse.ArgumentParser(description="Utility to mosaic a set of sub-image point clouds into a composite point cloud with a vrt index.  Assumes that individual point clouds are in double-precision floating point format.  Each subset is assigned an arbitrary position in the mosaic, with its left border 2048 pixels to the right of the right border of its left-hand neighbor.")

parser.add_argument('-o', default='PC.vrt', help='set output file name, default=%default')
parser.add_argument('files', metavar='file', nargs='+', help='PC files to mosaic')

args=parser.parse_args()

for file in args.files:
    print "found file:%s" % file

wkt_str='LOCAL_CS["Cartesian Grid", LOCAL_DATUM["XY grid", 0, UNIT["m", 1.0], AXIS["x", EAST], AXIS["y", North]]'
this_srs=osr.SpatialReference()
this_srs.ImportFromWkt(wkt_str)
print "projection is:"+this_srs.ExportToProj4()

file_string=" "
nxy=np.zeros([len(args.files), 2])
x0y0=np.zeros_like(nxy)
x1y1_last=np.array([0., 0.])
for count, file in enumerate(args.files):
    
    ds=gdal.Open(file, gdal.GA_Update)
    nxy[count, 0]=ds.RasterXSize
    nxy[count, 1]=ds.RasterYSize
    x0y0[count,:]=x1y1_last+np.array([ 2048., 0.])
    x1y1_last=x0y0[count,:]+[nxy[count,0], 0]
    #ds.SetProjection(this_srs)
    ds.SetGeoTransform(np.array([x0y0[count,0], 1., 0., x0y0[count,1], 0, -1]));
    ds=None

    file_string=file_string+" "+file


system('gdalbuildvrt PC.vrt '+file_string)



