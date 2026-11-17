#!/usr/bin/env python3
# Cut a size x size pixel crop of a raster centered at each spot of a CSV (name,x,y in the raster's
# projection), shifted inward to stay inside the raster. Writes <outDir>/<name>.tif. Opening the
# raster and the crops in stereo_gui -w (single window), clicking a crop's name zooms to that spot.
# Usage: spot_crops.py raster spots.csv size outDir
import csv, os, sys
from osgeo import gdal
gdal.UseExceptions()
src, spots, size, out = sys.argv[1], sys.argv[2], int(sys.argv[3]), sys.argv[4]
os.makedirs(out, exist_ok=True)
d = gdal.Open(src)
g = d.GetGeoTransform()
for r in csv.DictReader(open(spots)):
  c = int((float(r["x"]) - g[0]) / g[1]) - size // 2
  l = int((float(r["y"]) - g[3]) / g[5]) - size // 2
  c = min(max(c, 0), d.RasterXSize - size)
  l = min(max(l, 0), d.RasterYSize - size)
  gdal.Translate(os.path.join(out, r["name"] + ".tif"), d, srcWin=[c, l, size, size])
