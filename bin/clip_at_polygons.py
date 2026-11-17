#!/usr/bin/env python3
# Crop rasters around polygons from a vector file (GeoPackage, shapefile, etc.).
# For each selected feature, write a square crop centered on the polygon's bounding
# box, of side max(--size, polygon extent + 2 * --pad) pixels, clamped to the raster.
# All rasters must share the same grid. Outputs: <outDir>/<raster_basename>_fid<N>.tif.

import argparse
import os
import sys

from osgeo import gdal, ogr, osr

gdal.UseExceptions()
ogr.UseExceptions()


def parse_fids(text):
  fids = []
  for part in text.split(","):
    part = part.strip()
    if not part:
      continue
    if "-" in part:
      a, b = part.split("-")
      fids.extend(range(int(a), int(b) + 1))
    else:
      fids.append(int(part))
  return fids


def main():
  p = argparse.ArgumentParser(description=__doc__)
  p.add_argument("--vector", required=True, help="Vector file with polygons.")
  p.add_argument("--fids", default="",
                 help="Comma-separated fids or ranges (e.g. 1,6,12-22). Default: all.")
  p.add_argument("--size", type=int, default=1000, help="Minimum crop side, in pixels.")
  p.add_argument("--pad", type=int, default=100,
                 help="Pixels of margin around a polygon larger than --size.")
  p.add_argument("--out-dir", required=True)
  p.add_argument("rasters", nargs="+", help="Rasters on the same grid.")
  a = p.parse_args()

  ds0 = gdal.Open(a.rasters[0])
  gt = ds0.GetGeoTransform()
  nx, ny = ds0.RasterXSize, ds0.RasterYSize
  rsrs = osr.SpatialReference(wkt=ds0.GetProjection())
  rsrs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
  for r in a.rasters[1:]:
    d = gdal.Open(r)
    if d.GetGeoTransform() != gt or (d.RasterXSize, d.RasterYSize) != (nx, ny):
      sys.exit("Raster grid differs from the first one: " + r)

  vds = ogr.Open(a.vector)
  layer = vds.GetLayer(0)
  vsrs = layer.GetSpatialRef()
  xform = None
  if vsrs is not None:
    vsrs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    if not vsrs.IsSame(rsrs):
      xform = osr.CoordinateTransformation(vsrs, rsrs)

  wanted = set(parse_fids(a.fids)) if a.fids else None
  os.makedirs(a.out_dir, exist_ok=True)
  found = set()
  for feat in layer:
    fid = feat.GetFID()
    if wanted is not None and fid not in wanted:
      continue
    found.add(fid)
    geom = feat.GetGeometryRef().Clone()
    if xform is not None:
      geom.Transform(xform)
    minx, maxx, miny, maxy = geom.GetEnvelope()
    # Pixel bounds of the polygon (north-up grid).
    c0 = (minx - gt[0]) / gt[1]
    c1 = (maxx - gt[0]) / gt[1]
    r0 = (maxy - gt[3]) / gt[5]
    r1 = (miny - gt[3]) / gt[5]
    side = int(max(a.size, c1 - c0 + 2 * a.pad, r1 - r0 + 2 * a.pad))
    cc, rc = 0.5 * (c0 + c1), 0.5 * (r0 + r1)
    xoff = int(round(cc - side / 2.0))
    yoff = int(round(rc - side / 2.0))
    xoff = max(0, min(xoff, nx - side))
    yoff = max(0, min(yoff, ny - side))
    w, h = min(side, nx), min(side, ny)
    if c1 < 0 or c0 > nx or r1 < 0 or r0 > ny:
      print("fid %d: outside the raster, skipped" % fid)
      continue
    for r in a.rasters:
      base = os.path.splitext(os.path.basename(r))[0]
      out = os.path.join(a.out_dir, "%s_fid%d.tif" % (base, fid))
      gdal.Translate(out, r, srcWin=[xoff, yoff, w, h],
                     creationOptions=["COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER"])
    print("fid %d: srcwin %d %d %d %d" % (fid, xoff, yoff, w, h))

  if wanted is not None and wanted - found:
    print("fids not found: " + " ".join(str(f) for f in sorted(wanted - found)))


if __name__ == "__main__":
  main()
