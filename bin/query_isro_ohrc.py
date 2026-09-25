#!/usr/bin/env python3
# Query ISRO Chandrayaan-2 OHRC footprint shapefiles by bounding box.
# Pure Python standard library (no geopandas/shapely required).
#
# Usage:
#   python3 query_isro_ohrc.py --stefano-box
#   python3 query_isro_ohrc.py --box -12000 -13000 -10000 -11000
#   python3 query_isro_ohrc.py --lonlat -47.5 -89.55 -37.5 -89.40
#   python3 query_isro_ohrc.py --stefano-box --list-only > ohrc_candidates.txt

import argparse
import math
import os
import sys

# Moon 2015 sphere radius in meters
MOON_RADIUS = 1737400.0

def lonlat_to_xy(lon_deg, lat_deg):
  # Project to South Polar Stereographic (Moon 2015 sphere)
  # lat_0 = -90, lon_0 = 0
  lat_rad = math.radians(lat_deg)
  lon_rad = math.radians(lon_deg)
  c = (math.pi / 2.0) + lat_rad
  k = 2.0 * MOON_RADIUS / (1.0 + math.cos(c))
  x = k * math.sin(c) * math.sin(lon_rad)
  y = -k * math.sin(c) * math.cos(lon_rad)
  return x, y

def point_in_polygon(px, py, poly):
  inside = False
  n = len(poly)
  for i in range(n):
    x1, y1 = poly[i]
    x2, y2 = poly[(i + 1) % n]
    if ((y1 > py) != (y2 > py)) and (px < (x2 - x1) * (py - y1) / (y2 - y1 + 1e-15) + x1):
      inside = not inside
  return inside

def line_intersect(p1, p2, p3, p4):
  def ccw(A, B, C):
    return (C[1] - A[1]) * (B[0] - A[0]) > (B[1] - A[1]) * (C[0] - A[0])
  return (ccw(p1, p3, p4) != ccw(p2, p3, p4)) and (ccw(p1, p2, p3) != ccw(p1, p2, p4))

def polygons_intersect(p1, p2):
  for x, y in p1:
    if point_in_polygon(x, y, p2):
      return True
  for x, y in p2:
    if point_in_polygon(x, y, p1):
      return True
  n1, n2 = len(p1), len(p2)
  for i in range(n1):
    for j in range(n2):
      if line_intersect(p1[i], p1[(i + 1) % n1], p2[j], p2[(j + 1) % n2]):
        return True
  return False

def read_dbf(dbf_path):
  if not os.path.exists(dbf_path):
    sys.exit(f"Error: DBF file not found: {dbf_path}")
  with open(dbf_path, "rb") as fh:
    data = fh.read()
  nrec = int.from_bytes(data[4:8], "little")
  hlen = int.from_bytes(data[8:10], "little")
  rlen = int.from_bytes(data[10:12], "little")
  fields, off = [], 32
  while data[off] != 0x0D:
    name = data[off:off + 11].split(b"\x00")[0].decode("ascii", "replace")
    size = data[off + 16]
    fields.append((name, size))
    off += 32
  records = []
  for r in range(nrec):
    pos = hlen + r * rlen + 1
    row = {}
    for nm, sz in fields:
      row[nm] = data[pos:pos + sz].decode("ascii", "replace").strip()
      pos += sz
    try:
      # In ISRO South Pole shapefile:
      # UL_LON is Easting (X), UL_LAT is Northing (Y) in meters
      ul = (float(row["UL_LON"]), float(row["UL_LAT"]))
      ur = (float(row["UR_LON"]), float(row["UR_LAT"]))
      br = (float(row["BR_LON"]), float(row["BR_LAT"]))
      bl = (float(row["BL_LON"]), float(row["BL_LAT"]))
      row["poly"] = [ul, ur, br, bl]
      records.append(row)
    except Exception:
      pass
  return records

def main():
  parser = argparse.ArgumentParser(description="Spatial query tool for ISRO OHRC footprints.")
  parser.add_argument("--dbf", default="shapefiles/ohr_r1_r11_shape_ver4/ch2_ohr_cal_sp.dbf",
                      help="Path to shapefile DBF (default: shapefiles/ohr_r1_r11_shape_ver4/ch2_ohr_cal_sp.dbf)")
  parser.add_argument("--stefano-box", action="store_true",
                      help="Use Stefano 2 km SfS DEM box: X in [-12000, -10000], Y in [-13000, -11000]")
  parser.add_argument("--box", nargs=4, type=float, metavar=("XMIN", "YMIN", "XMAX", "YMAX"),
                      help="Cartesian bounding box in South Polar Stereographic meters")
  parser.add_argument("--lonlat", nargs=4, type=float, metavar=("MINLON", "MINLAT", "MAXLON", "MAXLAT"),
                      help="Geographic bounding box in degrees")
  parser.add_argument("--list-only", action="store_true",
                      help="Print only the zip filenames (for piping to fetch scripts)")
  parser.add_argument("--out-list", help="Save matching zip filenames to a text file")
  args = parser.parse_args()

  # Determine query box polygon
  if args.stefano_box:
    xmin, ymin, xmax, ymax = -12000.0, -13000.0, -10000.0, -11000.0
  elif args.box:
    xmin, ymin, xmax, ymax = args.box
  elif args.lonlat:
    minlon, minlat, maxlon, maxlat = args.lonlat
    x1, y1 = lonlat_to_xy(minlon, minlat)
    x2, y2 = lonlat_to_xy(maxlon, maxlat)
    x3, y3 = lonlat_to_xy(minlon, maxlat)
    x4, y4 = lonlat_to_xy(maxlon, minlat)
    xs = [x1, x2, x3, x4]
    ys = [y1, y2, y3, y4]
    xmin, xmax = min(xs), max(xs)
    ymin, ymax = min(ys), max(ys)
  else:
    sys.exit("Error: Must specify one of --stefano-box, --box, or --lonlat")

  query_poly = [
    (xmin, ymin),
    (xmax, ymin),
    (xmax, ymax),
    (xmin, ymax)
  ]

  # Resolve DBF path relative to script directory or standard ISIS/project paths
  dbf_path = args.dbf
  if not os.path.isabs(dbf_path) and not os.path.exists(dbf_path):
    search_dirs = [
      os.path.dirname(os.path.abspath(__file__)),
      os.path.expanduser("~/projects/ohrc_lronac_align"),
      os.path.expanduser("~/projects/sfs_BCU2314-BDU1224-MM"),
      os.path.join(os.environ.get("ISISDATA", ""), "chandrayaan2"),
      "/nobackupnfs1/oalexan1/projects/isis3data/chandrayaan2",
      "/nobackupp19/oalexan1/projects/sfs_BCU2314-BDU1224-MM",
    ]
    for d in search_dirs:
      if not d:
        continue
      cand = os.path.join(d, dbf_path)
      if os.path.exists(cand):
        dbf_path = cand
        break
      cand_sub = os.path.join(d, "shapefiles", "ohr_r1_r11_shape_ver4", os.path.basename(dbf_path))
      if os.path.exists(cand_sub):
        dbf_path = cand_sub
        break

  records = read_dbf(dbf_path)
  matches = []
  for r in records:
    if polygons_intersect(query_poly, r["poly"]):
      matches.append(r)

  # Sort by observation start time
  matches.sort(key=lambda x: x.get("OBS_ST_TIME", ""))

  if args.list_only:
    for m in matches:
      print(m.get("DOWNLOAD") or (m.get("PRODUCT_ID") + ".zip"))
  else:
    print(f"Loaded {len(records)} footprints from {os.path.basename(dbf_path)}")
    print(f"Query box: X[{xmin:.1f}, {xmax:.1f}], Y[{ymin:.1f}, {ymax:.1f}]")
    print(f"Total matching footprints: {len(matches)}\n")
    print(f"{'PRODUCT_ID':<45} | {'START_TIME':<20} | {'TDI':<6} | {'DOWNLOAD'}")
    print("-" * 110)
    for m in matches:
      pid = m.get("PRODUCT_ID", "")
      st = m.get("OBS_ST_TIME", "")
      tdi = m.get("TDI", "")
      dl = m.get("DOWNLOAD", pid + ".zip")
      print(f"{pid:<45} | {st:<20} | {tdi:<6} | {dl}")

  if args.out_list:
    with open(args.out_list, "w") as fh:
      for m in matches:
        dl = m.get("DOWNLOAD") or (m.get("PRODUCT_ID") + ".zip")
        fh.write(dl + "\n")
    print(f"\nWrote {len(matches)} product filenames to {args.out_list}")

if __name__ == "__main__":
  main()
