#!/usr/bin/env python3
# eyeball_overlay.py <dem1> <dem2> <out.png> [label1] [label2] [grid_dem]
#
# The mandatory alignment check after any pc_align (or any time two DEMs must be compared).
# Judge alignment by the HILLSHADE OVERLAY, never by a vertical diff (a vertical diff is blind
# to horizontal shift and rotation on low-relief terrain).
#
# This tool does the regridding ITSELF - it never assumes the caller put the two DEMs on the
# same grid. Both inputs are gdalwarp'd to ONE common grid: same projection, same extent, same
# pixel size (taken from grid_dem, default dem1), with -r cubicspline. Then each is hillshaded
# (gdaldem -multidirectional -compute_edges) and shown side by side plus a red/green overlay
# (dem1 = red, dem2 = green; yellow = aligned; red/green fringes = translation, fringes that
# flip direction across the frame = rotation).
#
# Two rasters on different grids or projections CANNOT be eyeballed by loading them raw - numpy
# aligns by row/col index, not ground coordinates. Warp to a shared grid first, which is exactly
# what this tool guarantees.
import subprocess, os, sys, re, numpy as np
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from osgeo import gdal; gdal.UseExceptions()

if len(sys.argv) < 4:
    sys.exit("usage: eyeball_overlay.py <dem1> <dem2> <out.png> [label1] [label2] [grid_dem]")
d1, d2, out = sys.argv[1:4]
lab1 = sys.argv[4] if len(sys.argv) > 4 else "dem1"
lab2 = sys.argv[5] if len(sys.argv) > 5 else "dem2"
grid = sys.argv[6] if len(sys.argv) > 6 else d1
tmp = os.path.dirname(os.path.abspath(out)) or "."

srs = subprocess.check_output(["gdalsrsinfo", "-o", "proj4", grid]).decode().strip()
info = subprocess.check_output(["gdalinfo", grid]).decode()
ll = re.search(r"Lower Left\s+\(\s*([-\d.]+),\s*([-\d.]+)\)", info)
ur = re.search(r"Upper Right\s+\(\s*([-\d.]+),\s*([-\d.]+)\)", info)
ps = re.search(r"Pixel Size = \(\s*([-\d.eE]+),\s*([-\d.eE]+)\)", info)
te = ["-te", ll.group(1), ll.group(2), ur.group(1), ur.group(2)]
tr = abs(float(ps.group(1)))
trarg = ["-tr", str(tr), str(tr)]

def warp(src, dst):
    subprocess.run(["gdalwarp", "-q", "-overwrite", "-t_srs", srs, *te, *trarg,
                    "-r", "cubicspline", src, dst], check=True)
def hs(dem, o):
    subprocess.run(["gdaldem", "hillshade", "-multidirectional", "-compute_edges", dem, o],
                   check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
def load(f):
    ds = gdal.Open(f); a = ds.GetRasterBand(1).ReadAsArray().astype(float); a[a == 0] = np.nan; return a
def norm(a):
    v = a[np.isfinite(a)]
    lo, hi = np.percentile(v, 2), np.percentile(v, 98)
    b = np.clip((a - lo) / (hi - lo), 0, 1); b[~np.isfinite(a)] = 0; return b

warp(d1, f"{tmp}/_eo1.tif"); warp(d2, f"{tmp}/_eo2.tif")
hs(f"{tmp}/_eo1.tif", f"{tmp}/_eo1h.tif"); hs(f"{tmp}/_eo2.tif", f"{tmp}/_eo2h.tif")
a = norm(load(f"{tmp}/_eo1h.tif")); b = norm(load(f"{tmp}/_eo2h.tif"))
rgb = np.dstack([a, b, np.zeros_like(a)])
fig, ax = plt.subplots(1, 3, figsize=(14, 8.6))
ax[0].imshow(a, cmap="gray"); ax[0].set_title(lab1, fontsize=11)
ax[1].imshow(b, cmap="gray"); ax[1].set_title(lab2, fontsize=11)
ax[2].imshow(rgb); ax[2].set_title(f"Overlay: {lab1}=red, {lab2}=green\n(yellow=aligned, fringes=shift/rotation)", fontsize=10)
for x in ax: x.axis("off")
fig.tight_layout(); fig.savefig(out, dpi=120, bbox_inches="tight")
for f in ["_eo1.tif", "_eo2.tif", "_eo1h.tif", "_eo2h.tif"]:
    p = f"{tmp}/{f}"
    if os.path.exists(p): os.remove(p)
print("wrote", out)
