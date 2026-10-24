---
name: sfs-lola-prep
description: >-
  Preparing initial and reference LOLA DEMs for Shape-from-Shading (SfS) - Barker LOLA products (5 mpp vs 10 mpp coverage and extents), integer pixel center constraints, the 0.5 m half-integer bounding box offset for sfs_blend, and the canonical make_ref_dem.sh regridding workflow. Load before regridding, clipping, or preparing LOLA DEMs for lunar south polar SfS.
---

# LOLA Reference Terrain Preparation for Shape-from-Shading (SfS)

This skill documents how to select, crop, and regrid reference LOLA Digital Elevation Models (DEMs) for Lunar South Pole Shape-from-Shading (SfS) pipelines in Ames Stereo Pipeline (ASP).

The prepared LOLA DEM serves three roles in the SfS pipeline:
1. **Initial terrain**: Starting surface for photoclinometry (`parallel_sfs`).
2. **Ground constraint**: Height constraint in bundle adjustment (`--heights-from-dem`) and linescan jitter refinement.
3. **Shadow fill**: Reference surface blended into permanently shadowed regions with `sfs_blend`.

---

## 1. Barker LOLA Products and Coverage

The primary source for high-resolution lunar south polar LOLA topography is the work of Michael K. Barker et al. (NASA GSFC / PGDA).

### Barker 10 mpp DEM (PGDA Product 90)
* **Dataset**: `LDEM_83S_10MPP_ADJ.TIF`
* **Source**: [PGDA Product 90](https://pgda.gsfc.nasa.gov/products/90) (`https://pgda.gsfc.nasa.gov/data/LOLA_20mpp/LDEM_83S_10MPP_ADJ.TIF`)
* **Coverage**: Polewards of 83°S to 90°S across all longitudes.
* **Projection**: South Polar Stereographic ($lat_0 = -90^\circ, lon_0 = 0^\circ, R = 1737400\text{ m}$).
* **Extent**: $X \in [-216400, 216400]\text{ m}$, $Y \in [-216400, 216400]\text{ m}$ ($43280 \times 43280$ pixels).
* **Format**: Cloud-Optimized GeoTIFF (COG), 5.1 GB, DEFLATE compressed, tiled ($512 \times 512$) with overviews.
* **When to use**: **Default choice for south polar SfS sites.** It covers all landing sites and candidate regions between 83°S and 90°S without gaps.

### Barker 5 mpp Mosaic (PGDA Product 81)
* **Dataset**: `ldem_87s_5mpp.tif`
* **Source**: [PGDA Product 81](https://pgda.gsfc.nasa.gov/products/81) (`https://pgda.gsfc.nasa.gov/data/LOLA_5mpp/87S/ldem_87s_5mpp.tif`)
* **Coverage**: Strictly 87°S to 90°S.
* **Extent**: $X \in [-100000, 100000]\text{ m}$, $Y \in [-100000, 100000]\text{ m}$ ($40000 \times 40000$ pixels).
* **When to use**: Only when all target sites and any required anchor pads lie entirely within 3° of the pole (inside the 100 km radius box). Any site north of 87°S falls outside this DEM.

### Individual 5 mpp Landing Site DEMs (Barker 2021)
* **Source**: [PGDA LOLA_5mpp](https://pgda.gsfc.nasa.gov/data/LOLA_5mpp/)
* **Coverage**: Isolated patches for specific landing sites:
  * `Site01`: Connecting ridge ($X \in [-19, -3]\text{ km}, Y \in [-20, -4]\text{ km}$)
  * `Site04`: Shackleton rim ($X \in [-9, 7]\text{ km}, Y \in [-15, 1]\text{ km}$)
  * `Site07`: Peak near Shackleton ($X \in [22, 38]\text{ km}, Y \in [-28, -12]\text{ km}$)
  * `Site11`: de Gerlache rim ($X \in [-45, -29]\text{ km}, Y \in [7, 23]\text{ km}$)
  * `Site20`: Leibnitz beta plateau ($X \in [65, 81]\text{ km}, Y \in [110, 126]\text{ km}$)
  * `Site23`: Malapert massif ($X \in [-11, 10]\text{ km}, Y \in [111, 132]\text{ km}$)
* **When to use**: Only when working strictly within one of these specific small patches. For broad or multi-site studies, use the contiguous 10 mpp mosaic.

### 20 mpp Polar Product
* **Dataset**: `LDEM_80S_20MPP_ADJ.TIF` or PDS `LDEM_80S_20M.IMG`
* **Coverage**: 80°S to 90°S.
* **When to use**: Regional context or very wide anchor DEMs (e.g. 50-100 km pads) where 10 mpp is unnecessarily heavy.

---

## 2. Integer Pixel Centers and the Half-Integer ($n + 0.5$) Bound Rule

In ASP, Shape-from-Shading and orthorectification require an exact 1:1 pixel correspondence across tools:

1. **Integer Ground Centers**:
   When `mapproject` projects an image at 1 m/pixel, it evaluates pixels centered at integer grid coordinates ($x, y \in \mathbb{Z}$).
2. **Half-Integer Bounding Box ($n + 0.5$)**:
   Since a 1 m pixel centered at integer $x$ spans the interval $[x - 0.5, x + 0.5]$, the outer bounding box coordinates ($x_{\min}, y_{\min}, x_{\max}, y_{\max}$) must have a fractional part of `0.5`.
3. **The `sfs_blend` Failure Mode**:
   If the initial reference DEM bounds are not snapped to $n + 0.5$, its pixel grid will be shifted by a subpixel offset relative to the mapprojected images. Later, `sfs_blend` fails because it checks for strictly identical pixel grids between the SfS DEM, the reference LOLA DEM, and the maximally lit mosaic.

### Coordinate Snapping Formula

Given a site center coordinate $(x_c, y_c)$ in projected meters and a desired half-width/radius $R$ (e.g. $R = 2500\text{ m}$ for a $5 \times 5\text{ km}$ box):

```python
# Snap min bound so its center is closest to xc - R, with fractional part exactly 0.5:
xmin = round(xc - R - 0.5) + 0.5
xmax = xmin + 2 * R

ymin = round(yc - R - 0.5) + 0.5
ymax = ymin + 2 * R
```

Properties of this snapping:
* $x_{\max} - x_{\min} = 2R$ (an exact integer pixel count).
* All four corners have `.5` fractional coordinates.
* Every pixel center in the resulting raster is an exact integer: $x_{\text{pixel\_center}} = x_{\min} + 0.5 + k \in \mathbb{Z}$.
* The center of the box deviates from $(x_c, y_c)$ by at most 0.5 m.

---

## 3. Projection Standard

Lunar south polar processing uses South Polar Stereographic:
```bash
proj="+proj=stere +lat_0=-90 +lon_0=0 +R=1737400 +units=m +no_defs"
```

---

## 4. Canonical Regridding Workflows

### Method A: Canonical Script `make_ref_dem.sh`
Use the reference script in `~/projects/SfsPipeline/bin/make_ref_dem.sh`:
```bash
~/projects/SfsPipeline/bin/make_ref_dem.sh \
  "$src_dem" "$out_dem" "$xmin $ymin $xmax $ymax" 1 "$work_dir" "$proj" [blur_sigma]
```
* Uses `-r cubicspline`.
* Enforces ASP-preferred $256 \times 256$ internal block tiling (`-co BLOCKXSIZE=256 -co BLOCKYSIZE=256`).
* When `blur_sigma` (e.g. 2) is provided, also generates `<out_dem>_blur.tif` for mapprojection drape.

**ASPROOT and the GDAL/PROJ data on the Mac.** The script defaults `ASPROOT` to
`~/projects/BinaryBuilder/StereoPipeline` and points `GDAL_DATA`/`PROJ_DATA`/`PROJ_LIB`
at `$ASPROOT/share`. That full ASP install exists on pfe and the Linux boxes, but NOT on
the Mac mini. On the Mac the SfS/ASP pipeline is installed separately from the deps, and
the `asp_deps` conda env carries everything EXCEPT ASP itself: a complete GDAL (with
`gdalwarp` and `share/proj/proj.db`) but none of the ASP binaries. With the stale default
`ASPROOT`, `gdalwarp -t_srs` fails with "PROJ: proj_create: Cannot find proj.db". Fix on
the Mac: export `ASPROOT` to the active deps env first, e.g.
```bash
export ASPROOT=/Users/oalexan1/anaconda3/envs/asp_deps
```
so the script's `$ASPROOT/bin` gdalwarp and `$ASPROOT/share/{gdal,proj}` resolve. This is
fine as long as no `blur_sigma` is passed (blur calls `dem_mosaic`, an ASP tool absent
from the deps env). For a blurred drape on the Mac, run against a real ASP install.

### Method B: Direct `gdalwarp` Command
If running standalone:
```bash
proj="+proj=stere +lat_0=-90 +lon_0=0 +R=1737400 +units=m +no_defs"

gdalwarp -overwrite -r cubicspline -tr 1 1 -t_srs "$proj" \
  -co COMPRESSION=LZW -co TILED=yes -co INTERLEAVE=BAND \
  -co BLOCKXSIZE=256 -co BLOCKYSIZE=256 -co BIGTIFF=yes \
  -te $xmin $ymin $xmax $ymax \
  "$src_dem" "$out_dem"
```

*Always use cubic spline (`-r cubicspline`), never nearest neighbor or bilinear, to avoid introducing gridding steps into the photoclinometry solver.*
