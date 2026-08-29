---
name: dem-comparison
description: Compare two DEMs the RIGHT way - dh/dv/dz analysis (horizontal disparity from correlating hillshades + vertical geodiff), and fix a horizontal misregistration with dem2gcp -> bundle_adjust. Load whenever comparing/aligning two DEMs, hunting a horizontal ground shift geodiff can't see, running dem2gcp, or plotting DEM-to-DEM disparity. Distilled from the CaSSIS work (which did a LOT of this).
---

# Comparing two DEMs: dh / dv / dz

geodiff gives only the VERTICAL difference (dz). A HORIZONTAL misregistration
between two DEMs is nearly INVISIBLE in geodiff on flat/low-relief terrain, yet it
is a real ground-plane error. To SEE and MEASURE it, correlate the two DEMs'
hillshades -> a horizontal disparity (dh east, dv north). This dh/dv is the ground
displacement that geodiff cannot show. This is ESSENTIAL for registration work and
we use it constantly (huge amount of it on CaSSIS).

Canonical CaSSIS references (read for depth - "we did a huge lot"):
  ~/projects/cassis_asp/cassis_dem2gcp_plan.sh   (the dem2gcp -> BA plan + log)
  ~/projects/cassis_asp/cassis_asp_stereo_plan.sh (master stereo/DEM log)
  ~/projects/cassis_asp/cassis_native_res_rationale.sh (why native/image-GSD res)
Example scripts to copy: ~/projects/casa_grande/dem2gcp.sh ,
  ~/projects/cassis_asp/jez_bigctx_dem2gcp.sh . ASP manual (RST in the ASP source
  ~/projects/StereoPipeline/docs/): tools/dem2gcp.rst, tools/disparitydebug.rst,
  correlation.rst (--correlator-mode), tools/bundle_adjust.rst (GCP options).

## Step 1 - put BOTH DEMs on ONE common grid

Regrid + crop both to the SAME projection, SAME extent, and the SAME resolution =
the COARSER of the two grids (never upsample real detail into precision you don't
have). Use gdalwarp:
  - fine -> coarse (downsampling to the coarser grid): `-r average`
  - coarse -> fine (or same-ish res resampling): `-r cubicspline`
  Pick by direction of the resolution change (Oleg's rule).
  gdalwarp -t_srs <one proj> -te <xmin ymin xmax ymax> -tr <res> <res> \
    -r average  in.tif  out_ongrid.tif        # (or -r cubicspline)
Crop the reference to the overlap first if it is much larger. Comparing DEMs on
different grids - or with raw numpy by array index - is meaningless; regrid FIRST.

**-te EXTENT ORDER (CRITICAL, bit us):** `gdalwarp -te` wants **`xmin ymin xmax
ymax`**. This is NOT the same as the `--t_projwin` / `gdal_translate -projwin`
order, which is **`xmin ymax xmax ymin`** (ulx uly lrx lry). `gdal_win.sh` emits
the *projwin* order by DEFAULT (for `mapproject --t_projwin`); pass it a 2nd arg
(`gdal_win.sh dem minmin`) to get the `-te` order. Feeding projwin order to
`gdalwarp -te` swaps ymin/ymax and silently builds a **flipped, south-up grid**
(positive Y pixel size) - which then propagates through dem_mosaic / gdaldem /
image_calc and only shows up later as an upside-down plot. So: for BOTH rasters,
warp with the SAME explicit `-te xmin ymin xmax ymax` and SAME `-tr`, then VERIFY
`gdalinfo` shows identical `Size is`, identical extent, a NEGATIVE Y pixel size
(north-up), and the right proj. For correlation especially (parallel_stereo
--correlator-mode, or dem2gcp's warped->ref disparity) the two inputs must be
pixel-for-pixel on the exact same grid/extent/proj, or the disparity is garbage.
When plotting points over a warped raster, draw in projected coords (imshow with a
UTM `extent`, scatter in easting/northing) and flip any south-up array to north-up
first - never trust raw pixel indices for the geotransform sign.

## Step 2 - hillshade both (ALWAYS gdaldem hillshade)

Always hillshade with `gdaldem hillshade` (as on CaSSIS), typically
`-multidirectional -compute_edges`. NEVER correlate raw elevation - correlate the
hillshades. Terrain that shows as VERTICAL in geodiff shows as HORIZONTAL motion in
the hillshade disparity - that horizontal motion is exactly what we are after.

## Step 3 - correlate the two hillshades -> disparity

parallel_stereo in --correlator-mode (pure image correlation, no cameras) with the
two hillshades as left/right, asp_mgm. On FLAT/low-texture terrain (e.g. Key West
bathy) IP-seeded correlation FAILS ("Number of IPs left after filtering is 5 ...
less than required") - the hillshades are too featureless to seed. FIX (CaSSIS
lesson): --corr-seed-mode 0 with a BOUNDED fixed search sized to the expected shift:
  parallel_stereo --correlator-mode --stereo-algorithm asp_mgm --subpixel-mode 9 \
    --corr-seed-mode 0 --corr-search -30 -30 30 30 \
    left_hs.tif right_hs.tif dispDir/run
Output disparity = dispDir/run-F.tif (filtered) / run-D.tif (raw). Left=warped,
right=ref -> run-F is the warped->ref disparity (what dem2gcp wants).

## Step 4 - split into components and PLOT (dh, dv) + geodiff (dz)

disparitydebug turns the raw disparity into horizontal + vertical rasters:
  disparitydebug dispDir/run-F.tif        # -> run-F-H.tif (dh, px), run-F-V.tif (dv)
("raw disparity" plot = disparitydebug on the disparity, i.e. run-F.tif/run-D.tif.)
Multiply by the grid size to get METERS. Plot dh (east) and dv (north) with a
diverging colormap + colorbar, alongside the geodiff dz. This dh/dv/dz triptych is
THE way to compare two DEMs: dz = vertical, dh/dv = horizontal ground shift geodiff
is blind to. On flat sites a clean nonzero dh/dv with ~0 dz = pure horizontal
misregistration -> fixable by re-doing cameras (below), not by a vertical shift.

## Step 5 - fix a horizontal misregistration: dem2gcp -> bundle_adjust

dem2gcp turns the DEM-to-DEM disparity into GCPs that pin the ground horizontally,
then bundle_adjust bends the cameras to honor them (see casa_grande/dem2gcp.sh):
  dem2gcp --warped-dem our_dem.tif --ref-dem ref_ongrid.tif \
    --warped-to-ref-disparity dispDir/run-F.tif \
    --left-image L.tif --right-image R.tif \
    --left-camera L.json --right-camera R.json \
    --match-file <dense disp match> --gcp-sigma 1 --max-disp <px> \
    --search-len 0 --output-gcp out.gcp
The cameras (L.json/R.json) MUST be the baked `<prefix>-<img>.adjusted_state.json`
from the latest bundle_adjust - dem2gcp has NO `--bundle-adjust-prefix`, so the
baked state IS the only way to give it an adjusted camera (see the asp-photogrammetry
skill). For >2 images use `--image-list`/`--camera-list` + `--match-files-prefix`
(reuse existing sparse matches; dense not required). Land-only GCP: pass
`--gcp-sigma-image <georef land mask>` (GCP where the mask is nodata/<=0 are SKIPPED)
and/or feed a land-masked warped-DEM; set `--max-disp` to reject change/layover
blobs (e.g. keep a real few-px warp, drop >=10 px). `--max-num-gcp` caps the count.
NOTE: weight-image in bundle_adjust masks TRIANGULATED tie points only, NOT GCP -
GCP land-purity must come from dem2gcp itself.
Then bundle_adjust with the latest cameras + these GCPs to fix the horizontal
ground plane (gcp-sigma ~1; --robust-threshold ~1 - check tools/bundle_adjust.rst
for how robust-threshold gates residuals, and whether that magnitude suits your
match noise), typically also with --heights-from-dem <ref> so vertical stays tied.
Carry cameras forward AS-IS (no pc_align in the middle - it spoils horizontal, a
hard CaSSIS lesson). Then RE-MAPPROJECT with the new cameras at the IMAGE-GSD
compromise resolution (not the DEM res) and REDO stereo; re-doing cameras +
mapproject + stereo is what actually removes the misregistration. Re-run Steps 1-4
to confirm dh/dv shrank (and dz stayed good).

## Gotchas
- Common grid FIRST, always (proj + extent + coarser res). cubicspline vs average
  by resample direction (above).
- Hillshade with gdaldem hillshade; correlate hillshades, never raw elevation.
- Horizontal shift is invisible to geodiff on flat ground - that is the whole point.
- No pc_align between BA stages (spoils horizontal); cameras carry forward.
- Mapproject/correlation at image-GSD res, DEM res only for the final point2dem grid.
- Plotting dz/dh/dv/tri-err for a figure or artifact? LOAD the `visual-inspection`
  skill and follow its colormap convention: error → `plasma` (vmin=0), signed dz/dh/dv
  → `RdBu_r` symmetric, nodata black, per-panel full-height right colorbar (matplotlib,
  numeric ticks only), no baked text, tight crop, p95 clamp shared before/after.
  Reusable renderer: `~/projects/cassis_asp/ctx_k19_jitter_scripts/render_panels.py`.
