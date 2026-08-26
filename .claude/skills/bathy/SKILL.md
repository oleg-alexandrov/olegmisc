---
name: bathy
description: Shallow-water bathymetry in ASP - the end-to-end pipeline (NIR water/land threshold + mask, water-surface plane via bathy_plane_calc, stereo with Snell-refraction correction, DEM before/after) and the newer single georeferenced --ortho-bathy-mask option. Load whenever doing bathymetry / underwater terrain / water masking / bathy_plane_calc / bathy_threshold_calc / refraction correction / --left-bathy-mask / --right-bathy-mask / --ortho-bathy-mask in Ames Stereo Pipeline.
---

# Shallow-water bathymetry in ASP

Under clear, shallow, still water with texture on the bottom, camera rays bend at
the water surface (Snell's law). ASP models this at the triangulation stage so
underwater terrain is placed correctly. Sessions: `dg`, `rpc`, `nadirpinhole`
(Earth, WGS84). Works with raw or mapprojected images, with or without bundle
adjustment / alignment.

ASP docs (RST, the source of truth - cite these, they carry the `:numref:`
targets): `docs/examples/bathy.rst` (:numref:`bathy_intro`), tools
`docs/tools/bathy_plane_calc.rst`, `docs/tools/bathy_threshold_calc.rst`,
`docs/bathy_water_masking.rst`, and the stereo options in `docs/stereodefault.rst`.

## The pipeline (green = stereo, NIR = mask)

Use the G band (green, band 3 of WorldView MS) for the underwater STEREO match,
and the N band (NIR1, band 7) to separate water from land (water is uniformly
dark in NIR). Extract a band with `gdal_translate -b N -co compress=lzw -co
TILED=yes ...` (:numref:`bathy_intro`).

1. **Water/land threshold** on the NIR band (:numref:`bathy_thresh`). Two tools:
   `otsu_threshold img_b7.tif` (pure C++, no deps; slightly higher threshold) or
   `bathy_threshold_calc.py --image img_b7.tif --num-samples 1e6` (KDE histogram
   valley; needs a `bathy` conda env; ALWAYS eyeball its plot - it may pick the
   wrong minimum). Other methods (NDWI/Otsu): :numref:`bathy_water_masking`.
2. **Mask** (:numref:`bathy_mask_creation`). `image_calc -c "gt(var_0,T,1,0)"
   -d float32 img_b7.tif -o mask.tif`. Convention: **land = positive value,
   water = non-positive value or nodata**. For a mapprojected/ortho mask keep the
   georef and nodata: `image_calc -c "sign(max(T,var_0)-T)" --output-nodata-value
   -1 ...`. NDWI-style indices (water brighter) flip polarity: use `lt`.
3. **Water-surface plane** with `bathy_plane_calc` (:numref:`bathy_plane_calc`,
   `water_surface`). Three ways to supply the shoreline: `--shapefile` (hand-drawn
   shoreline) + `--dem`; `--mask` (raw-image land/water mask) + `--camera` + `--dem`;
   or `--ortho-mask` (a georeferenced land/water raster) + `--dem`. Outputs a
   4-coefficient plane `a x + b y + c z + d = 0` in a local stereographic frame
   (line 3 = centre lat/lon); water height at the centre is `-d/c`. Also emits
   `--output-inlier-shapefile`. The plane can instead be a georeferenced
   water-surface raster, e.g. from a tide model (:numref:`bathy_plane_img`).
4. **Stereo with bathymetry** (:numref:`bathy_stereo_run`):
   `parallel_stereo -t dg L.tif R.tif L.xml R.xml --left-bathy-mask lmask.tif
   --right-bathy-mask rmask.tif --refraction-index 1.34 --bathy-plane plane.txt
   --stereo-algorithm asp_mgm run/run`, then `point2dem run/run-PC.tif`.
   - **Refraction index: 1.34 saltwater (default choice for marine), 1.333 only
     freshwater** - they differ enough to bias depths; compute exactly with
     `refr_index`. Do NOT default to the freshwater value.
   - Bathy correction happens ONLY at triangulation. To toggle it or change
     plane/refraction, re-run only `stereo_tri` (`--entry-point 5`), or reuse a
     whole prior run with `--prev-run-prefix` (:numref:`bathy_reuse_run`).
   - `--output-cloud-type` = `all` (default) / `bathy` (underwater only) / `topo`
     (above water only; use the topo part for pc_align, water level is variable).

## Single ortho water mask: --ortho-bathy-mask (:numref:`bathy_ortho_mask`)

New option (2026-08): one GEOREFERENCED land/water mask replaces the separate
`--left-bathy-mask` / `--right-bathy-mask`. At triangulation each point is
projected into the mask; if it lands on water, the rays are bent. Because the
decision is on the ground point, it works with raw OR mapprojected input images.
Mutually exclusive with the per-image masks. Still needs `--bathy-plane` +
`--refraction-index` (the plane is the bending surface; the mask only says where).

Build the ortho mask by mapprojecting a raw NIR band onto a DEM, then threshold:
`mapproject dem.tif L_b7.tif L.xml L_b7_map.tif`; `image_calc -c
"sign(max(T,var_0)-T)" --output-nodata-value -1 L_b7_map.tif -o ortho_mask.tif`.

Implementation (for maintenance): the decision lives in VW
`BathyStereoModel::operator()` (`isWaterInOrthoMask(xyz)`: ECEF -> lon/lat ->
mask pixel, nearest-neighbor; outside/nodata -> land; water = value <= 0). ASP
`stereo_tri.cc` loads the mask (must be georeferenced, else throw) and calls
`set_ortho_mask`; the functor passes `do_bathy=true` and the model overrides it
from the uncorrected tri point. `useOrthoBathyMask()` /`doBathy()` in
`asp/Core/Bathymetry.cc`; option + mutual-exclusion + help in
`asp/Core/StereoSettings.cc`; `align_bathy_masks` early-returns in ortho mode.

## Validation (ALWAYS do this - eyeball, do not assume)

- **Deepening check**: `geodiff bathy-DEM nobathy-DEM` should be NEGATIVE (bottom
  pushed down) under water and ~0 on land. Colorize RdBu_r symmetric. This is the
  proof the correction did something physical (:numref:`bathy_intro` effect
  section).
- **Ortho vs per-image**: `geodiff ortho-DEM lr-DEM` should be ~0 everywhere
  (median 0, tight spread), with disagreement only in a thin shoreline ring.
- **Shoreline/plane sanity**: overlay `--output-inlier-shapefile` inliers on the
  mask/DEM - they should sit on the land-water interface (:numref:`bathy_validation`).
- Do a small clip first (`stereo_gui`) before a big run; bathy runs are slow.

## Notes / worked example

SDB subtask (raw + ortho end to end, with a step-by-step HTML and the
--ortho-bathy-mask implementation + 3-way validation):
`~/projects/sdb_2026_08/bathy_ortho_mask_notes.sh` (child of
`sdb_2026_08_notes.sh`; peer `bathy_plan.sh`). Regression test dirs:
`~/projects/StereoPipelineTest/ssDG_alignAffEpp_seedMode1_mapProj0_bathy` (the
canonical illustrative run.sh), `ss_bathy_plane_*`. Complements the
asp-photogrammetry, visual-inspection, and dem-comparison skills.
