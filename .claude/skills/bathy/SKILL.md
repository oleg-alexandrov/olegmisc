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
mask pixel, nearest-neighbor; outside -> land; water = INVALID pixel). The mask
is read in `stereo_tri.cc` via `vw::read_bathy_mask` (SAME reader as the per-image
masks), so water = non-positive value OR nodata, matching `areMasked`'s
classification exactly (the only inherent difference: ortho samples one ground
point vs the per-image AND of both views). `stereo_tri.cc` also throws if the
mask has no georef and calls `set_ortho_mask`; the functor passes `do_bathy=true`
and the model overrides it from the uncorrected tri point. `useOrthoBathyMask()`
/`doBathy()` in `asp/Core/Bathymetry.cc`; option + mutual-exclusion + help in
`asp/Core/StereoSettings.cc`; `align_bathy_masks` early-returns in ortho mode.

Getting ortho and per-image masks to AGREE (they should, to ~mm): (1) read the
ortho mask through `read_bathy_mask` so nodata counts as water, same as per-image;
(2) more important, get the WATER PLANE right - a plane fit to too few / poorly
distributed inliers TILTS and places the surface tens of metres off at the far
edges, inflating depths and any mask-boundary disagreement into metres. Fit the
plane from the DENSE mask boundary (`--mask`+`--camera` or `--ortho-mask`, big
`--num-samples`) with an inlier `--outlier-threshold` ~1 m on rough imagery, and
CHECK the inliers cover ALL shorelines (islands too), not just one shore, else the
plane is skewed. Handy flags: `--save-shapefiles-as-polygons`,
`--mask-boundary-shapefile` (all pre-RANSAC points, then exits),
`--output-outlier-shapefile`. Plot the inlier points as a fine scatter, not a
polygon (RANSAC-order polygon = spaghetti).

## Validation (ALWAYS do this - eyeball, do not assume)

- **Deepening check**: `geodiff bathy-DEM nobathy-DEM` should be NEGATIVE (bottom
  pushed down) under water and ~0 on land. Colorize RdBu_r symmetric. This is the
  proof the correction did something physical (:numref:`bathy_intro` effect
  section).
- **Ortho vs per-image**: `geodiff ortho-DEM lr-DEM` should be ~0 everywhere
  (median 0, tight spread), with disagreement only in a thin shoreline ring.
- **Inspect BOTH shapefiles for the plane fit - this is the #1 way the plane goes
  wrong.** Always look at (a) the INPUT points to `bathy_plane_calc` (the shoreline
  shapefile, or `--mask-boundary-shapefile` for the mask/ortho-mask case = all
  pre-RANSAC boundary points) AND (b) the `--output-inlier-shapefile`, overlaid on
  the DEM. **The inliers MUST cover the shoreline REPRESENTATIVELY** - every stretch
  of coast, every island, not just one shore. If the inliers bunch on one side while
  other shorelines are all outliers, the plane is fit to a subset and is TILTED /
  skewed: it will sit metres off at the far edges and create a false water->land
  cliff (and inflate depths). Symptom seen in practice: a 9-vertex hand-drawn
  shoreline gave 4 inliers all on the bottom shore -> plane tilted ~27 m across a
  4 km scene; switching to dense `--mask` boundary sampling with a sane threshold
  gave 5754 inliers spanning all shores incl. islands -> plane flat. Plot inliers
  as a FINE SCATTER, not a polygon (RANSAC-order polygon = spaghetti).
- **Set `--outlier-threshold` comparable to the DEM GSD, not tiny.** The threshold
  is a distance in metres from a boundary point to the plane; a boundary point's
  height is looked up in the DEM by interpolation, so its noise floor is on the
  order of the DEM ground sample distance (and the terrain slope near shore). If the
  DEM GSD is 4 m, a 0.1 m threshold is ridiculous - it rejects almost everything and
  keeps a biased few. Rule of thumb: threshold ~ a small multiple of the GSD (e.g.
  ~1 m for ~1-2 m data, larger for coarse DEMs, and larger still for rough imagery).
  If the inliers do not representatively cover the shoreline, the threshold is wrong
  (usually too small) - raise it until they do, then confirm the plane is near-flat.
- Do a small clip first (`stereo_gui`) before a big run; bathy runs are slow.

## Notes / worked example

SDB subtask (raw + ortho end to end, with a step-by-step HTML and the
--ortho-bathy-mask implementation + 3-way validation):
`~/projects/sdb_2026_08/bathy_ortho_mask_notes.sh` (child of
`sdb_2026_08_notes.sh`; peer `bathy_plan.sh`). Regression test dirs:
`~/projects/StereoPipelineTest/ssDG_alignAffEpp_seedMode1_mapProj0_bathy` (the
canonical illustrative run.sh), `ss_bathy_plane_*`. Complements the
asp-photogrammetry, visual-inspection, and dem-comparison skills.

## Aerial / frame-camera bathy vs linescan (WorldView) bathy - DIFFERENT problem

WorldView/linescan bathy is ONE big swath: you can mask water early (per-image NIR mask)
and stereo the single pair - masking never disconnects anything. An AERIAL DRONE BLOCK is
many small overlapping FRAMES that must be ASSEMBLED (bundle adjustment ties the block, then
per-pair stereo, then dem_mosaic). The order matters and masking is the trap:

- **MASK LATE, assemble first.** If you apply a per-frame water mask BEFORE the block SfM,
  offshore/water-heavy frames go nearly EMPTY (no interest points) and drop out - you lose the
  block's connectivity and coverage and are "left with nothing". So run the bundle on the FULL
  (unmasked, or land-dominated) green frames first - the Parrish approach: no mask, a robust
  cost down-weights the few water matches, LAND dominates and ties the block. Get the cameras,
  THEN bring in the water mask + refraction only at the triangulation stage (bathy) as a
  refinement. Linescan single-swath does not have this disconnection problem.
- The aerial pipeline is: bands (green stereo / NIR mask) -> EOP->tsai cameras (or cam_gen) ->
  parallel_bundle_adjust (unmasked, robust, cam-pos-uncertainty anchor) -> select pairs by
  convergence -> parallel_stereo per pair -> point2dem -> dem_mosaic -> (LATER) bathy: NIR
  water mask + `--bathy-plane` + refraction at triangulation. Worked example + the honest
  step-by-step: `~/projects/sdb_2026_08/aerial_notes.sh` (RCD30 Florida block, 2026-08).
- Extra aerial gotchas (see aerial_notes.sh): frames are RAW but often carry a spurious CRS
  tag (strip it); the EOP is photogrammetric Omega/Phi/Kappa (NOT cam_gen roll/pitch/yaw - a
  ~90 deg trap) and its height may be ORTHOMETRIC (a ~geoid-sized Z bias vs an ellipsoidal
  reference - catch it with a prior-DEM eval + pc_align); water areas of the stereo DEM are
  unreliable until the bathy step. Validate absolute placement against a rough prior DEM
  (Copernicus) via the dem-comparison + pc-align skills.
- **Concrete aerial-block bathy pipeline (SDB 2026-08):** after the land-only bundle + per-pair
  stereo + DEM mosaic (no bathy):
  (1) mapproject each frame's GREEN and NIR band with the adjusted cameras onto the STEREO DEM
      (hole-filled), `dem_mosaic` -> green ortho mosaic + NIR ortho mosaic (DN preserved).
  (2) Otsu-threshold the NIR ortho -> ONE georef water mask, `image_calc -c "sign(max(T,var_0)-T)"
      --output-nodata-value -1` (land positive, water nodata). This SINGLE mask serves BOTH tools.
  (3) ONE GLOBAL plane: `bathy_plane_calc --ortho-mask mask.tif --dem stereoDEM.tif --bathy-plane
      plane.txt` (not a per-frame plane).
  (4) Per pair, RE-TRIANGULATE cheaply reusing the existing correlation:
      `parallel_stereo ... --prev-run-prefix stereo/<pair>/run --ortho-bathy-mask mask.tif
       --bathy-plane plane.txt --refraction-index 1.34 <green L/R> <cam L/R> stereo_bathy/<pair>/run`
      (bathy acts ONLY at triangulation, so no need to redo correlation - use `--prev-run-prefix`
      or `--entry-point 5`). `point2dem` at the 4x-image-GSD DEM res (NOT the coarse prior's grid).
  (5) `dem_mosaic` the bathy DEMs; VALIDATE by `geodiff no_bathy_DEM bathy_DEM` -> must be POSITIVE
      (water pushed DOWN/deeper) under water, ~0 on land; hillshade+colorize both side by side.
  Use the same `--ortho-bathy-mask` for all pairs (one mask), far simpler than per-frame left/right
  masks for a many-frame block. Do NOT go to the external prior DEM for point2dem - that grid is only
  for alignment/validation.
