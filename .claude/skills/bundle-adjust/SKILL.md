---
name: bundle-adjust
description: bundle_adjust match-file mechanics - the --mapprojected-data workflow (find matches on mapprojected images, transfer to raw), the THREE match sets it writes (mapproj / raw-unprojected / clean) and their naming, getting DENSE unprojected raw matches via --num-matches-from-disparity, ip-detect-method 0 vs 1, and the generous-outlier-removal rule when cameras are bad. Load before running bundle_adjust with mapprojected data, choosing which match file to feed dem2gcp/jitter_solve, or debugging why clean matches are too few.
---

# bundle_adjust match files (esp. --mapprojected-data)

## --mapprojected-data: match on the mapproj images, transfer to raw

When the two images are very different in perspective/scale (historical KH-7/KH-9,
big elevation range), match on the MAPPROJECTED images (which look similar because
both are draped on the same DEM), then transfer the matches to the raw images:
```
bundle_adjust L.tif R.tif L.json R.json -t csm \
  --mapprojected-data "L_map.tif R_map.tif dem.tif" \
  --ip-per-tile 1000 --matches-per-tile 1000 --ip-detect-method 1 \
  --min-matches 0 --num-iterations 0 -o out/run
```
The mapproj images must carry the metadata mapproject writes (INPUT_IMAGE_FILE,
CAMERA_MODEL_TYPE, DEM_FILE) - gdalinfo to confirm. Mapproject all images at ONE
--tr near the image GSD.

## THE THREE MATCH SETS (know which to use)

--mapprojected-data writes, in this order:
1. **mapproj matches** `out/run-<Lmap>__<Rmap>.match` (named by the MAPPROJ image
   basenames) - matches found ON the mapprojected images. Plot THESE on the mapproj
   images to inspect ip coverage.
2. **raw / unprojected matches** `out/run-<L>__<R>.match` (named by the RAW image
   basenames) - the SAME matches transferred to raw-image pixel coords. **This is
   the set to feed dem2gcp and jitter_solve** (`--match-files-prefix out/run` loads
   this one, since it matches the raw image names).
3. **clean matches** `out/run-<L>__<R>-clean.match` - outlier-filtered raw matches
   (loaded via `--clean-match-files-prefix`). Controlled by `--remove-outliers-params`.
   Outlier filtering operates per observation: individual bad camera observations
   are dropped rather than deleting the entire 3D point when >= 2 observations survive.
   Clean match files exclude any pair where either observation was flagged as an outlier.

Plot match files with the [[match-plot]] skill (red balls, no lines, plot_matches.py).

## GENEROUS outlier removal when the cameras are still bad (CRITICAL)

When the cameras are far off (e.g. a freshly extended/synthesized linescan), bundle's
default outlier filter (built on reprojection error through the WRONG cameras) NUKES
most good matches: the clean.match can drop from ~130 to ~80, or worse. So:
- Feed the **RAW (unprojected) matches, NOT clean**, to dem2gcp / jitter_solve.
- In jitter_solve/bundle set `--max-initial-reprojection-error` VERY HIGH (e.g. 1e5)
  so no match is pre-rejected while the cameras are still wrong - we are "horribly
  off" and the reprojection error is huge but the matches are real.
- Only tighten outlier removal AFTER a first correction brings the cameras closer.

## DENSE unprojected raw matches: --num-matches-from-disparity

Sparse ip (100-130) is thin control. To get DENSE raw-image matches, run
parallel_stereo (WITH cameras, on the mapproj images) with
`--num-matches-from-disparity 20000` (or more). It computes the dense disparity and
UNPROJECTS the matches to the RAW images, writing `run-disp-<L>__<R>.match` (tens of
thousands of matches, full-frame). Measured on KH-7 Vale: ~64k dense raw matches vs
~130 sparse - a far richer set for a jitter attempt.
- Correlator-mode on the two mapproj images alone (no cameras) also gives ~20k dense
  matches, but in MAPPROJ-image coords (`run-disp-<Lmap>__<Rmap>.match`) - not usable
  for jitter, which needs raw-image matches. Use the stereo-with-cameras form to get
  the unprojected raw matches.
- KEY finding (KH-7 Vale): the warped mapproj IMAGES correlate densely everywhere
  (both draped on COP look similar), FAR better than the warped DEM's hillshade
  (which gave only ~50 dem2gcp GCP). Prefer image-space dense matches over DEM-space.

## ip-detect-method 0 vs 1

`--ip-detect-method 0` = OBALoG (ASP's own), `1` = SIFT/OpenCV. On KH-7 Vale mapproj
images, method 1 gave a few more matches (132 vs 106), both well-distributed; try
both. `--ip-per-tile` + `--matches-per-tile` (e.g. 1000/1000) force uniform coverage
across the frame (one tile = 1024^2 px) instead of clustering on high-texture spots.

## Robust median tie-point triangulation (multi-camera)

When a tie point is observed by 3 or more cameras (>= 2 ray pairs), initial
triangulation combines pairwise ray intersections using the component-wise median
rather than the arithmetic mean. The median has a ~50% breakdown point per axis,
preventing a single camera with an erroneous initial pose from dragging the
initial 3D point off the consensus of the good rays. For a 2-camera point, this
reduces to the single pair's triangulation unchanged.

## Related
[[dem-comparison]] (dem2gcp -> GCP from the ours-vs-ref disparity; census cost-mode 3),
[[jitter-solve]] (feed it the raw matches + GCP; guard GCP with --gcp-robust-threshold,
keep camera-position-uncertainty tight or the GCP move the cameras km),
[[match-plot]] (plot any .match: red balls, no lines).
Doc: bundle_adjustment.rst :numref:`mapip`, tools/bundle_adjust.rst, tools/dem2gcp.rst.
