---
name: bundle-adjust
description: >-
  bundle_adjust mechanics: the --mapprojected-data workflow, the three match sets it
  writes, dense raw matches via --num-matches-from-disparity, using the baked
  adjusted_state.json directly (never --bundle-adjust-prefix), parallel_bundle_adjust, and
  inspecting the residual pointmap. Load before running bundle_adjust or choosing which
  match file to feed dem2gcp/jitter_solve.
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
- The USABLE output is `run-disp-<Lraw>__<Rraw>.match` (named by the ORIGINAL RAW
  images) - raw-image coords, feed to jitter/BA. Do NOT use the aligned-domain
  `run-<L>__<R>.match` (feeding it to BA against raw images gave ~380 px residuals).
- The mapproj DEM feeding num-matches-from-disparity MUST be clean: a DECLARED NoData
  and no absurd sentinel heights. Matches are placed by intersecting each camera ray
  with the mapproj DEM (`Map2CamTrans::forward`); an undeclared -FLT_MAX (-3.4e38)
  sentinel poisons `demHeightGuess` so NO ray intersects -> "Found 0 left-to-right
  matches" -> empty `-disp-` match, while triangulation (direct DEM sample) still makes
  a fine DEM, so F.tif/PC look good yet matches are 0 (A/B proof: clean ASP DEM 3828
  matches, external DEM 0). `gdalinfo -stats -mm` the DEM first; fix with
  `gdal_edit.py -a_nodata` / a `gdalbuildvrt -vrtnodata` overlay, or use a clean ASP DEM
  (point2dem NoData -1e6). ALWAYS eyeball a match file before trusting it ([[match-plot]]);
  a non-empty file can still be garbage.

## ip-detect-method 0 vs 1 (OBALoG vs SIFT under varied illumination)

`--ip-detect-method 0` = OBALoG (ASP's native detector), `1` = OpenCV SIFT.
- On historical terrestrial / KH-7 Vale mapproj images, method 1 gave slightly more matches (132 vs 106), both well-distributed.
- CRITICAL FINDING (OHRC / LRO NAC lunar linescans): under low-sun, grazing illumination, or subtle cross-illumination differences, SIFT (`1`) severely under-matches or starves pairs (often 7 to 38 raw matches, 0 clean matches), falsely making healthy frames look like unmatchable "junk" or "poisoning" the network. In contrast, OBALoG (`0`, with ASP's image normalization) yielded 8,000 to 11,000 matches uniformly across every pair, solving cleanly to sub-pixel (<0.5 px) reprojection.
- RULE: For planetary, shadow-dominated, low-contrast, or varying illumination imagery, prefer `--ip-detect-method 0` (OBALoG).
- `--ip-per-tile` + `--matches-per-tile` (e.g. 1000/1000 or 2000/1000) force uniform coverage across the frame (one tile = 1024^2 px) instead of clustering on high-texture spots.

## Robust median tie-point triangulation (multi-camera)

When a tie point is observed by 3 or more cameras (>= 2 ray pairs), initial
triangulation combines pairwise ray intersections using the component-wise median
rather than the arithmetic mean. The median has a ~50% breakdown point per axis,
preventing a single camera with an erroneous initial pose from dragging the
initial 3D point off the consensus of the good rays. For a 2-camera point, this
reduces to the single pair's triangulation unchanged.

## Use the adjusted_state.json cameras directly, NOT --bundle-adjust-prefix (CRITICAL)

Every bundle_adjust/parallel_bundle_adjust run writes, per input camera, a standalone CSM
state file `<pfx>-<image>.adjusted_state.json` with the adjustment BAKED IN (automatic for
CSM frame/linescan; DG/WorldView and ISIS cams are converted to a CSM linescan state and
baked; `--inline-adjustments` forces it for Pinhole (`.tsai`)/OpticalBar, and
`--solve-intrinsics` implies it). The SECOND bundle that only applies a pc_align transform
(`--apply-initial-transform-only`) also emits a baked state file. USE THAT FILE DIRECTLY as
the camera in every downstream tool (mapproject, parallel_stereo, point2dem, dem2gcp,
jitter_solve, another bundle_adjust) and do NOT also pass `--bundle-adjust-prefix`. Why:
(1) some tools (notably dem2gcp) have NO `--bundle-adjust-prefix`; (2) passing BOTH the
state file AND `--bundle-adjust-prefix` DOUBLE-APPLIES the adjustment - a silent, serious
error; (3) it is self-documenting. Only reach for `--bundle-adjust-prefix` when a run did
NOT produce a state file (older builds / the `.adjust` form). Verify equivalence once:
`cam_test --image img --cam1 run-img.adjusted_state.json --cam2 raw.cam
--cam2-bundle-adjust-prefix run` -> pixel/center diff ~1e-9. Keep intrinsics FIXED (no
`--solve-intrinsics`) unless self-cal is explicitly wanted.

## Many images: parallel_bundle_adjust, and co-locate cams with their mapproj images

For a block/survey (not a handful) ALWAYS use `parallel_bundle_adjust`, never plain
`bundle_adjust` - pairwise IP MATCHING is the bottleneck and plain BA under-parallelizes it
(a 46-image block used ~8 of 56 threads, ~18% efficiency, hours for 582 pairs).
- Its ONLY parallel knobs are `--nodes-list`, `--entry-point`/`--stop-point` (0 stats,
  1 matching, 2 optimization), `--parallel-options`; everything else is a passthrough
  bundle_adjust option. It has NO `--processes`/`--threads-multiprocess` (those are
  parallel_stereo's) - passing one forwards it to the worker bundle_adjust, which prints
  "ERROR: Error parsing input" + help and dies at spawn. Usage is literally
  `parallel_bundle_adjust <bundle_adjust args...> -o pfx` (+ optional `--nodes-list`).
- DRY-RUN the arg string first: `... --stop-point 0` (parses, no compute) or 5 s on the
  head node grepping the log for "Error parsing", THEN qsub.
- It REUSES existing `*.vwip`/`*.match`, so a killed run RESUMES cheaply - but first wipe
  the ~dozen NEWEST `.match` (the one being written when it died can be truncated).
- FOUNDING PRINCIPLE: a bundle run's adjusted cameras AND the images you mapproject with
  those cameras belong in the SAME output dir (e.g. `ba_green/`). Then each bundle dir
  self-contains its cams + their mapproj images - you never confuse which cameras made
  which mapproj images, and can wipe/redo one bundle as a unit.
- REDOING a bundle after fixing cameras: REUSE the matches - keep raw `run-*.match`, WIPE
  `run-*-clean.match` (tied to the old solution's outlier removal), run plain
  `bundle_adjust --match-files-prefix <old_prefix>` in a NEW output dir.

## bundle_adjust caches match files - wipe them when changing IP settings

bundle_adjust REUSES existing `<pfx>-*.match` (and stereo its `-stats.tif`) if present, so
a re-run with different IP settings (`--ip-per-tile`, `--matches-per-tile`,
`--ip-detect-method`, ...) silently keeps the OLD matches and your new settings do nothing.
Before a re-run with changed IP/matching options, delete the old matches or use a fresh
`-o` dir (`rm -rf ba` is the simplest safe move).

## After any solve (esp. --solve-intrinsics): inspect the residual pointmap SPATIALLY

bundle_adjust writes `<pfx>-{initial,final}_residuals_pointmap.csv` (lon, lat, height,
mean_residual_px, num_obs) and `-*_residuals_stats.txt`; also convergence_angles,
camera_offsets, triangulation_offsets. Judge by the MEDIAN (the mean is outlier-driven),
but ALSO colorize the pointmap points by column 4 (reprojection error px) over the terrain
(plasma, robust clamp, split by site if joint) - the median HIDES structure that decides
whether a solve is honest: per-framelet/CCD striping, a cross-track tilt, corner blobs,
under-constrained strip-end framelets, a distortion/pose null-space artifact. MANDATORY
with `--solve-intrinsics`. Two coefficient gotchas: `--fixed-distortion-indices i,j` nails
specific distortion coeffs (CSM transverse via a Ceres SubsetManifold, e.g. `0,10` =
transverse x/y CONSTANT terms, to kill an optical-center/pose gauge shift);
`--remove-outliers-params "pct factor min max"` - the 3rd/4th values CLAMP the removal
threshold in PIXELS, so `"... 100 100"` = "remove only >100 px" = almost no filtering (use
a small ceiling like `"10 10 1 2"` for picky GCP/tie filtering; GCP are never dropped as
outliers).

## Cross-check camera poses BEFORE trusting a bundle (blunder catch)

A huge initial reprojection residual (median >~100 px vs ~1-4 px for good EOP) means the
INPUT cameras carry a systematic - find it, do not let BA silently absorb it. Cheap test
when matches exist: `bundle_adjust --num-iterations 0 --match-files-prefix <matches>` per
camera variant and compare the INITIAL residual; the variant with the lowest initial
residual has the right convention. Also mapproject a FEW pre-bundle frames onto a rough
prior DEM (e.g. Copernicus) and eyeball - the mis-registration names the blunder class:
whole-frame rotation = wrong yaw/kappa convention; mirror = an axis-flip (`diag(1,-1,-1)`
or an R transpose - see [[dem-sanity-check]]); uniform shift = wrong position or optical
center; scale/keystone = wrong focal length or pixel pitch. Blunders live in intrinsics,
position, OR orientation - check all three. (SDB aerial: an OPK->tsai gave 116 px initial
median from a missing UTM meridian-convergence `Rz(gamma)` term, gamma~0.7 deg.)
- **tsai intrinsics: prefer PIXEL units** - focal length and optical center in PIXELS with
  `pitch = 1` (`fu = focal_mm/pixel_mm`, `cu = ncols/2`, `cv = nrows/2`). Physical units
  (focal/center in mm, `pitch = pixel_mm`) are equivalent IF consistent, but pixel+pitch=1
  is the convention here and removes a class of unit-mismatch blunders.

## Related
[[dem-comparison]] (dem2gcp -> GCP from the ours-vs-ref disparity; census cost-mode 3),
[[jitter-solve]] (feed it the raw matches + GCP; guard GCP with --gcp-robust-threshold,
keep camera-position-uncertainty tight or the GCP move the cameras km),
[[match-plot]] (plot any .match: red balls, no lines).
Doc: bundle_adjustment.rst :numref:`mapip`, tools/bundle_adjust.rst, tools/dem2gcp.rst.
