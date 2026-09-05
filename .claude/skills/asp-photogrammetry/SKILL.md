---
name: asp-photogrammetry
description: ASP/VW photogrammetry tool usage and knowledge - stereo/mapproject resolution, pc_align, point2dem --errorimage and tri-err cutoffs, dem_mosaic, gdalwarp/proj.db, disparitydebug, robust median/MAD stats, hillshade alignment judging, dh/dv/dz notation, BA/jitter stats, CSM model-state JSON, derived-raster naming, sparse_disp, and the asp_manual.sh/asp_scripts pointers. Load before running any ASP or VW photogrammetry tool (stereo, bundle_adjust, jitter_solve, mapproject, point2dem, pc_align, dem_mosaic, geodiff).
---

## bundle_adjust: prefer inline-adjusted cameras (CSM/pinhole), not --bundle-adjust-prefix

For camera models that support it (CSM frame and linescan, and pinhole), STRONGLY
prefer using the bundle-adjusted cameras with the adjustment applied INLINE, over
passing `--bundle-adjust-prefix` to every downstream tool. For pinhole, pass
`--inline-adjustments` to bundle_adjust so it writes adjusted camera files. For CSM
this is AUTOMATIC: bundle_adjust always writes, for each input CSM camera, an
adjusted state file `<out-prefix>-<camera>.adjusted_state.json`, which is the CSM
camera with the adjustment baked in. Pass that `.adjusted_state.json` directly to
mapproject / parallel_stereo / point2dem instead of the original camera plus
`--bundle-adjust-prefix`. (`--solve-intrinsics` also implies `--inline-adjustments`
for these models.) See bundle_adjust.rst (`--inline-adjustments`, the CSM
adjusted-state note) and csm.rst for context.

## bundle_adjust caches match files - wipe them when changing IP settings

bundle_adjust REUSES existing `<out-prefix>-*.match` files if present, so re-running
with different interest-point settings (`--ip-per-tile`, `--matches-per-tile`,
`--ip-detect-method`, etc.) silently keeps the OLD matches and your new settings do
nothing. Before a re-run with changed IP/matching options, delete the old matches
(or use a fresh `-o`/`--output-prefix` directory). `rm -rf ba` before re-running is
the simplest safe move.

## Interest-point / match-point plots: RED FILLED balls (dots)

Whenever plotting interest points or tie-point matches on an image (match figures,
pointmaps, ip overlays), draw them as RED FILLED circles (matplotlib
`scatter(..., c='red', marker='o')`, filled, not hollow, not yellow). This is the
house style for match plots everywhere - docs, notes, chat figures.

## Derived Raster Product Naming (DEMs, diffs, cmaps, hillshades, pngs)

When producing many derived rasters across processing stages (DEM comparison work
etc.), name them so they stay trackable later. Pattern:
`<stage>_<product>[_<modifier>].<ext>`

- `<stage>` = the processing stage / source identity that made the DEM:
  `vendor`, `deband`, `dem2gcp`, `ba_htdem`, `ba_nodem`, etc. with `_vN` for
  iterations (`ba_htdem_v2`). NEVER use vague tags like `before`/`after`/`new`/`tmp`.
- `<product>` chains left to right as products build on each other:
  `dem` -> `hs` -> `<ref>diff` (e.g. `ctxdiff`) -> `<ref>diff_cmap`. A derived
  product borrows its parent's name and just extends it (the `.png` viewer copy
  keeps the same basename as its `.tif`).
- NEVER use leading-underscore or `tmp`/scratch throwaway names (`_eyeball.png`,
  `_dz.tif`) for anything that outlives the command - they read as junk and end up
  dangling. Give an HONEST name derived from the SOURCE product: an eyeball/preview
  PNG of `foo-DEM.tif` is `foo-DEM_eyeball.png` (or the same basename). Only a truly
  intermediate file deleted in the same script may use a `_` prefix, and it must be
  `rm`'d before exit.
- WHERE it lands: write each derived product into the SAME dir as its source
  dataset, right next to its parent - NEVER a throwaway `work_*`/`tmp` dir. A
  regridded CTX lives by the CTX (`ref/.../ctx_regrid_10m.tif`); a resampled DEM
  and its diff/cmap live by that DEM. General rule for any dataset you manipulate,
  not just rasters: result goes home next to the input, since scratch dirs get wiped.
- PRESERVE THE SOURCE'S FULL BASENAME + a suffix - do NOT re-invent a short cryptic
  name that drops it. `data/lidar/Florida_..._Ellipsoid.tif` -> blurred is
  `..._Ellipsoid_blur.tif`, filled `..._Ellipsoid_filled.tif`, filled-then-blurred
  `..._Ellipsoid_filled_blur.tif` (chain suffixes in operation order), ALL in
  `data/lidar/`. Do NOT dump it in a generic `dem/` as `blurred_lidar.tif` - that
  mixes lidar-derived with stereo DEMs and destroys provenance. Distinguish variants
  by an EXPLICIT token (`_blur`, `_filled`, `_mask`, `_full`), NEVER by only `.` vs
  `_` before the same word (`X.map.tif` vs `X_map.tif` is an unreadable trap; the
  `.map.tif` form is reserved for where a tool requires it, e.g. bundle_adjust
  --mapprojected-data).

## Running sparse_disp From a Dev Build

`sparse_disp` is a Python script needing numpy/scipy/gdal. A packaged release
wraps it to its bundled Python. A dev build has no wrapper, so the `python` on
PATH must carry those modules. Recipe: put the dev `install/bin` AHEAD of the
deps env on PATH:
`export PATH=~/projects/StereoPipeline/install/bin:$ISISROOT/bin:$PATH`
(`$ISISROOT`=`asp_deps`). The ASP tools (including `sparse_disp`) then resolve
from `install/bin`; `python`, absent there, falls through to `asp_deps`. The
ordering is self-correcting. NEVER use PYTHONPATH-only with a different
interpreter (ABI mismatch -> import failure). The regression config points `$ASP`
at the RELEASE TARBALL, which lags dev source by up to a day, so when testing a
fresh `sparse_disp` change, force dev `install/bin` first and confirm which copy
ran. The dev-note comment lives at the top of the `sparse_disp` script too.

## Inspect BA/Jitter Stats After Every Run

After any bundle_adjust or jitter_solve run, inspect the residual/stats output
files (initial AND final per-camera residual stats, convergence_angles,
camera_offsets, triangulation_offsets, pointmap) - listed in the output-files
section of the bundle_adjust and jitter_solve RST docs. Judge by the MEDIAN (the
mean is outlier-driven). Skip the per-residual raw_pixels files (too big). These
tell you whether the solve behaved (sub-pixel medians, bounded offsets, cameras
multiply-tied).

## gdal "Cannot find proj.db" -> the PROJ framework is missing, output is JUNK (CRITICAL)

Any time a gdal/ASP tool warns `PROJ: proj_create_from_name: Cannot find proj.db`,
the env has NO PROJ data. This is NEVER cosmetic. STOP immediately - do not call it
harmless, do not proceed. Without proj.db every projection operation (`-t_srs`,
gdalwarp, reprojection, geodiff across datums, mapproject) silently misbehaves and
produces WRONG, subtly-broken georeferenced results. These bugs are subtle,
downstream, and I am NOT reliably able to detect or debug them after the fact - so
the only safe policy is to PREVENT them: out of an abundance of caution, ENSURE the
geo framework (PROJ + proj.db) is present for EVERY gdal/ASP invocation, always, up
front, before running anything. Never run a geo tool and hope the georef survives.
- Local / conda: `conda activate asp_deps` (or any env with gdal) first.
- pfe / Athena packaged build (non-interactive ssh has nothing set): export the
  PROJ path to the packaged share dir in EVERY remote script, e.g.
  `export PROJ_LIB=$HOME/projects/BinaryBuilder/StereoPipeline/share/proj`
  (also `export PROJ_DATA=$PROJ_LIB` for PROJ 9+; proj.db lives there). Set it
  alongside PATH/ISISROOT in the script header, not as an afterthought.
- After any masking / image_calc / warp, VERIFY the output still carries the right
  CRS and geotransform (`gdalinfo | grep -E "PROJCRS|Origin|Pixel Size"`) before
  trusting it. A missing or altered CRS means redo it with PROJ set.
Even when a given op (e.g. image_calc copying an existing geotransform) happens to
survive, treat the warning as a hard stop: fix the env and re-run. Do NOT rationalize
it away as cosmetic - that mistake shipped a georef-broken result once and Oleg had to
catch it.

## Mapprojected stereo: give EVERY image the SAME --tr and --t_srs (CRITICAL, keeps recurring)

When mapprojecting images to feed `parallel_stereo`, ALL of them must share the same
projection AND grid size, or stereo aborts with "The input mapprojected images must
have the same ground resolution ... can be overridden with
--allow-different-mapproject-gsd, but is not recommended." By DEFAULT `mapproject`
auto-picks the GSD per image from its own resolution, so left vs right (different
off-nadir angles) come out at DIFFERENT GSDs (e.g. 1.245 vs 1.587 m) and stereo
refuses. FIX: pass an explicit, identical `--tr <gsd>` (and `--t_srs` / `--t_srs auto`)
to EVERY mapproject call so the grids overlay pixel-for-pixel. Do NOT reach for
`--allow-different-mapproject-gsd` (degrades the result). ASP docs say this outright:
`docs/tools/mapproject.rst` ("All mapprojected images passed to stereo should use the
same projection and grid size", :numref:`mapproj_grid` / :numref:`mapproj-example`).
Burned 2026-08-28 on the WV green CCD before/after run. Same GSD also lets the
mapprojected images, DEM, and mosaics share one grid phase.

## gdalwarp: Always -r cubicspline, Never the Default Nearest-Neighbor

Always run `gdalwarp` with `-r cubicspline`; never rely on its default nearest-neighbor resampling, which snaps and misregisters continuous rasters (DEMs, geodiffs, error fields) by up to half a pixel.

## dem_mosaic: Call With `-o output.tif`, Not `-o out`

Recent `dem_mosaic` writes the given name directly when `-o` ends in `.tif` (e.g. `-o mosaic.tif` -> `mosaic.tif`); a bare `-o out` produces `out-tile-0.tif`. Always pass the honest `.tif` output name and reference that file later.

## pc_align: Denser Cloud First, and Direct-vs-Inverse Transform (CRITICAL, easy to get backwards)

`pc_align <reference> <source>` aligns SOURCE onto REFERENCE. Two hard rules that
interact and silently ruin everything if confused:
- **Denser cloud MUST be the first (reference) arg** (ICP quality). So if your ASP
  DEM is DENSER than the ground-truth you align to (e.g. an 18 m CTX DEM vs a
  200 m HRSC/MOLA reference), the DENSE DEM goes FIRST, the coarse truth SECOND -
  the opposite of the "align my DEM to the reference" mental model.
- `run-transform.txt` maps SECOND(source)->FIRST(ref); `run-inverse-transform.txt`
  maps FIRST->SECOND. To move the CAMERAS (which live in the DEM's frame) INTO the
  coarse-truth frame, you need FIRST->SECOND = **`run-inverse-transform.txt`**.
- Apply to CSM cameras: `bundle_adjust <imgs> <bundled_state.json> --initial-transform
  align/run-inverse-transform.txt --apply-initial-transform-only --inline-adjustments`.
  (Per pc_align.rst "Applying a transform to cameras": stereo DEM as pc_align's FIRST
  arg -> use the INVERSE transform; stereo DEM as SECOND arg -> use the direct one.)
Verify: mapproject the aligned cams onto the reference and overlay (no shift); the
aligned DEM's geodiff median vs the reference should be near zero. Full worked recipe
in `~/projects/cassis_olympus_mons/cassis_002920_ctxpair_A_notes.sh` (stage 1e-1f).

## Prefer adjusted_state.json Cameras, NOT --bundle-adjust-prefix (CRITICAL)

For MANY images (a block/survey, not a handful), ALWAYS use `parallel_bundle_adjust`,
never plain `bundle_adjust`. Pairwise interest-point MATCHING is the bottleneck and plain
`bundle_adjust` runs it under-parallelized (seen 2026-08-26: a 46-image aerial block used
only ~8 of 56 threads, ~18% job efficiency - hours for 582 pairs). It REUSES existing
`*.vwip` (per-image IP) and `*.match` files, so a killed run RESUMES cheaply - but first
wipe the ~dozen NEWEST `.match` files (the one being written when the job died can be
truncated/corrupt).
- **Options gotcha (parse-error trap):** `parallel_bundle_adjust` has NO
  `--processes`/`--threads-multiprocess`/`--threads-singleprocess` - those are
  `parallel_stereo`'s. Its ONLY parallel knobs are `--nodes-list` (multi-node; omit =
  local cores via GNU Parallel), `--entry-point`/`--stop-point` (0 stats, 1 matching, 2
  optimization), `--parallel-options`. EVERYTHING else is a passthrough `bundle_adjust`
  option. Passing a `parallel_stereo`-only flag forwards it to the worker `bundle_adjust`,
  which prints "ERROR: Error parsing input" + full help and the run dies at spawn. So its
  usage is literally `parallel_bundle_adjust <bundle_adjust args...> -o pfx` (+ optional
  `--nodes-list`).
- **DRY-RUN BEFORE qsub** (burned 2026-08-26, wasted a queue+run): validate the arg string
  cheaply first, e.g. `parallel_bundle_adjust <all args> --stop-point 0` (parses + stops
  before statistics, no real compute), or run 5 s on the head node and grep the log for
  "Error parsing". Only then qsub the full run.
- **`--inline-adjustments` is PREFERRED for BOTH Pinhole (.tsai) AND CSM.** It bakes the
  solved extrinsics into standalone adjusted camera files (`<pfx>-<image>.tsai` for pinhole;
  `<pfx>-<image>.adjusted_state.json` for CSM). Then USE THOSE ADJUSTED FILES DIRECTLY as
  the cameras in every downstream tool (mapproject, parallel_stereo, point2dem, dem2gcp,
  jitter, another BA) - do NOT also pass `--bundle-adjust-prefix`. Keep intrinsics FIXED
  (do NOT pass `--solve-intrinsics`) unless intrinsic self-cal is explicitly wanted.
- Same spirit for stereo: `parallel_stereo`, and split many pairs across several qsub jobs
  (never one serial job that does all - it will time out / die).
- **HIGH-VALUE DEBUG SKILL — cross-check camera poses by mapprojecting a FEW frames with
  the (pre-bundle) cameras onto a rough prior DEM (e.g. Copernicus) and eyeballing.** If the
  orientation/position/intrinsics are right, each frame's content lands where expected -
  shoreline, islands, roads fall on the DEM's coastline/features (Copernicus is coarse but
  gets shoreline+islands well). Mis-registration diagnoses the blunder class: a whole-frame
  rotation = wrong yaw/κ convention; a mirror = an axis-flip (the photo↔camera `diag(1,-1,-1)`
  or an R transpose); a uniform shift = wrong position or optical-center; a scale/keystone =
  wrong focal length or pixel pitch. Do this BEFORE trusting a bundle: a huge initial
  reprojection residual (e.g. median >~100 px, vs the ~1-4 px expected for good EOP) means the
  INPUT cameras carry a systematic - find it rather than let BA silently absorb it. Blunders
  live in intrinsics (focal, center, pitch), position, OR orientation - check all three.
  Cheap quantitative test when matches exist: `bundle_adjust --num-iterations 0
  --match-files-prefix <matches>` per camera variant and compare the INITIAL residual; the
  variant with the lowest initial residual has the right convention. (SDB aerial 2026-08-26:
  a photogrammetric OPK→tsai gave 116 px initial median; suspected the UTM meridian-convergence
  `Rz(γ)` term - γ≈0.7° matches the ~0.67° implied error.)
- **tsai intrinsics: prefer PIXEL units** - focal length and optical center in PIXELS with
  `pitch = 1` (e.g. `fu = focal_mm/pixel_mm`, `cu = ncols/2`, `cv = nrows/2`). Physical units
  (focal in mm, center in mm, `pitch = pixel_mm`) are mathematically equivalent IF consistent,
  but pixel+pitch=1 is the convention here and removes a whole class of unit-mismatch blunders.
- When REDOING a bundle after fixing cameras, REUSE the matches: keep the raw `run-*.match`,
  but WIPE the `run-*-clean.match` (they are tied to the previous solution's outlier removal),
  and run plain `bundle_adjust --match-files-prefix <old_prefix>` in a NEW output dir. Keep the
  parallel_bundle_adjust dir as the match store; the honest solve lives in its own dir.
- **dem_mosaic always appends `-tile-0.tif` (and `-max`/`-first` etc.) to `-o <prefix>`** - it
  never writes the exact name you pass, so you get the "pathetic" `mosaic-tile-0.tif`. Give an
  explicit clear prefix (`-o mosaic_dem` -> `mosaic_dem-tile-0.tif`) and, if a clean single-file
  name is wanted downstream, `gdal_translate mosaic_dem-tile-0.tif mosaic_dem.tif` (or `mv`) right
  after, so scripts/plots reference `mosaic_dem.tif` not the tile suffix.
- **FOUNDING PRINCIPLE — related outputs go in ONE subdir; co-locate mapprojected images
  with the bundle cameras that made them.** A `bundle_adjust` run's adjusted cameras
  (`<pfx>-*.tsai`/`.adjusted_state.json`) AND the images you mapproject with those cameras
  belong in the SAME output dir (the bundle dir, e.g. `ba_green/…`). Then N bundle dirs each
  SELF-CONTAIN their cams + their mapprojected images: you never confuse which cameras
  produced which mapproj images, and you can wipe/redo one bundle's whole output as a unit.
  Generalizes: keep a stage's related products together in its own subdir, not scattered.

Every `bundle_adjust`/`parallel_bundle_adjust` run writes, per input camera, a
standalone CSM state file `<prefix>-<image>.adjusted_state.json` with the
adjustment BAKED IN (for CSM frame/linescan this is automatic; DG/WorldView and
ISIS cams are converted to a CSM linescan state and baked; `--inline-adjustments`
forces it for Pinhole/OpticalBar). This includes the SECOND bundle that only
applies a pc_align transform (`--apply-initial-transform-only`) - it too emits a
baked `adjusted_state.json`. USE THAT FILE DIRECTLY as the camera in every
downstream tool (mapproject, parallel_stereo, dem2gcp, jitter_solve, another
bundle_adjust) and do NOT pass `--bundle-adjust-prefix`. Reasons: (1) some tools
(notably `dem2gcp`) have NO `--bundle-adjust-prefix` option, so the baked camera
is the only way to feed them an adjusted camera; (2) passing BOTH the
adjusted_state.json AND `--bundle-adjust-prefix` DOUBLE-APPLIES the adjustment - a
silent, serious error; (3) it is self-documenting (the camera file IS the state).

## ALWAYS check a DEM's min value AND NoData before using it (CRITICAL)
Before feeding ANY DEM to mapproject / stereo / bundle_adjust (especially as the mapproj
surface for `num-matches-from-disp(-triplets)` or any camera-ray -> DEM intersection), run
`gdalinfo -stats -mm <dem>` and confirm BOTH: (1) a NoData Value IS declared, and (2) the
STATISTICS_MINIMUM is physically sane (not a sentinel). The classic trap: a "filled" DEM whose
nodata cells hold -FLT_MAX (-3.4028e38, or -1e38 / -1e6 / -9999) but with NO NoData declared in
the header. VW/ASP then treats the sentinel as VALID terrain: `vw::cartography::demHeightGuess`
averages it to garbage (~-5.7e37 m), and `Map2CamTrans::forward()` (camera_pixel_to_dem_xyz)
diverges -> ray-DEM intersection FAILS for every ray -> `num-matches-from-disparity` writes an
EMPTY `-disp-` match, while `reverse()` (direct DEM sampling, used by triangulation) still works,
so the DEM/PC look fine and the failure is silent. (Burned 2026-08-25, SDB Key West: external
lidar blurDem, 0 matches; proven by A/B - a clean ASP DEM gave 3828, the same DEM + a
`gdalbuildvrt -vrtnodata` overlay gave 4622.) FIX: declare it - `gdal_edit.py -a_nodata <sentinel>
<dem>` (in place) or a non-destructive `gdalbuildvrt -vrtnodata <sentinel> nd.vrt <dem>` overlay -
or use a clean ASP-made DEM (point2dem writes NoData -1e6, sane min/max). Tell-tale in
`gdalinfo -stats`: STATISTICS_MINIMUM ~ -3.4e38 (or other sentinel) and/or a missing "NoData
Value". This is also a latent VW robustness gap (demHeightGuess should reject absurd/-FLT_MAX
heights) worth an upstream fix.

## Dense matches from a stereo disparity (num-matches-from-disparity) - residual/refraction BA
`--num-matches-from-disparity` and `--num-matches-from-disp-triplets` ARE ALIASES in current
ASP. Verified src/asp/Tools/stereo.cc ~987 ("In the latest ASP always create triplets"):
`--num-matches-from-disparity`'s value is COPIED into `num_matches_from_disp_triplets` and the
former is ZEROED, after printing "equivalent to --num-matches-from-disp-triplets". So ONLY the
triplets code path (`tripletsMatches` in src/asp/Core/DisparityProcessing.cc) ever runs;
`noTripletsMatches` is effectively dead code. `tripletsMatches` DOES work on a 2-image pair
(no >=3 requirement); it walks the LEFT RAW-IMAGE grid, `left_trans->forward` into the
disparity, samples, `right_trans->reverse` to raw -> so matches END UP IN RAW image coords.
OUTPUT FILE: `<out>-disp-<Lraw>__<Rraw>.match` (named after the ORIGINAL raw images, from the
mapproj header) - this is the one to USE. Do NOT use `<out>-L__R.match` (input/aligned-domain
sampling; feeding it to BA against RAW images gave ~380 px reproj residuals, burned 2026-08-25).
Consume: copy/rename `-disp-<raw>__<raw>.match` to `<prefix>-<Lrawbase>__<Rrawbase>.match`,
`bundle_adjust --match-files-prefix <prefix>` with RAW images + cameras, and do NOT pass
`--mapprojected-data-list` (mutually exclusive; not needed - matches are raw). Sanity: 0-iter
pointmap reproj residuals must be SMALL.
SOLVED (2026-08-25): 0 matches from a MAPPROJECTED pair was NOT a code bug - it was the
mapproj DEM. `tripletsMatches` places matches by walking the LEFT RAW grid and calling
`left_trans->forward()` = `Map2CamTrans::forward()` = `camera_pixel_to_dem_xyz` (intersect the
camera ray with the MAPPROJ DEM). Our external "filled" lidar DEM contained -FLT_MAX
(-3.4e38) sentinel pixels but declared NO NoData in the GeoTIFF header, so VW treated -3.4e38
as valid terrain: `demHeightGuess` averaged to ~-5.7e37 m, and the ray-marching started at an
absurd height and NEVER intersected for ANY ray -> "Found 0 left-to-right matches" -> empty
`-disp-`. `reverse()` (map->raw, direct DEM sample) is unaffected, so triangulation still made
a fine DEM - THAT is why F.tif/PC were good yet matches were 0. PROVEN by an A/B on one window,
same cameras/images, fresh stereo each, only the mapproj DEM differing: internal ASP DEM
(NoData -1e6, sane range) -> 3828 matches; external DEM -> 0. LESSON: for
num-matches-from-disp(-triplets) on mapprojected inputs, the mapproj DEM MUST be clean - a
DECLARED NoData and no absurd sentinel heights - or `forward()` silently yields 0 while tri
looks fine. CHECK the DEM with `gdalinfo -stats -mm` (watch STATISTICS_MINIMUM ~ -3.4e38 and a
missing "NoData Value"); fix with `gdal_edit.py -a_nodata <sentinel>` / a `gdalbuildvrt
-vrtnodata` overlay, or use a clean ASP-made DEM (point2dem writes NoData -1e6). demHeightGuess
being poisoned by undeclared -FLT_MAX is also a latent VW robustness gap (guard the height
guess / reject absurd values) worth an upstream fix. F.tif non-empty is necessary but NOT
sufficient - the mapproj DEM must be clean too.
ALWAYS EYEBALL any match / dense-match file before trusting it: overlay on BOTH images
(`~/bin/plot_matches.py` or stereo_gui) and confirm the SAME feature sits at the two match
endpoints. A NON-empty file can still be garbage (wrong domain/transform -> the 380 px case).
CHEAP: matches need no full point cloud - a single `stereo_tri` resuming an existing run (add
cameras + the flag) suffices. Standard bathy stereo (`--left/right-bathy-mask` +
`--bathy-plane` + `--refraction-index`) is the dense alternative that avoids matches entirely.
Verify equivalence once with `cam_test --image img --cam1 run-img.adjusted_state.json
--cam2 raw.cam --cam2-bundle-adjust-prefix run` -> pixel/center diff must be ~1e-9
(machine zero; confirmed on WV3, 2026-08-23). Only reach for `--bundle-adjust-prefix`
when a run did NOT produce adjusted_state.json (older builds / adjust-only .adjust).

## point2dem --errorimage Always; Mosaic the Error Too

Every `point2dem` that makes a DEM gets `--errorimage` (the triangulation
IntersectionErr is a key diagnostic - distortion/misreg/blunders show there).
Whenever DEMs are `dem_mosaic`'d, ALSO mosaic the per-pair error images
(`dem_mosaic --max` over the `*-IntersectionErr.tif` -> a worst-case
tri-error mosaic). For an ALIGNED DEM, align the POINT CLOUD (it carries the
error in band 4) and `point2dem --errorimage` it, rather than aligning the
bare DEM (a rigid align repositions the error, doesn't change it).

**A `*-IntersectionErr.tif` may be a VECTOR (3-band X/Y/Z), not a magnitude.**
`gdalinfo` it first: if `RasterCount` is 3 (point2dem wrote the 3D error, not the
norm - e.g. Jay Laura's Kaguya usgs_dtms_v2 errors), you MUST take the NORM
`sqrt(b1^2+b2^2+b3^2)`. Plotting/stat-ing one band understates it badly - on
Jay's Kaguya errs band 1 is ~0.01 m but the norm is ~1 m (~100x). Full write-up:
`~/projects/asp_manual.sh` (TRIANGULATION ERROR section).

## point2dem After Jitter: Same Absolute Tri-Err Cutoff Before and After (CRITICAL)

point2dem's default `--remove-outliers-params 75 3` is a RELATIVE tri-err filter,
so it can be too aggressive and strip VALID data. After a jitter_solve the
triangulation error drops, which TIGHTENS the relative threshold, so the
post-jitter DEM loses lines the pre-jitter DEM kept. A before-minus-after DEM
then shows fake gaps and banding from differential stripping, not from jitter.
Rules for a jitter before/after comparison:
- Give BOTH the pre-jitter and post-jitter point2dem the SAME absolute cutoff
  `--max-valid-triangulation-error`, set to about 5*GSD (CTX GSD ~5 m -> ~25 m),
  so the filter never removes real terrain and both DEMs strip identically.
- Difference against the ALIGNED pre-jitter DEM (same frame as the jitter
  cameras, which come from the aligned bundle_adjust), NOT the unaligned stereo
  DEM. Otherwise the diff carries the whole pc_align vertical offset (a ~220 m
  bulk shift bit the CTX FUB jitter study, 2026-08-18: every before-minus-after
  read a uniform +221 m because it used the unaligned pre-jitter DEM).

## ASP Tools: Read the Manual, Not --help

When using an ASP tool, do NOT rely on `--help` - read its RST manual
(`~/projects/StereoPipeline/docs/tools/<tool>.rst`). `--help` lists flags but has
NO sensible usage examples; the RST has worked examples and the gotchas that make
options behave (e.g. dem_mosaic fill: small `--fill-search-radius` + more
`--fill-num-passes`, since a large radius stalls). bundle_adjust, dem_mosaic,
pc_align, and the rest all have extensive documented examples.

## ASP Primer / Manual I Maintain - `~/projects/asp_manual.sh` (READ AT START OF ANY ASP WORK)

This is my growing ASP primer - the ONE place that collects every hard-won,
non-obvious, recurring insight into how ASP/VW actually works. READ IT when
starting any ASP task. STANDING RULE: whenever I learn something about how ASP
works that is non-obvious and likely to recur (a workflow, a gotcha, a tool
behavior, a file format, an option interaction), ADD IT to this primer and
`git -C ~/projects add`/commit/push - do not leave it only in a per-project
notes file. Build it up over time so I stop rediscovering the same things.
What it currently contains (grep the headers for detail):
- Reading the RST manuals (not --help).
- Interest-point .match file format (binary, how to read/compare).
- MAPPROJECTED STEREO - the two-pass workflow (mapproject at NATIVE image GSD,
  same --tr/--t_srs both images, aligned cameras, --alignment-method none,
  eval tri-err/ortho/color-hillshade-DEM/dz/dd-H/dd-V). The high-quality path
  used for CaSSIS/CTX/Viking/TMC/OHRC.
- **parallel_stereo PARALLELISM (--nodes-list + --processes + --threads-
  multiprocess): READ the primer section before setting these on ANY
  parallel_stereo/parallel_bundle_adjust run.** Bare minimum: get_num_cpus()
  auto-detects cores so one script is portable; ALWAYS pass --nodes-list
  $PBS_NODEFILE (single-node file = 1 node, safe); set --processes P and
  --threads-multiprocess T with P*T ~= cores/node (P reduced if RAM-bound, e.g.
  Athena 256 -> --processes 32 --threads-multiprocess 8). **NEVER size
  --processes from `nproc` or `wc -l < $PBS_NODEFILE` in a wrapper: INSIDE a NAS
  PBS job both return 1 (the node has 128-256 CPUs), silently forcing
  --processes 1 = FULLY SERIAL (Eff 0%, cpupercent ~1.5 cores). Pass --processes
  EXPLICITLY (you set ncpus in the qsub) or use `grep -c ^processor /proc/cpuinfo`;
  ALWAYS verify with `qstat -f <job> | grep cpupercent` (/100 = cores busy).
  Full write-up: qsub_rules.sh RULE E, asp_manual.sh. Burned 2026-08-17.**
- pc_align applying a transform to cameras (direct vs inverse; carry via
  bundle_adjust --apply-initial-transform-only --inline-adjustments).
- ATHENA (Turin) for ASP jobs - fully visible (/nobackup + build mounted),
  256 cores/node, more expensive; single-node parallelism via --processes.
Bare minimum to remember without reading:
- Interest-point `.match` files (written by both VW and ASP - stereo,
  bundle_adjust, jitter_solve, image_align, etc.) are little-endian binary:
  header is two `uint64` counts (equal = number of matched pairs), then the IP
  records. Read just the count: first 8 bytes as `uint64`.
- The regression suite keeps real match files: `ss*/run/*.match` (fresh) vs
  `ss*/gold/*.match` (reference) - the right layer to judge an IP-affecting
  change is diffing those, not just the final DEM/camera output.
- Official parser: `parse_match_file.py` (binary<->text). Visual/residual
  overlay: `~/bin/plot_matches.py` (use `--red --radius N` for readable solid-red
  dots, never the rainbow, when handing a match overlay to a human).
- bundle_adjust/stereo CACHE their `.match` and per-image `-stats.tif` in the run
  dir and REUSE them on a rerun, so ALWAYS wipe the run dir (or use a fresh one)
  before a rerun meant to test a change, or you measure stale results and draw
  wrong conclusions.

## Canonical ASP Scripts - `~/projects/asp_scripts/` (USE or ADAPT, don't re-figure)

Reusable, commented, parameterized reference workers for the ASP operations we
keep redoing (so we stop reinventing and re-blundering). When doing new ASP work,
USE one of these or ADAPT it; do not write from scratch. Each embeds the hard-won
rules inline. The primer `~/projects/asp_manual.sh` points to each script.
- `stereo_mapproj.sh` - mapprojected stereo (pass 2): native-GSD mapproject, DEM
  as the LAST parallel_stereo arg, asp_mgm + subpixel-9, --nodes-list + --processes
  (Athena: nodesMode local), optional `--resume-at-corr`.
- `stereo_localepi.sh` - local_epipolar stereo (pass 1) + point2dem.
- `bundle_adjust.sh`, `parallel_bundle_adjust.sh` - BA (list-order + residual rules).
- `pc_align.sh` - align a DEM to a ref: regrid `-r average` (dense-vs-sparse fix),
  hillshade seed, carry transform to native cameras.
- `geo_figures.py` - CANONICAL plotting library (import it): hillshade DEM,
  colorized signed diff (dz/dd-H/dd-V, diverging+symmetric+robust clamp), one-sided
  error (tri-err, magma), each with its OWN full-image-height colorbar + unit; NO
  text baked in the figure (caption lives in the HTML/RST); robust median/NMAD.
  Do not re-write figure code per project. Detail: visual_raster_inspection.sh.
- `fetch_lola_shots.sh` - AUTOMATED LOLA shots for a lon/lat box from the NASA/USGS
  LOLA COPC on AWS (PDAL, no manual download) -> lon,lat,radius_km CSV. Detail:
  `~/projects/lola_notes.sh` (LOLA gridded LDEM vs shots; the AWS COPC method is
  the primary route, superseding the manual ODE tool).

## Robust Stats: ALWAYS median/MAD, NEVER mean/std for raster comparison metrics (CRITICAL)

For comparing rasters (dz vs a reference, dd-H/dd-V disparity, tri-err /
IntersectionErr mosaics, geodiffs), ALWAYS report and compare the robust
**median and MAD** (plus p90/p99 if useful), NOT the mean and std. These fields
carry a few catastrophic blunder pixels (a max-tri-err mosaic hit 750-1440 m at
Jezero) that pollute the MEAN and STD wildly while the median/MAD are stable.
Judging by the mean led to a wrong conclusion once (a "6x better tri-err" that was
purely blunder pixels; the medians were identical - CaSSIS WF1 vs WF2, 2026-07-11).
`gdalinfo -stats` gives only mean/std/min/max - for median/MAD read the raster
with numpy (nodata-aware): see `~/projects/cassis_asp/tri_median.py`.

## DEM Alignment: Judge by Hillshade Eyeball, NOT Vertical Diffs (CRITICAL)

For ANY DEM alignment/registration work, judge by the EYEBALL of HILLSHADES (red/green
overlay), NEVER by vertical dz/geodiff or its NMAD/std - dz is blind to horizontal
misregistration and dominated by DEM noise/coverage, so a dz number says nothing about
alignment (a well-aligned pair can show 20+ m dz NMAD; a badly-shifted one near zero).
Full detail, recipes, and cross-modality (image-vs-hillshade) tips:
`~/projects/visual_raster_inspection.sh`.

## Disparity Stats: disparitydebug --raw, NEVER gdalinfo on run-F.tif (CRITICAL)

A correlator/stereo `run-F.tif` (parallel_stereo `--correlator-mode`) packs horizontal
disparity (band 1), vertical disparity (band 2), and a VALIDITY MASK (band 3) in one file.
`gdalinfo -stats` and `gdal_translate -b` IGNORE band 3, so invalid (uncorrelated) pixels
read as 0 and pollute the dd-H/dd-V stats - a mostly-invalid flat scene then fakes a ~0
shift, HIDING the real one. This bit us REPEATEDLY (a true CaSSIS dd-V shift of -3.4 px read
as 1.4, flipping a conclusion). ALWAYS extract the disparity with:
`disparitydebug --raw run-F.tif --output-prefix P` -> `P-H.tif` (dd-H), `P-V.tif` (dd-V),
Float32 with real nodata (-1e6); THEN stat those (gdalinfo -stats is nodata-aware on them).
disparitydebug is ASP's OWN tool; a release build sets ISIS up itself, our dev/packaged build
needs `export ISISROOT=<asp_deps env>` (holds IsisPreferences). EVERY script that runs
correlator-mode and analyzes disparity must emit these raw bands right there (cassis_corr.sh
does). NEVER `gdal_translate -b` to pick a disparity band - it writes the invalid pixels as 0.

## Alignment-Residual Notation: dh / dv / dz, Not dd-H / dd-V

When labeling a horizontal/vertical alignment residual (a DEM-to-reference
correlation shift, or a stereo disparity residual) in FIGURES, CAPTIONS, and DOCS,
use the short informal `dh` (horizontal), `dv` (vertical), and `dz` (height
difference). AVOID `dd-H` / `dd-V` - even though these are informal, `dh`/`dv`/`dz`
read more easily and are consistent. This is a labeling convention only; internal
band names from `disparitydebug` (`-H.tif`, `-V.tif`) stay as the tool emits them.

## Stereo/Photogrammetry Resolution (CRITICAL - screwed this up MULTIPLE TIMES)

For stereo/photogrammetry, correlation ALWAYS runs at near-native image
resolution. When mapprojecting, pin ONE `--tr` that is a COMPROMISE near the
native GSD of the INPUT IMAGES (not the DEM), and mapproject BOTH/ALL images at
that SAME res - auto (no `--tr`) drifts per image and parallel_stereo
correlator-mode then errors on mismatched GSD. The seed/draping DEM is only an
interpolated surface, usually ~4x coarser; its coarseness must NEVER set the
mapproject/correlation grid. Only the OUTPUT DEM (point2dem) lives at the coarse
~4x-GSD res. Do not downsample imagery to the DEM. (CaSSIS native GSD ~4.59 m;
DEM ~18 m.) Repeatedly assumed the DEM res sets the mapproject res - it does NOT.
Mapprojecting at the coarse DEM res produced a rough, blocky DEM (CaSSIS PHASE 0,
2026-06-27). Corollary: for a simple 2-image pair you can SKIP mapproject entirely
and stereo the raw images (affineepipolar) - correlation is native by definition;
mapproject is for many images / hard terrain / large convergence.

**Hillshade-correlation for dem2gcp AND for DEM-to-DEM/CTX alignment ALWAYS runs
at NATIVE IMAGE resolution (~4x FINER than the DEM grid), NEVER at the coarser
DEM/CTX res. VERY IMPORTANT.** The dense correlation window (5x5/9x9) locks onto
coarser features while the disparity is sampled on the fine native grid at SUBPIXEL,
so it resolves ~6 m shifts even when DEMs are ~18 m. The honest gain is finer spatial
sampling of the shift field (~18 m -> ~9-10 m effective), not lower per-point noise;
faux precision in smooth patches averages out over many dense GCP. Full rationale:
`~/projects/cassis_asp/cassis_native_res_rationale.sh`.

## CSM Model-State JSON

Parse CSM model-state / `.adjusted_state.json` files CAREFULLY - they are NOT plain
JSON: a model-name line comes FIRST, then the JSON (so `json.load` fails; skip line 1).
Frame center, linescan position interpolation, parsing recipe: `~/projects/csm_camera_notes.sh`.

## Aligning bundle-adjusted cameras to a reference DEM (pc_align -> apply to cameras)

To seat BA cameras on a reference DEM/lidar (fix a residual camera-vs-ground offset)
without re-solving the whole bundle. Learned on the SDB WV-3 tri-stereo run (Key
West, 2026-08). Manual = the RST docs in the ASP source: `~/projects/StereoPipeline/docs/`.

1. Read the offset. BA writes `run-{initial,final}_residuals_pointmap.csv`
   (`lon,lat,height_above_datum,mean_resid,nobs`). geodiff it vs the reference:
   `geodiff ref.tif pointmap.csv --csv-format '1:lon 2:lat 3:height_above_datum'`.
   Heights are ELLIPSOIDAL above the datum - mind the geoid: in Florida sea level is
   ~ -25 m ellipsoidal, so a LAND filter is `height > ~-24 m`, NOT `>0`. Filter to
   land first so refracted/underwater points don't bias the shift. Manual:
   `outputfiles.rst`, `bundle_adjustment.rst` (geodiff `--csv-format`).
2. pc_align the pointmap onto the reference. DENSER cloud FIRST (the DEM), pointmap
   SECOND. Use `--compute-translation-only` so NO tilt/rotation is introduced (a pure
   shift can't tilt - ideal when you only want a vertical/horizontal offset removed and
   the pointmap is noisy/urban/water). Bound outliers with `--max-displacement`.
   CAUTION: a DEM reference is loaded as a full point cloud - SLOW and RAM-heavy;
   DOWNSAMPLE it first (`gdal_translate -tr`) and NEVER run pc_align on a pfe LOGIN
   node (little free RAM - it crawled at 0% loading a 260M-pt DEM there).
   `pc_align --max-displacement D --compute-translation-only \`
   `  --csv-format '1:lon 2:lat 3:height_above_datum' \`
   `  ref_downsampled.tif pm_land.csv --save-transformed-source-points -o align/run`
   Read the NED translation + Euler angles from the log (translation-only -> Euler ~0).
   Manual: `tools/pc_align.rst`.
3. Apply the transform to the CAMERAS (no re-optimization) -> new .adjust cameras:
   `bundle_adjust <images> <cameras> -t <session> \`
   `  --initial-transform align/run-transform.txt --apply-initial-transform-only \`
   `  -o ba_align/run`
   DIRECTION (the footgun): when the REF (denser) is FIRST in pc_align, apply
   `run-transform.txt`; if the order is reversed (your cloud first), apply
   `run-inverse-transform.txt`. ALWAYS VALIDATE, never trust the rule blind: geodiff
   pc_align's `run-trans_source` (= EXACTLY what the aligned cameras triangulate) vs
   the ref - the median should collapse to ~0. If it doubled, use the other transform.
   Manual: `bundle_adjustment.rst` ("This alignment can then be applied to the cameras
   as well", ~L309-326) and `tools/bundle_adjust.rst` (`--initial-transform`,
   `--apply-initial-transform-only`).
4. Downstream `mapproject` / `parallel_stereo` then take `--bundle-adjust-prefix
   ba_align/run` to use the aligned cameras. (For DG/WV, `-t dg`.)

## Convergence angles - ASP computes them (bundle AND stereo)

`bundle_adjust` writes `run-convergence_angles.txt`: per image pair, the 25/50/75
percentile ray-convergence angle (deg) + match count. `parallel_stereo`'s
preprocessing (stereo_pprc) also reports a pair's convergence. Independent geometry
cross-check from the delivery XML: unit vector `u = [cos(El)sin(Az), cos(El)cos(Az),
sin(El)]` from `MEANSATEL`/`MEANSATAZ`, `conv = arccos(u1.u2)`; this agreed with ASP
to ~0.1 deg on the WV-3 set. Wide-baseline / longer-dt pairs have larger convergence
-> better height sensitivity, but also show jitter most strongly in the tri-error.
Along-track banding in `point2dem --errorimage` output + systematic per-pair DEM
disagreement = jitter (next step: `jitter_solve`). Manual: `tools/bundle_adjust.rst`.

## MAPPROJECT-FIRST after ANY camera generation (the surest blunder-catch in stereo)

The VERY FIRST thing to do after generating or modifying a camera - `sat_sim`, `cam_gen`,
`bundle_adjust`/`jitter_solve` output, a hand-built CSM/RPC/tsai, a converted ISIS/DG camera -
is MAPPROJECT the image onto a reference surface and COMPARE it to an INDEPENDENT reference
(that DEM's hillshade, an existing ortho, a different-instrument image of the same ground),
BEFORE any stereo/BA/point2dem. This catches gross blunders on step ONE - the worst being a
LEFT-RIGHT / up-down FLIP/MIRROR from a wrong cross-track sample-axis or time sign, which is
insidious because the stereo can be INTERNALLY consistent (great tri-err) yet globally
REFLECTED, and pc_align (rigid, no reflection) can NEVER register it - so it masquerades as an
un-removable "warp" downstream.
The FULL procedure and toolkit for this now live in the inspection cluster (factored out of
this skill): load **[[dem-sanity-check]]** (the mapproject-vs-independent-reference test, the
crater pose-cluster flip detector, and the key mechanism - bundle adjustment CANNOT reflect, so
a mirror must be fixed by flipping the IMAGE, never by a GCP-column change alone) and
**[[visual-inspection]]** (warp-to-common-grid, hillshade, overlay mechanics). Rule of thumb to
keep here: cam-gen -> mapproject -> compare to an independent ortho/hillshade -> ONLY THEN proceed.

## USE YOUR EYES - EYEBALL EVERY PRODUCT, EVERY STEP (the #1 rule)

This cannot be overstated. ASP/VW/geospatial tools are FRAGILE and fail SILENTLY -
a water mask that keeps the coral and drops the houses, a correlation that locked
onto noise, a mapproject onto the wrong grid, a pc_align that found a spurious
rotation. The recurring, expensive mistake is running a multi-step pipeline WITHOUT
LOOKING, so a bad product at step 2 is only discovered 10 steps later after hours of
wasted compute. Claude HAS EYES (Read an image and see it) - so use them constantly:
- For EACH product (image OR DEM - both are visually inspectable), FIRST state the
  HYPOTHESIS (what it must look like: "the land/water mask keeps the runway, landfill
  and houses and drops the coral/underwater"; "the aligned hillshades overlay in
  yellow"), THEN colorize/hillshade -> downsample to PNG -> LOOK to CONFIRM before
  moving on. Do not proceed on an unconfirmed product.
- To compare two rasters by eye, first `gdalwarp` BOTH to the SAME grid, extent, and
  projection, then hillshade/colorize each to PNG - only then is the side-by-side or
  overlay apples-to-apples (mismatched grids make the comparison meaningless).
- Masks especially: overlay the mask on the source image (or show masked vs raw) and
  verify the land/water boundary is right at the shoreline, not eating land or keeping
  water. otsu/KDE thresholds are fragile; never trust one unlooked-at.
Frequent visual inspection is not overhead - in mapping work it IS the work.
