---
name: asp-photogrammetry
description: The ASP/VW photogrammetry HUB - the cross-cutting pipeline conventions and how the tools interplay, routing to the tool specialists. Carries stereo/mapproject resolution (correlation is native-res), the same-tr/-t_srs and tr-in-meters mapproject rules, point2dem --errorimage and tri-err cutoffs (before/after jitter), the DEM min/NoData pre-flight check, robust median/MAD stats, hillshade alignment judging, dh/dv/dz notation, disparitydebug --raw, derived-raster naming, sparse_disp, mapproject-first blunder catch, and the asp_manual.sh/asp_scripts pointers. Load before running any ASP/VW photogrammetry tool; it points to bundle-adjust, pc-align, gdal-rasters, match-plot, csm-models, jitter-solve, dem-comparison, visual-inspection for tool-specific depth.
---

## DigitalGlobe / WorldView (WV1/2/3, incl. green MS): ALWAYS linescan, NEVER RPC (CRITICAL)

For ANY DigitalGlobe/Maxar/Vantor product (WorldView PAN and MS, including the WV3
green-band work) we ALWAYS use the LINESCAN camera model, never RPC. This is a
standing rule for DG, not a per-task choice. Mechanics (verified against ASP docs,
:numref:`dg_csm` in tutorial.rst + bundle_adjustment.rst):
- WorldView linescan cameras use the CSM model INTERNALLY, but the ASP session name
  is still `-t dg` (or `-t dgmaprpc` for mapprojected-RPC input), NOT `-t csm`. The
  INPUT to bundle_adjust is the image + the delivery XML: `bundle_adjust -t dg
  <img.tif> <img.xml> ... -o ba/run`. RPC (`-t rpc`) is a smooth polynomial fit that
  SMOOTHS OUT jitter - do not use it when jitter/CCD artifacts must stay visible.
- bundle_adjust (and jitter_solve) OUTPUT is an adjusted CSM MODEL STATE per camera:
  `ba/run-<image>.adjusted_state.json` (refined position + orientation baked in).
  So: input = DG linescan-from-XML (`-t dg`); output = adjusted CSM linescan state.
- Downstream (mapproject, parallel_stereo, point2dem) USE those `.adjusted_state.json`
  files directly, with session `-t csm` (they are self-contained CSM cameras - no
  `--bundle-adjust-prefix`, no original XML needed). Alternatively the `.adjust` files
  can be used with the original cameras and `-t dg`, but prefer the state files.
- Atmospheric refraction + velocity aberration are auto-corrected on load, making the
  DG linescan very close to the vendor RPC; bundle_adjust + pc_align still recommended.
- Native GSD for mapproject: read `MEANPRODUCTGSD` from each image's XML (WV3 MS is
  ~1.2-1.4 m; do NOT round to 2 m - use the published value). Left and right MUST share
  the SAME `--tr` (see the mapproject-grid rule below) so the mapprojected pair overlays
  pixel-for-pixel. A subpixel image correction (e.g. wv_correct dx/dy centered at 0)
  does NOT change the camera, so no re-bundle is needed after applying it - reuse the
  same adjusted_state.json cams and re-mapproject.
- Reference DEM for mapproject/align: we FETCH our own (standard practice), typically
  Copernicus GLO-30 over the footprint, then dem_geoid --reverse-adjustment (EGM2008
  geoid height -> ellipsoid) + a light blur. We do not assume one is staged.

## Key interplay: feed downstream tools the baked adjusted cameras, not --bundle-adjust-prefix

The pipeline principle tying bundle_adjust to everything downstream: bundle_adjust bakes
its adjustment into a standalone camera (`<pfx>-<image>.adjusted_state.json` for CSM,
automatic; `--inline-adjustments` for pinhole `.tsai`), and mapproject / parallel_stereo /
point2dem / dem2gcp / jitter_solve take THAT file directly - never the original camera plus
`--bundle-adjust-prefix` (passing both DOUBLE-APPLIES the adjustment). Full mechanics,
parallel_bundle_adjust, match caching, and residual-pointmap inspection: **[[bundle-adjust]]**.

## bundle_adjust (and stereo) ID images by BASENAME - same-basename inputs collide

ASP identifies each image by its basename (no dir, no extension). Two inputs that share a
basename - e.g. every scene's dg_mosaic green is `ms_mos.r100.b3.tif` in a different orderid
dir - fail bundle_adjust with `ERROR: Found duplicate image: ms_mos.r100.b3`. Fix: stage
unique-named COPIES (not symlinks) `<oid>_green.{tif,xml}` co-located in one inputs dir, and
feed those. The camera XML must share the image's basename so `-t dg` pairs them.

## Replicating a published pipeline: use the pipeline AT EVERY STEP, eval each stage's product

When reproducing results from a published multi-stage pipeline (e.g. CassisPipeline)
with one changed knob (a distortion, a parameter), run the PIPELINE ITSELF for the
FULL stage range - do NOT (a) hand-roll your own eval (the pipeline already publishes
an evaluable product at each stage - read those, e.g. its own dz/dd `eval*/`), or
(b) mix intermediates from DIFFERENT runs/alignments (regenerating only some stages
and reusing older copied outputs silently breaks self-consistency - e.g. a fresh
stage-1 alignment with a copied stage-4 camera set puts the bootstrap DEM and the
cameras in different frames, mildly degrading good sites and catastrophically
breaking fragile ones). To change one knob, edit ONLY that config value and run the
full pipeline so every stage is consistent. EVALUATE each stage's published product
as it lands to localize any divergence early, never run blind through all stages and
eval only at the end. (Burned on the CaSSIS unc10relax 5-site replicate 2026-09-05:
a "run stage 1 then 5-7 with copied stage-4 cams" shortcut broke ox2 to 91 m dz-std.)

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

## gdal "Cannot find proj.db" is a HARD STOP, never cosmetic (CRITICAL)

Any `PROJ: Cannot find proj.db` warning means the env has NO PROJ data - STOP, do not
proceed. Every projection op (`-t_srs`, gdalwarp, reprojection, geodiff across datums,
mapproject) then produces WRONG, subtly-broken georef you cannot reliably catch later.
Ensure PROJ is set up front for EVERY gdal/ASP invocation (local: `conda activate
asp_deps`; pfe: export `PROJ_DATA`/`PROJ_LIB` to the packaged share dir in the script
header). Full env recipes + the after-warp CRS verify: **[[gdal-rasters]]**.

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

## mapproject --tr is in METERS, not degrees, even for a longlat DEM (CRITICAL gotcha)

`mapproject`'s `--tr` (grid size) is in the units of the OUTPUT projection, and mapproject
picks a METRIC projection by default. Even when the input DEM is geographic (longlat), ASP
"finds a projection in meters first" (`docs/tools/mapproject.rst`, :numref:`mapproj_auto`),
so the output is UTM/stereographic in meters and `--tr` must be METERS (e.g. `--tr 30`), NOT
degrees. Passing a degree-scale value (e.g. `--tr 0.0006`) makes it fail at the
`--query-projection` step with: `The user-set grid size (option --tr) is so small that likely
it is in degrees, while meters are expected.` (The wrapper then raises `Failed executing:
mapproject_single --query-projection ...`, which looks like a camera/DEM error but is just the
units.) FIX: give `--tr` in meters, or set `--t_srs` explicitly to the metric CRS you want.
This differs from `point2dem`/`gdalwarp`, whose `--tr`/`-tr` follow whatever `--t_srs`/`-t_srs`
you pass (degrees if longlat). Burned 2026-09-07 on the KH-7 browse-res replicate (`--tr 0.0006`
in degrees -> "could not sample"/grid-too-small; `--tr 30`/`50` meters fixed it).

## dem_mosaic: Call With `-o output.tif`, Not `-o out`

Recent `dem_mosaic` writes the given name directly when `-o` ends in `.tif` (e.g. `-o mosaic.tif` -> `mosaic.tif`); a bare `-o out` produces the OLD `out-tile-0.tif`. ALWAYS pass the honest `.tif` output name (this is the PREFERRED newer usage) and reference that file later - NEVER the bare-prefix form. This keeps recurring when copying older scripts; when you write or reuse a `dem_mosaic` call, make `-o` end in `.tif`.

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

## Dense matches from a stereo disparity (num-matches-from-disparity)

To get dense raw-image matches for residual/refraction BA or jitter, run parallel_stereo
WITH cameras on the mapproj pair with `--num-matches-from-disparity` - it unprojects the
dense disparity to the RAW images. Which output file to use, the aligned-domain trap, the
mapproj-DEM-must-be-clean nodata poison (0 matches while tri looks fine), and the always-
eyeball rule: **[[bundle-adjust]]**. A cheap alternative that needs NO matches is standard
bathy stereo (`--left/right-bathy-mask` + `--bathy-plane` + `--refraction-index`).

## point2dem --errorimage Always; Mosaic the Error Too

**"Triangulation error" and "intersection error" are the SAME thing** - the closest
distance between the two camera rays at the triangulated point, written by
`point2dem --errorimage` as `<prefix>-IntersectionErr.tif`. The terms are used
interchangeably (prefer "triangulation error" in prose per Oleg's style note). It is
the go-to 2D diagnostic: jitter, lens distortion, misregistration, blunders, and CCD
artifacts all show up in it. CCD artifacts appear as VERTICAL (along-column) streaks -
and to SEE them, colorize the tri-err AND make a `gdaldem hillshade` of the DEM (GDAL's
hillshade reveals jitter/CCD streaks far better than ASP's hillshade).

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
  used for CaSSIS/CTX/Viking/TMC/OHRC. Two refinements (RST next_steps.rst
  :numref:`mapproj-example`, learned on KH-7 Vale 2026-09-18):
  (1) **Match the L/R grids with `--ref-map`, don't hand-set --tr.** Mapproject the
  LEFT with NO `--tr` (it auto-guesses the native image GSD - e.g. KH-7 sub8 = 5.76 m),
  then mapproject the RIGHT with `--ref-map L_map.tif` so it borrows L's exact
  projection + grid size. parallel_stereo REQUIRES both mapproj images share grid/proj.
  (2) **Mapproject onto a SMOOTH/BLURRED low-res seed DEM, not a sharp one.** A sharp
  seed's fine detail imprints artifacts into the correlation/final DEM. Blur the seed
  with `dem_mosaic --dem-blur-sigma 5 seed.tif -o seed_blur.tif` (hole-fill first if it
  has holes) and mapproject onto seed_blur; the stereo then recovers real terrain,
  esp. on steep slopes, instead of draping COP's texture. Use the BLURRED seed only for
  the stereo mapproject; use the REGULAR (sharp) reference DEM for jitter/BA
  heights-from-dem and anchors.
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
- **asp_mgm (and any non-BM algorithm, and non-BM alignment like local_epipolar)
  can run ONLY via `parallel_stereo`, NOT the plain `stereo` wrapper** - plain
  `stereo` errors "Alignment method 'local_epipolar' and/or other algorithms
  except ASP_BM can be used only with parallel_stereo". Plain `stereo` (single
  process, all stages in one process) works ONLY for `--stereo-algorithm asp_bm`.
  So: want asp_mgm on a RAM-limited box (Mac) -> use `parallel_stereo --processes 1`
  (or 2), which runs tiles with few workers = bounded RAM; each `stereo_corr` worker
  can use 1-3 GB, so N processes ~ N*(1-3) GB - keep N small on the Mac. `--processes 1`
  is the safe low-RAM choice; try 2 if RAM allows. Watch for LEFTOVER/zombie stereo_*
  processes from killed or re-invoked runs: they STACK and OOM the Mac (`pgrep -fl
  install/bin/stereo`; `pkill -9 -f install/bin/stereo`). NEVER wrap parallel_stereo in
  a re-invoke loop - each call spawns fresh workers while old ones linger = OOM.
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

CSM model-state / `.adjusted_state.json` files are NOT plain JSON: a model-name line comes
FIRST, then the JSON (so `json.load` fails; skip line 1). Field layout, frame vs linescan,
parsing, and inspection: **[[csm-models]]**.

## Aligning bundle-adjusted cameras to a reference DEM

To seat BA cameras on a reference DEM/lidar (remove a residual camera-vs-ground offset)
without re-solving the bundle: geodiff the BA pointmap vs the ref, pc_align the
land-filtered pointmap onto the ref with `--compute-translation-only`, and apply the
transform to the cameras (mind the direction footgun, validate by geodiff-ing
`run-trans_source`). Full workflow (geoid/land filter, downsample-the-ref, direct-vs-inverse
validation): **[[pc-align]]**.

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

## Related specialists (this hub routes to them)

Tool-specific depth was factored out - load the specialist when you go deep on a tool:
- **[[bundle-adjust]]** - match files (--mapprojected-data, the three sets), dense matches
  from disparity (+ the mapproj-DEM nodata trap), adjusted_state.json vs --bundle-adjust-prefix,
  parallel_bundle_adjust, match caching, residual-pointmap inspection, pose blunder catch.
- **[[pc-align]]** - alignment methods, denser-cloud-first, apply-to-DEM/cameras, the
  direct-vs-inverse footgun, seating BA cameras on a reference DEM.
- **[[gdal-rasters]]** - gdalwarp/gdal_translate/gdalinfo, common-grid warp, resampling
  choice, the PROJ/proj.db env, float-PNG quick-looks.
- **[[match-plot]]** - plotting any .match/tie points (red filled balls, no lines, the tool).
- **[[csm-models]]** - the model-state .json format, fields, distortion, inspection.
- **[[jitter-solve]]** - refining linescan per-line poses (knots, anchors, --mapproj-dem).
- **[[dem-comparison]]** / **[[dem-sanity-check]]** / **[[visual-inspection]]** - dh/dv/dz,
  flip/mirror detection, and the eyeball/overlay procedures.
