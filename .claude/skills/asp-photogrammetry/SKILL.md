---
name: asp-photogrammetry
description: ASP/VW photogrammetry tool usage and knowledge - stereo/mapproject resolution, pc_align, point2dem --errorimage and tri-err cutoffs, dem_mosaic, gdalwarp/proj.db, disparitydebug, robust median/MAD stats, hillshade alignment judging, dh/dv/dz notation, BA/jitter stats, CSM model-state JSON, derived-raster naming, sparse_disp, and the asp_manual.sh/asp_scripts pointers. Load before running any ASP or VW photogrammetry tool (stereo, bundle_adjust, jitter_solve, mapproject, point2dem, pc_align, dem_mosaic, geodiff).
---

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
