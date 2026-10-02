---
name: sfs-run-align
description: >-
  Run Shape-from-Shading on a full site and co-register every image to the SfS terrain.
  The stage AFTER the camera refine chain, whacky-camera prune, and SfS image-subset: tile
  the reference DEM and run parallel_sfs per tile then dem_mosaic-assemble one SfS DEM;
  then per image render an SfS-simulated view, image_align it to measure the pixel shift,
  and gcp_gen a corrective GCP; and hillshade-correlate the SfS DEM against the reference
  (LOLA) to get the global horizontal shift gate. Load when running SfS over tiles,
  assembling a site SfS DEM, measuring per-image registration to the SfS terrain, producing
  GCPs for a joint re-solve / trans_gcp, or gating whether to re-register SfS to LOLA.
---

# Run SfS over a site, then co-register every image to it

The stage AFTER [[sfs-post-bundle-eval]] (refine chain + whacky prune) and
[[sfs-image-selection]] (the SfS subset). Two phases: build ONE full-site SfS DEM, then
measure and correct each image's registration to it. Distilled from the BCU2314-BDU1224
lunar polar LRO NAC run (2026-10-01), which executed cleanly end to end. Work in ONE fixed
pfe work dir, paths relative. Reusable scripts live in `~/projects/sfs/`. All ASP tools are
`bin/` wrappers that self-wire the env, so the scripts' hardcoded dead `asp_deps` ISISROOT
is inert on pfe (see the [[sfs]] "pfe env" note). pfe needs an explicit `-q` queue;
`normal` caps at 8 h, `long` allows 120 h (e2305/s2764 are in its ACL). qsub over ssh is
`/PBS/bin/qsub`.

## Phase 0 - global exposures (do FIRST, it gates everything)

One exposures file, reused by every SfS tile and every sim render. Without it, dark images
render as blank sim/SfS. `sfs_exposures.sh <imageList> <baPrefix> <dem> <sfsDir> <currDir>`
runs `sfs --compute-exposures-only` (single process) and writes `<sfsDir>/run-exposures.txt`.
```bash
/PBS/bin/qsub -q normal -m n -r n -N sfsexp -l walltime=4:00:00 -W group_list=e2305 \
  -j oe -S /bin/bash -o $W/ -l select=1:ncpus=28:model=bro_ele -- \
  ~/projects/sfs/sfs_exposures.sh lists/secondary_images.txt ba_htdem/run \
    ref/lola_1mpp_extra_noblur.tif exposures_sec $W
```
Gate: job F + `run-exposures.txt` has one nonzero value per image. ~335 images took ~15 min
single-process; ~978 took ~42 min. A few zero exposures = all-shadow frames (fine, they just
won't contribute).

## Phase 1 - SfS per tile, then assemble

Two-level tiling: OUTER tiles by `tile_dem.py` (each run as its own qsub), INNER sub-tiles by
parallel_sfs within the node set. Manual outer tiling is more robust than one giant
parallel_sfs (a failed tile reruns alone; small jobs schedule faster).

1. **Tile** (run under the `geo` conda env - `tile_dem.py` uses bare `gdal_translate` +
   `osgeo`, NOT wrapper-mediated): `tile_dem.py <dem> 4000 4000 tiles 200` -> ~20 padded
   outer tiles `tiles/tile-10001.tif ...` for a ~19.5x15.5 km / 1 m site. 4000 size + 200 pad
   are the lunar defaults.
2. **Per-tile parallel_sfs**, one qsub per tile, via `launch_sfs_tiles.sh` (do NOT hand-roll):
```bash
IMAGE_DIR=lronac_all IMAGE_SUFFIX=.cal.echo.cub NUM_CPU=4 NUM_NODES=2 \
DEM_WEIGHT=0.0025 SHADOW_THRESH=0.005 WALLTIME=16:00:00 QUEUE=long \
MODEL=bro_ele NCPUS=28 QSUB_BIN=/PBS/bin/qsub \
~/projects/sfs/launch_sfs_tiles.sh tiles lists/secondary_images.txt ba_htdem/run \
  exposures_sec/run-exposures.txt sfs 0 $W
```
   It copies the exposures into each `sfs/clip<id>/` (so parallel_sfs REUSES them, no
   recompute), and each tile writes `sfs/clip<id>/run-DEM-final.tif`. Reflectance-type 1
   (Lunar-Lambert), smoothness 0.08, initial-dem-constraint 0.0025, inner tile-size 200 /
   pad 50 are baked in. MEASURED: 4000 tile on 2 bro_ele nodes ~6.5-9.6 h; 20 tiles ~350
   node-hours (~350 SBU). NB `launch_sfs_tiles.sh` does NOT forward `--low-light-threshold`
   (parallel_sfs default is used); set it explicitly if low-light regions look wrong.
3. **Assemble**: BLENDED `dem_mosaic` (NO `--max`, so tile seams smooth) of all
   `sfs/clip*/run-DEM-final.tif` -> one SfS DEM, via `dem_mosaic_list.sh` (qsub, THREADS=28).
   dem_mosaic writes `<prefix>-tile-0.tif` for a single output tile - rename it to the
   canonical `sfs_dem.tif`.
4. **INSPECT** (mandatory, [[visual-inspection]]): `geodiff sfs_dem.tif <refDEM> --threads 1`
   -> dz stats (expect mean ~0, std sub-meter: SfS stays centered on the constraint while
   adding detail); hillshade + a scaled-dz quick-look -> look for TILE SEAMS (a 5x4 grid
   pattern = bad blend), flips, or dropouts. BCU2314: mean dz -0.0008 m, std 0.69 m, no
   seams.

## Phase 2 - per-image sim-align: shift measurement + GCP

For EVERY image with a camera (including the whacky-pruned ones - the point is to MEASURE
them), render an SfS-sim view through its camera, `image_align` the measured mapproj to the
sim to record the (dx,dy) shift, and `gcp_gen` a corrective GCP. Doc: sfs_usage.rst
`sfs_sim`. First recompute exposures over the FULL image+camera set on the FINAL `sfs_dem.tif`
(Phase 0 was on the reference DEM / subset): same `sfs_exposures.sh` call with the full list
and `sfs_dem.tif`, prefix e.g. `exposures_all/run`.

Batcher `batch_sfs_sim.sh imageList beg end sfsDem imgDir baDir simDir currDir [mapList] [procs]`
(xargs-parallel, per-image subdir `simDir/<id>/`), worker `sfs_sim_align.sh`. Chunk the list
(~100/chunk, one qsub each, 1 bro_ele node, procs 8):
```bash
IMAGE_DIR unused here; env: EXPOSURES_PREFIX (REQUIRED), SHADOW_THRESHOLD, ALIGN_THRESH.
/PBS/bin/qsub -q normal ... -v ALIGN_THRESH=0,EXPOSURES_PREFIX=exposures_all/run,SHADOW_THRESHOLD=0.005 -- \
  ~/projects/sfs/batch_sfs_sim.sh lists/filtered_images.txt <beg> <end> sfs_dem.tif \
    lronac_all ba_htdem sim_eval $W
```
CRITICAL GOTCHAS:
- **`ALIGN_THRESH=0` to get a GCP for EVERY image.** The default `ALIGN_THRESH=2.0` exits
  after image_align (recording the shift) WITHOUT a GCP when the shift is < 2 px, so a
  default run emits GCPs only for the few misaligned frames. For a joint re-solve / trans_gcp
  you want ALL of them (a "stay put" GCP is still a constraint, and the solver benefits from
  the full set). `ALIGN_THRESH=0` forces gcp_gen (+ a per-image BA) on every image. The
  cheaper `--gcp-only` (gcp, no BA) is not forwarded by batch_sfs_sim yet.
- **Do NOT pass an empty-string `mapList` arg through qsub `--`**: qsub DROPS it, shifting
  `procs` into the mapList slot and tripping the pairing check (1-second Exit 1). Omit both
  trailing optional args to get mapList="" + procs=8.
- Re-runs are cheap: `sfs_sim_align.sh` REUSES an existing per-image `<id>.meas.map.tif` and
  `*-sim-intensity.tif`, so a second pass (e.g. flipping ALIGN_THRESH) only redoes
  image_align + gcp_gen.
- **No mapproj reuse across DEMs**: the sim mapproject must be onto `sfs_dem.tif` (the SfS
  terrain), not an earlier LOLA-DEM mapproj.

Products per image: `sim_eval/<id>/{<id>.meas.map.tif, *-sim-intensity.tif,
run-align-<id>-transform.txt (the dx/dy shift), <id>_gcp.gcp}`. Aggregate the transforms into
`shift_report.txt` (id dx dy |shift|, sorted desc). BCU2314 (944 measured): median 0.13 px,
p90 0.30 px - cameras are sub-pixel registered to the SfS terrain; a handful of outliers are
the whacky/culled frames (and ~33 unmeasurable = blank sim = the worst). The sim-shift is the
CLEAN, resolution-agnostic registration metric; see [[sfs-post-bundle-eval]] section 2b for
how it relates to the bundle mapproj-offset and native GSD (and why mapproj-offset alone is a
coarseness-confounded predictor).

## Phase 2b - SfS-vs-reference shift GATE (hillshade correlation)

Measure the GLOBAL horizontal shift of the SfS terrain vs the gridded reference (LOLA).
`hillshade_correlator.sh <sfsDem> <refDEM> <currDir> <stereoDir> [maxSearch=25]` hillshades
both with `hillshade -e 10` (ASP, grazing 10 deg = strong relief for correlation - NOT gdaldem;
both DEMs get the same treatment so the method cancels), runs
`parallel_stereo --correlator-mode --stereo-algorithm asp_mgm --corr-kernel 9 9`, and writes
`<stereoDir>/run-F_b1_nodata.tif` / `run-F_b2_nodata.tif` = the dx / dy disparity = the
horizontal SfS->ref shift. Heavy (asp_mgm over ~20k x 15k): qsub, 2 nodes, long queue, ~1 h.
```bash
/PBS/bin/qsub -q long ... -l select=2:ncpus=28:model=bro_ele -- \
  ~/projects/sfs/hillshade_correlator.sh sfs_dem.tif ref/lola_1mpp_extra_noblur.tif \
    $W hcorr_sfs_lola 25
```
Read-out: robust MEDIAN of run-F_b1/b2_nodata.tif (mask nodata; ~66% valid, shadows don't
correlate). The pc_align matrix translation it prints is an origin-vs-centroid artifact of a
tiny rotation about the far polar origin - IGNORE it, read the disparity medians. Add
`geodiff` for the vertical dz. BCU2314: dx median -2.15 m, dy -1.02 m, dz ~0 - a modest,
roughly uniform offset within LOLA's own several-meter slop at this latitude.

## trans_gcp - re-register SfS into the LOLA frame (the DEFERRED decision)

The per-image shifts (Phase 2) + this global shift (Phase 2b) together decide whether to
re-register. Negligible -> skip. Significant -> `trans_gcp.sh` feeds the per-image GCPs
through `dem2gcp --input-gcp-list ... --max-pairwise-matches 0` to move their ground coords
from the SfS frame into the LOLA frame -> one merged GCP for a final bundle_adjust (doc
`sfs_gcp`). Do this ONLY after the shift eval, and typically on the user's call - it is not
automatic.

Two things that raise GCP yield through the transfer:
- **A more-filled disparity transfers more GCP** (dem2gcp maps each GCP through the
  SfS->ref disparity; holes -> GCP lost at `--search-len 0`). But MEASURED on BCU2314
  (2026-10-01), `gdaldem hillshade -multidirectional` gave LESS fill than ASP
  `hillshade -e 10`, not more: 57% vs 66.6% valid. Multidirectional averages several
  azimuths so it fills shadows but WASHES OUT the directional contrast that
  hillshade-to-hillshade asp_mgm correlation lives on, and loses more matches to the flat
  contrast than it gains from un-shadowed pixels. So for this correlation PREFER the grazing
  single-azimuth `hillshade -e 10` (harsh shadows = high texture = more valid disparity).
  The median shift was ~identical either way (-2.2/-1.0 m), so the global offset is robust
  to the hillshade method. (Don't assume "multidirectional = more fill" for correlation -
  contrast matters more than shadow-fill here.)
- **A GCP on a no-disparity hole is THROWN OUT, not kept untransformed** (dem2gcp.cc
  `find_disparity` + the `if (!is_valid(disp)) continue;` in the main loop): it tries the
  interpolated then raw disparity at the pixel and, failing both, drops the point from the
  output. The `--search-len` option (DEFAULT 0) optionally searches an N-px neighborhood for
  the nearest valid disparity ("a desperate measure... should not be overused"); leave it 0
  to drop holes cleanly. So throw-out is the default and desired behavior - and it is exactly
  why the filled multidirectional disparity above matters (fewer holes = fewer drops).

## Autonomous execution notes (this pipeline is long and multi-stage)

Each stage is a qsub; gate the next on job_state=F (never on output-file existence - a PBS
product appears while still half-written). Drive it with a detached qstat-gated background
poll per job set (re-invokes once on completion) plus, if running unattended, an in-session
CronCreate heartbeat. Log SBU per stage (nodes x walltime x model-rate) and check
`acct_ytd` after big jobs, mindful of the Oct 1 fiscal-year allocation rollover. Detail:
[[autonomous-ops]], [[pfe-nas]], and the SBU note in [[sfs]].

## Related
[[sfs]] (parent: batch mapproject, the refine chain, exposures, SBU, the pfe-env note),
[[sfs-post-bundle-eval]] (the pre-SfS gate + the GSD/mapproj-offset/sim-shift metric
expertise), [[sfs-image-selection]] (the SfS subset), [[jitter-solve]], [[pc-align]],
[[dem-comparison]] (dh/dv/dz the right way), [[visual-inspection]], [[pfe-nas]],
[[autonomous-ops]].
