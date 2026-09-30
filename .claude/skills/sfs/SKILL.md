---
name: sfs
description: Shape-from-Shading (SfS) and the canonical batch mapprojection framework in ~/projects/sfs/. Load before running batch mapprojection across multiple images, setting up SfS illumination modeling, running parallel_sfs, or preparing tiles and orthoimages for photoclinometry.
---

# Shape-from-Shading (SfS) & Canonical Batch Mapprojection

This skill documents the Ames Stereo Pipeline Shape-from-Shading (SfS) photoclinometry toolset and the canonical batch mapprojection framework maintained in `~/projects/sfs/`.

## Do Not Reinvent the Wheel: Use the Standard Batch Mapproject Scripts

When preparing mapprojected images for SfS, bundle adjustment, GCP generation, or mosaic building across many images (from 10 to 1300+ images on large grids like 20k x 20k pixels):
- **NEVER** write ad-hoc mapprojection loops or custom submission scripts from scratch.
- **ALWAYS** use the robust, tested scripts in `~/projects/sfs/`:
  - `~/projects/sfs/batch_mapproject.sh`: Slices an image list into chunks (default 30 images per job) and submits concurrent PBS jobs to Pleiades/Athena.
  - `~/projects/sfs/mapproject_chunk.sh`: Worker script executed on each compute node to mapproject its assigned slice of images.

### Features of the Canonical Scripts
1. **Arbitrary Sensor Support**:
   - Seamlessly handles LRO NAC product IDs (`M...LE/RE`), Chandrayaan-2 OHRC (`ch2_ohr_...`), MRO CTX, or full/relative filesystem paths to `.cub` or `.tif` files.
2. **Flexible Camera Pairing**:
   - **Glob Mode**: Camera for image id `F` is resolved via `${baPrefix}*${F}*.json`.
   - **Paired List Mode** (optional argument `cameraList.txt`): Line $N$ of `cameraList.txt` matches line $N$ of `imageList.txt`, allowing cameras from arbitrary directories, subfolders, or differing naming schemes.
3. **Resolution Configuration (`--tr`)**:
   - Set `TR=<meters>` (e.g. `TR=0.5` or `TR=1.0`) to mapproject all images to a constant, identical ground resolution.
   - Set `TR_COL=<col_idx>` to extract per-image native resolution from that column of `imageList.txt`.
   - Unset defaults to legacy `--tr 1`.
4. **Target Extent (`--t_projwin`)**:
   - Set `PROJWIN="<xmin> <ymin> <xmax> <ymax>"` to constrain all mapprojections to a specific bounding box (e.g. a 3 km site or 20k x 20k regional tile).
   - When unset, each image is mapprojected across its full intersection with the DEM.
5. **Robust Error Handling**:
   - Catches out-of-range footprint exceptions non-fatally (e.g. images that do not intersect the bounding box or produce unphysical horizon ray projections).
   - Cleans up 0-byte partial GeoTIFFs and leftover `*_tif_tiles` scratch directories so failed runs leave no stale artifacts.
   - Disables core dumps (`ulimit -c 0`) to prevent disk exhaustion.
   - Automatically skips already-completed, non-empty GeoTIFFs (`-s "$map"`).

---

## Running Batch Mapprojection

### Invocation Syntax

```bash
# Basic usage with baPrefix glob
~/projects/sfs/batch_mapproject.sh dem.tif imageList.txt baPrefix mapDir currDir

# Paired list usage with explicit camera list
~/projects/sfs/batch_mapproject.sh dem.tif imageList.txt ignored mapDir currDir cameraList.txt
```

### Environment Variables for `batch_mapproject.sh`

Control job dispatch and mapprojection settings via environment variables:

| Variable | Default | Description |
| :--- | :--- | :--- |
| `TR` | *(unset)* | Fixed grid resolution in meters/pixel (e.g. `TR=0.5`) |
| `TR_COL` | *(unset)* | Column in `imageList.txt` with per-image native GSD |
| `PROJWIN` | *(unset)* | Target extent `"xmin ymin xmax ymax"` |
| `CHUNK_SIZE` | `30` | Number of images per PBS worker job |
| `PROCESSES` | `10` | Worker processes per node inside `mapproject` |
| `THREADS` | *(ASP default)* | Threads per process inside `mapproject` |
| `TILE_SIZE` | `2048` | Tile dimension for parallel mapprojection |
| `NO_MOSAIC` | *(unset)* | Set to 1 to skip the per-chunk `dem_mosaic --max` |
| `EXTRA_OPTS`| *(unset)* | Additional flags passed directly to `mapproject` |
| `MODEL` | `bro_ele` | PBS node architecture (`bro_ele`, `tur_ath`, etc.) |
| `NCPUS` | `28` | Number of CPUs requested per PBS node |
| `WALLTIME` | `6:00:00` | PBS walltime limit per job |
| `IMG_DIR` | `img` | Directory prefix for relative image paths |
| `IMG_EXT` | `.cal.echo.cub` | File extension for image paths |

### Example: Mapprojecting OHRC and LRO NAC on a 3 km Grid

```bash
cd /nobackupp19/oalexan1/projects/my_project

export TR=0.5
export PROJWIN="-12500 -10500 -9500 -13500"
export NO_MOSAIC=1
export CHUNK_SIZE=30
export WALLTIME="04:00:00"

~/projects/sfs/batch_mapproject.sh \
  ref_dem.tif \
  lists/images.txt \
  ignored \
  maps_3km \
  $(pwd) \
  lists/cameras.txt
```

---

## End-to-End Polar LRO NAC Matches Pipeline (precise invocations)

This is the proven sequence for producing interest-point matches over a delivery
box from a large azimuth-sorted LRO NAC set (used on the m2m and BCU sites). Each
step is spelled out so it is not rediscovered. All lists stay in one solar-azimuth
order, one line per image, with image/camera/mapprojected lists in exact
correspondence. Work in ONE fixed work dir on pfe, paths relative.

**Step 0 - query and download candidate LRO NAC images.** Query the PDS ODE REST
API for all NAC images intersecting the site using `query_lro.py` (or `query_lro.sh`).
Pass either the bounding box coordinates or the site DEM directly, with optional
filters for incidence angle (low sun) and ground resolution. Output direct `.IMG`
download URLs for batch downloading and clean product IDs:

```bash
# Query ODE by lat/lon box, emitting download URLs and product IDs
~/projects/sfs/query_lro.sh \
  --lat-lon -84.95 -84.45 20.7 27.0 \
  --min-incidence 70 --max-incidence 90 \
  --max-resolution 1.5 \
  --output-urls lists/urls.txt \
  --output-products lists/products.txt

# Or query directly using the reference DEM bounding box (plus 1 km margin)
~/projects/sfs/query_lro.sh \
  --dem ref/lola_1mpp_extra.tif --margin-km 1.0 \
  --min-incidence 70 --max-incidence 90 \
  --output-urls lists/urls.txt \
  --output-products lists/products.txt

# Download images in bulk (resumable with retries)
~/projects/sfs/download_all.sh lists/urls.txt
```

**Step 1 - reference DEM (1 m/pixel, half-integer grid).** Regrid the LOLA source
(for lunar 83-90 South use Barker LDEM_83S_10MPP_ADJ.TIF at 10 m/pixel; the 5 m
product only reaches 87-90 South) to 1 m/pixel with cubic spline, ASP 256-block
tiling, and a `-te` snapped OUTWARD to half-integer edges so 1 m pixel centers
land on integers (required later by `sfs_blend`, :numref:`terrain_bounds`).

**DEM naming rule (honesty).** A DEM's name must encode its state, because mapproject
BURNS the DEM path into every output (`DEM_FILE=...` in the geoheader) and later tools
look it up. Two orthogonal axes: EXTENT (`_extra` = the product box PADDED, e.g. +2 km,
the working DEM mapproject and BA actually use since footprints spill past the ROI; a bare
name would be the exact product-box DEM) and PROCESSING. `_extra.tif` means the honest
regrid with NO blur or other processing. ANY operation applied to a DEM (blur, fill, ...)
MUST be reflected in the name (`_extra_blur.tif`, `_extra_fill.tif`, ...): never let a
processed DEM hide behind a plain name. Use the honest unprocessed `_extra` DEM for
everything downstream, mapproject AND the `--heights-from-dem` constraint. Do NOT blur by
default: a sigma-2 blur smears fine terrain and adds bias while barely moving the
statistics on km-scale relief, so it does not make the DEM better. Blur only if LOLA
spikes actually cause artifacts, and use the blurred DEM only as a mapproject drape
surface, never as the height constraint. IMPORTANT defensive exception: if a plain name
was ALREADY burned into products as a DIFFERENT (processed) DEM, do NOT recycle that name.
Vacate it (leave no file at that path) and use explicit `_noblur` / `_blur` names, so a
burned-in lookup FAILS LOUD instead of silently resolving to the wrong terrain. (This
project did exactly that: its maps carry `DEM_FILE=ref/lola_1mpp_extra.tif` from when that
name held the blurred DEM, so the honest DEM is now `ref/lola_1mpp_extra_noblur.tif`, the
blurred one is `ref/lola_1mpp_extra_blur.tif`, and bare `_extra.tif` is deliberately gone.)

```bash
proj="+proj=stere +lat_0=-90 +lon_0=0 +k=1 +x_0=0 +y_0=0 +R=1737400 +units=m +no_defs"
gdalwarp -overwrite -r cubicspline -tr 1 1 -t_srs "$proj" \
  -te 71240.5 162789.5 90731.5 178256.5 \
  -co COMPRESSION=LZW -co TILED=yes -co INTERLEAVE=BAND \
  -co BLOCKXSIZE=256 -co BLOCKYSIZE=256 -co BIGTIFF=yes src.tif ref/lola_1mpp_extra.tif
# optional, only if spikes bite; blurred DEM is for mapproject drape ONLY:
# dem_mosaic --dem-blur-sigma 2 ref/lola_1mpp_extra.tif -o ref/lola_1mpp_extra_blur.tif --threads 1
```

Build this on a compute node (devel), not the head node. Confirm 100% valid and a
half-integer origin with `gdalinfo -stats`.

**Step 2 - azimuth-sorted lists.** Get per-image sun azimuth (`sfs --query`, see
the sfs-azimuth skill and `query_azimuth.sh`), then sort by the 0-360 azimuth
column and derive matching image and camera lists:

```bash
sort -k3,3 -n azimuth_tables.txt > lists/azimuth.txt
awk '{print $1}' lists/azimuth.txt > lists/azimuth_images.txt
sed 's/\.cal\.echo\.cub$/.cal.echo.json/' lists/azimuth_images.txt > lists/azimuth_cameras.txt
```

Azimuth sorting is what makes `--overlap-limit` in the next BA match images of
similar illumination (matched shadows), which is what co-registration needs.

**Step 3 - batch mapproject (embarrassingly parallel, one node per chunk).** Leave
`TR` UNSET so it uses legacy `--tr 1` (still 1 m/pixel) whose output name
`<id>.cal.echo.map.tr1.tif` is exactly what `bundle_adjust.sh` expects; setting
`TR=1` names them `<id>.map.tif` and BA finds none. Full-path image lists let the
worker use the cubes directly. On pfe a non-interactive ssh has no `qsub` on PATH,
so pass `QSUB_BIN=/PBS/bin/qsub`.

```bash
cd /nobackupp19/oalexan1/projects/<site>
export QSUB_BIN=/PBS/bin/qsub
export PROJWIN="71240.5 162789.5 90731.5 178256.5"
export NO_MOSAIC=1
export CHUNK_SIZE=121   # images per one-node job; sized to yield ~10 jobs
export MODEL=bro_ele
export WALLTIME=4:00:00
~/projects/sfs/batch_mapproject.sh ref/lola_1mpp_extra_noblur.tif \
  lists/azimuth_images.txt ignored maps $(pwd) lists/azimuth_cameras.txt
```

**Step 4 - cull, then rebuild lists in the same azimuth order.** Non-intersecting
footprints leave no file; all-shadow frames have a near-zero maximum. Keep frames
whose `gdalinfo` maximum is at or above the LRO NAC lit-vs-shadow cutoff (0.005),
preserving azimuth order. INSPECT the max-value distribution before trusting the
threshold (:ref: the sfs-azimuth and visual-inspection skills), do not assume it.

```bash
sed 's#lronac_all/\(M[0-9]*[LR]E\)\.cal\.echo\.cub#maps/\1.cal.echo.map.tr1.tif#' \
  lists/azimuth_images.txt > lists/azimuth_map.txt
~/projects/sfs/filter_by_max.sh lists/azimuth_map.txt lists/filtered_map.txt $(pwd) 0.005
```

`bundle_adjust.sh` rebuilds its image/camera/mapprojected lists from the ids on
each line of the list it is given, so passing `filtered_map.txt` keeps all three
in lockstep.

**Step 5 - matches-only bundle adjust.** `NUM_ITERATIONS=0` harvests the match
files (written during matching, before any solve) without a drift-prone solve of
free cameras. `IMG_DIR` points at the cube/camera dir (default `img`). Forward the
env with `-v` (only set vars, see the gotcha below).

```bash
qsub -m n -r n -N ba -l walltime=23:01:00 -W group_list=e2305 \
  -j oe -S /bin/bash -l select=10:ncpus=20:model=bro_ele \
  -v "IMG_DIR=lronac_all,OVERLAP_LIMIT=75,NUM_ITERATIONS=0,PROCESSES=10,THREADS=8" -- \
  ~/projects/sfs/bundle_adjust.sh lists/filtered_map.txt ref/lola_1mpp_extra_noblur.tif maps ba/run $(pwd)
```

The `.match` files under `ba/` are the deliverable, reusable in a later controlled
BA (USGS-polar cameras held fixed, see coregister-linescan / image-gcp-gen).

## Controlled Refinement Family: bundle_adjust_refine.sh (fixed -> free -> dem)

After the matches-only harvest (Step 5), the controlled solve is a three-stage
chain, all done by ONE script `~/projects/sfs/bundle_adjust_refine.sh`, which
reuses the harvested matches and turns the two optional constraints on via env:
`FIXED_LIST` (subset of images whose cameras are held fixed) and `REF_DEM`
(adds `--heights-from-dem` + `--mapproj-dem`). This mirrors the documented
registration refinement in the ASP manual (:numref:`sfs_ba_refine`), using fixed
registered anchors in place of the manual's stereo-DEM + `pc_align` alignment.

**Each stage feeds on the previous one.** They go from most-constrained to
least-, then re-tighten to the ground:

1. **fixed** (`FIXED_LIST` set, no DEM): the registered anchor (USGS) cameras are
   held FIXED and pull the free cameras into their frame. This establishes the
   coordinate system. Output `ba_fix`.
2. **free** (nothing set): starting from `ba_fix`, ALL cameras are relaxed and
   refined together with no external constraint, letting the network settle to a
   consistent minimum. Output `ba_free`.
3. **dem** (`REF_DEM` set): starting from `ba_free`, the reference terrain is added
   as a constraint for the final vertical/registration tighten. Output `ba_htdem`.
   `DEM_UNCERTAINTY` defaults to 20 m (per the manual); use 10 to trust the DEM
   more, up to 100 if the cameras are believed far from it. Be mindful of this
   value: past runs tried both 20 and 10 with little practical difference, so 20
   is hardcoded, but it is worth reconsidering when moving to a wildly different
   reference DEM, where how much to trust the terrain matters more.

Baked-in behavior (do not override): matches are reused via `--match-files-prefix`
plus `--skip-matching`, so they are never recomputed, and NEVER
`--clean-match-files-prefix` (the clean matches from a `NUM_ITERATIONS=0` harvest
were outlier-filtered against UN-optimized cameras, so they are over-filtered and
would starve the solve). `--camera-weight 0` lets cameras move; intermediate
cameras are saved.

**Assemble the lists offline, 1-to-1.** For stage 1 the image list is the
survivors (cub paths from the full dir), and the camera list is 1-to-1 with it,
but each anchor image points at its REGISTERED (USGS) `.json`, not the vanilla one.
`FIXED_LIST` is that same anchor image subset. Stages 2 and 3 take the previous
stage's `outDir/run-image_list.txt` and `run-camera_list.txt` (they already point
at the adjusted cameras BA wrote).

The reuse stages are serial `bundle_adjust` (matching is skipped), so one node
each. Run each on its own qsub; each waits for the previous:

```bash
# Stage 1: fixed - anchor (USGS) cameras hold the frame
qsub -m n -r n -N ba_fix -l walltime=8:00:00 -W group_list=e2305 \
  -j oe -S /bin/bash -l select=1:ncpus=20:model=bro_ele \
  -v "FIXED_LIST=lists/usgs_fixed_images.txt" -- \
  ~/projects/sfs/bundle_adjust_refine.sh \
  lists/filtered_images.txt lists/filtered_cameras_mixed.txt ba/run ba_fix $(pwd)

# Stage 2: free - relax ALL cameras, no constraint (feeds on ba_fix)
qsub -m n -r n -N ba_free -l walltime=8:00:00 -W group_list=e2305 \
  -j oe -S /bin/bash -l select=1:ncpus=20:model=bro_ele -- \
  ~/projects/sfs/bundle_adjust_refine.sh \
  ba_fix/run-image_list.txt ba_fix/run-camera_list.txt ba/run ba_free $(pwd)

# Stage 3: dem - final tighten to the terrain (feeds on ba_free)
qsub -m n -r n -N ba_htdem -l walltime=8:00:00 -W group_list=e2305 \
  -j oe -S /bin/bash -l select=1:ncpus=20:model=bro_ele \
  -v "REF_DEM=ref/lola_1mpp_extra_noblur.tif" -- \
  ~/projects/sfs/bundle_adjust_refine.sh \
  ba_free/run-image_list.txt ba_free/run-camera_list.txt ba/run ba_htdem $(pwd)
```

Validate each stage's `<outDir>/run-final_residuals_stats.txt`: the median
reprojection error per camera should fall to about 1-2 px (:numref:`sfs_usage`);
if not, the solve did not converge.

This supersedes the old `bundle_adjust_fix.sh` / `bundle_adjust_heights_from_dem.sh`
/ `bundle_adjust_reuse_matches.sh` (removed; the last used a stale isis5.0.1 env and
clean matches). `bundle_adjust_dem_gcp.sh` is a separate GCP-based variant, not part
of this chain.

## Pre-SfS max-lit alignment sanity check (do this BEFORE any SfS)

After the refine chain, prove the newest cameras co-register the whole set before
committing to SfS. Mapproject all survivors with the NEWEST (final-stage) cameras onto
the honest DEM, build per-chunk max-lit mosaics in illumination (azimuth) order, then
max-lit the FIRST-half image group and the SECOND-half group SEPARATELY, and finally
max-lit those two halves into one grand mosaic. The point of the two halves: they are
two DISJOINT illumination groups, so if the cameras are well registered the terrain
features coincide when overlaid; ghosting or doubling between the halves is residual
misregistration. Stopping at two halves (not going down to individual frames) is enough
to localize misalignment while staying cheap. Template: `sfs_m2m_ca`
`sfs_ca_align_notes.sh` Step 6 (which merged all partials directly; the halves split is
the added diagnostic).

```bash
# 1. batch mapproject; each chunk auto-writes map_htdem/max_mosaic_<beg>_<end>.tif.
#    Pass the final-stage adjusted camera list as the 6th arg. Do NOT set NO_MOSAIC.
export CHUNK_SIZE=50          # generous, not excessive (~20 chunks per ~1000 images)
export WALLTIME=6:00:00
~/projects/sfs/batch_mapproject.sh ref/lola_1mpp_extra_noblur.tif \
  lists/filtered_images.txt ba_htdem/run map_htdem $(pwd) ba_htdem/run-camera_list.txt
# 2. split partials into two illumination halves by beg (imagecount/2), max-lit each.
#    batch_max_mosaic.sh runs dem_mosaic --threads 20: qsub it or drop to --threads 2,
#    NEVER > 2 threads on a pfe login node.
cd map_htdem; ls max_mosaic_*.tif | awk -F'[_.]' '$3<500'  > half1_list.txt
              ls max_mosaic_*.tif | awk -F'[_.]' '$3>=500' > half2_list.txt; cd -
~/projects/sfs/batch_max_mosaic.sh map_htdem/half1_list.txt map_htdem/half1_max_mosaic.tif $(pwd)
~/projects/sfs/batch_max_mosaic.sh map_htdem/half2_list.txt map_htdem/half2_max_mosaic.tif $(pwd)
# 3. grand max-lit of the two halves.
printf 'map_htdem/half1_max_mosaic.tif\nmap_htdem/half2_max_mosaic.tif\n' > map_htdem/halves_list.txt
~/projects/sfs/batch_max_mosaic.sh map_htdem/halves_list.txt map_htdem/all_max_mosaic.tif $(pwd)
```

Then inspect ([[visual-inspection]]): warp half1 vs half2 to a common grid and red/green
overlay to catch ghosting between the two illumination groups, and eyeball
`all_max_mosaic.tif` for self-consistency and any global shift vs a LOLA hillshade. This
is the go/no-go gate before SfS.

## Smeared cameras in a max-lit mosaic: detect, inspect, and fix cheaply

A max-lit mosaic can show diagonal **brush-stroke smears** while the terrain underneath
stays put (craters co-located, NOT a shift). That is one or a few badly-posed cameras
whose content drapes stretched in the wrong place; max-lit keeps the bright streak.

DETECT - the smoking gun is the mapproj offset stats, NOT the reprojection stats:
- `<ba>/run-final_residuals_stats.txt` (image, mean, median, count = reprojection px) is
  BLIND to a self-consistent-but-wrong camera: a smear camera can reproject at ~0.15 px
  (it fits its own handful of tie points perfectly) yet be km off in absolute terms.
- `<ba>/run-mapproj_match_offset_stats.txt` (image, 25/50/75/85/95%, count = METERS
  between where this image lands a feature and where the others land it) is the detector:
  a smear lights up with a huge upper-percentile (km), while its median stays small.
- Cross-check `<ba>/run-camera_offsets.txt` (horiz, vert center move, m) and the match
  count: a smear has a large offset AND a substantial in-box footprint (hundreds-thousands
  of matches). A near-zero-match camera (count < ~50) with a giant offset is a DROPOUT,
  not a smear: it drifted off the box and contributes nothing (leaves a black notch, an
  absence). RULE: mapproj-offset flags smears AND dropouts; only offset + big footprint
  smears. Drop BOTH from the camera set before a re-solve/SfS, but only smears need a
  mosaic rebuild.
- Join these per-camera files against the azimuth-ordered image list to see which
  illumination half a culprit falls in (that is which half-mosaic it contaminates).

INSPECT (which chunk):
- The per-chunk partials `map_htdem/max_mosaic_<beg>_<end>.tif` isolate ~50 images each,
  so a suspect chunk shows the smear far more starkly than the blended full mosaic (where
  the good images partly overwrite it). Render the suspect chunk alone to confirm the
  culprit lives there. Each image sits in exactly ONE chunk (by azimuth index), so a
  culprit contaminates exactly one chunk, one half, and the full mosaic.
- Quick 8-bit overview of a reflectance mosaic (values ~0-0.2): `gdal_translate -of PNG
  -ot Byte -outsize 1600 0 -scale 0 0.11 0 255 -a_nodata 0 in.tif out.png`. On pfe use the
  `geo` conda env for a clean gdal (see [[pfe-nas]]); a proj.db warning means the env is
  wrong. `gdalinfo -stats` for the min/max/mean to pick the -scale ceiling.

FIX cheaply (rebuild ONLY what the culprit touched, NO re-mapproject): the per-image
`map_htdem/*.map.tr1.tif` survive, and each chunk kept its input list
`map_htdem/map_list_<beg>_<end>.txt`. So rebuild just the affected chunk, then the one
contaminated half, then the full, all via `dem_mosaic --max` (batch_max_mosaic.sh):
```bash
# 1. cleaned chunk: drop the culprit id(s) from its map list, re-max-lit
grep -v M1139778716 map_htdem/map_list_550_600.txt > map_htdem/map_list_550_600_clean.txt
~/projects/sfs/batch_max_mosaic.sh map_htdem/map_list_550_600_clean.txt                \
  map_htdem/max_mosaic_550_600_clean.tif $(pwd)
# 2. cleaned half: same chunk mosaics, culprit chunk -> its cleaned version
sed 's#max_mosaic_550_600\.tif#max_mosaic_550_600_clean.tif#' map_htdem/half2_list.txt \
  > map_htdem/half2_clean_list.txt
~/projects/sfs/batch_max_mosaic.sh map_htdem/half2_clean_list.txt                      \
  map_htdem/half2_clean_max_mosaic.tif $(pwd)
# 3. cleaned full: unchanged half1 + cleaned half2
printf 'map_htdem/half1_max_mosaic.tif\nmap_htdem/half2_clean_max_mosaic.tif\n'        \
  > map_htdem/all_clean_list.txt
~/projects/sfs/batch_max_mosaic.sh map_htdem/all_clean_list.txt                        \
  map_htdem/all_clean_max_mosaic.tif $(pwd)
```
These are same-grid max-lit (no reprojection), quick enough for one serial `devel` qsub
(dem_mosaic --threads 20 needs a compute node, never > 2 threads on a login node). Wrap
the three calls in a tiny project worker and qsub it (see the BCU2314
`bcu_redo_clean_mosaics.sh` precedent). VERIFY: the streaks are gone AND the craters did
not move; if the terrain shifted you dropped a GOOD camera by mistake.

## Prior SfS matches/refinement projects (context, notes live in each dir)

When you need more context on this pipeline, the prior runs kept full work notes in
their own project dirs (read the `*_notes.sh` there):
`~/projects/sfs_m2m_ca`, `~/projects/sfs_m2m_sp`, `~/projects/sfs_m2m_mp` (the
mare/south-pole/cold-area m2m sites that first ran the harvest -> fixed -> dem chain),
and the current `~/projects/sfs_BCU2314-BDU1224-MM` (matches_pipeline_notes.sh). The
generic scripts in `~/projects/sfs/` are the reusable distillation; the per-site
notes carry the exact invocations, list-assembly, and what went wrong.

## qsub and Autonomous Orchestration (sanity checks, do not blunder)

- **qsub `-v` fails on UNSET variables** (`qsub: cannot send environment with the
  job`, rc=1). A fixed `-v A,B,C,...` list where any name is unset silently breaks
  EVERY submission (the loop prints its "Will do list" banner but no job id, and
  `qstat` shows nothing). Build `-v` from only the set variables. `batch_mapproject.sh`
  now does this; do the same in any hand-written qsub.
- **Gate the next pipeline step on JOB STATE, never on output-file existence.** A
  PBS product file appears the instant writing begins and looks done while still
  half-written (a monitor once fired on a half-blurred DEM). A monitor's exit
  condition must be that the job LEFT THE QUEUE by finishing (`qstat <id>` shows no
  `R`/`Q`; `qstat -x -f <id>` gives `job_state=F`, `Exit_status=0`), then verify the
  product. Detail in the pfe-nas skill.
- **Dry-test one image before the batch.** Run a single `mapproject` on a small
  sub-box (`--threads 1 --processes 1`, head-node-legal) to prove the DEM + cube +
  paired camera + projwin + naming chain works before submitting dozens of jobs.
- **Autonomous stepwise pattern (no cron):** prepare -> a detached, qstat-gated
  background poll waits for that job to finish and re-triggers the agent -> launch
  the next step -> monitor -> inspect -> next. Each poll is one cheap `qstat` over
  ssh; the agent is idle (not billed) between polls and wakes exactly when the jobs
  finish. This is the right mechanism, not a recurring cron.

## Timing and Scale Guidance for Planning (measured, past runs)

Node cpus: `ivy`/`has` = 20, `bro_ele` = 28, Athena `tur_ath` = 256 (submit only
from the Athena front end). bro_ele bills ~1 SBU/node-hour. Requested `walltime=`
values are ceilings; the numbers below are MEASURED durations.

| Case | Site | Images | Mapproject | Bundle adjust (measured) | parallel_sfs (measured) |
| :--- | :--- | :--- | :--- | :--- | :--- |
| m2m ca | 7.5x14 km | 1334 | 1 ivy/chunk 40 | 16 ivy, matches: **3:22** | 4 ivy, 4000x4000 clip: 2.4-5.5 h |
| m2m sp | 10x10 km | 1758 (613 usgs) | 1 ivy/chunk 40 | batched 3-4 ivy | 4 has, hit the 12 h ceiling |
| nobile_2 | 14.3x11 km | 1762 | 1 ivy/chunk 25 (48 jobs) | 1 ivy: **0:51-1:41** | 3 h |
| nobile_7 | ~11x5 km | ~419-1200 | - | 1-node local: 200 matches 57 GB/2.5 h; 600 62 GB/2:17; 1000 (post leak-fix) 52 min/5 GB | 16 ivy: **5.9-7.6 h** |
| ridge | 16x13 km | 1032 (subset 75) | 1 ivy/chunk 105 (10 jobs) | 8-20 ivy | 32 ivy: **6.5 h**; 20000x18000 est 400 core-h -> ~12.5 h on 32 cores |
| 1414A | small tiles | 157 (BA 422) | tur_ath/chunk 16, ~15-30 min/chunk | tur_ath 256 cpu: **~5 min for 422 cubs** | 4 bro_ele/tile: ~4 h |
| mons_mouton | 117 tiles | ~3000 | per-tile bro_ele, 1-3 h | 301 cams, 3-pass: ~50 SBU | 4 bro_ele/tile (~135 img): **5.5-6.6 h, ~22-26 SBU/tile** |

Planning takeaways:
- **Matches-only BA is fast once matching parallelizes.** 1334 images on 16 ivy
  nodes finished in 3.4 h (not the 23 h ceiling); 422 cubs on one 256-cpu tur_ath
  node took ~5 min. So a ~1000-image matches-only BA on 8-10 bro_ele nodes should
  finish in a few hours. Athena `tur_ath` is dramatically faster per node for the
  matching stage (256 cpu) and, being fewer nodes, less exposed to the
  `parallel_bundle_adjust` ssh-spawn failure.
- **Single-node local BA RAM scales hard with `--max-pairwise-matches`** (600
  matches = 62 GB). `bundle_adjust.sh` sets 5000, which is fine when
  `parallel_bundle_adjust` distributes across nodes, but do not run that on one
  node without watching memory.
- **Mapproject** is one node per 25-105 image chunk, chunks concurrent; each chunk
  a few hours at most on the small delivery box.
- **parallel_sfs** is the long pole: ~4 bro_ele nodes per ~2048-4000 px tile,
  4-7 h wall, ~22-26 SBU/tile. Budget the height-uncertainty (`estimError`) pass
  separately, it is single-core and much slower.

---

## Canonical SfS Toolkit Reference (`~/projects/sfs/`)

The `~/projects/sfs/` repository contains the core pipeline scripts developed for photoclinometry on Pleiades and Athena:

* **`make_ref_dem.sh`**: Reproject and regrid a source DEM to a fixed half-integer target grid with ASP 256-block tiling, optional spike blur. Builds the SfS reference DEM.
* **`batch_mapproject.sh`**: Chunked PBS job orchestrator for multi-node parallel mapprojection.
* **`mapproject_chunk.sh`**: Robust per-node worker script with error recovery and tile cleanup.
* **`filter_by_max.sh`**: Order-preserving cull of mapprojected images by their `gdalinfo` maximum (drops shadowed and non-intersecting frames), keeping azimuth order.
* **`bundle_adjust.sh`**: `parallel_bundle_adjust` wrapper. Env tunables `IMG_DIR`, `OVERLAP_LIMIT`, `NUM_ITERATIONS` (0 for matches-only), `PROCESSES`, `THREADS`. Submit via qsub across N nodes.
* **`parallel_sfs.sh`**: Distributed Shape-from-Shading runner across multiple tiles and nodes.
* **`sfs_sim_align.sh`**: Measures pointing errors against simulated illumination and runs single-camera bundle adjustment before SfS.
* **`query_lro.py` / `query_lro.sh`**: Query PDS ODE REST API for LRO NAC images by lat/lon box or DEM extent, emitting product IDs and direct `.IMG` download URLs.
* **`download_all.sh`**: Resumable multi-file URL downloader with retries (feeds directly from `query_lro.py --output-urls`).
* **`query_azimuth.sh`**: Fast extraction of camera solar azimuth and elevation via `sfs --query`.
* **`query_gsd.sh`**: Automatic querying of native ground sampling distance via `mapproject --query-projection`.
* **`blend_img_mosaic.sh` / `avg_mosaic.sh`**: Weighted-mean blending of mapprojected images with shadow suppression.
* **`bundle_adjust_dem_gcp.sh`**: Bundle adjustment constrained by DEM surface and ground control points.

