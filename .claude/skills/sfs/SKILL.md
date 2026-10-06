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

# Or run automated head-node friendly batch ingest (fetch + lronac2isis +
# spiceinit + lronaccal + lronacecho + isd_generate linear reduction + cam_test):
~/projects/sfs/batch_prepare_lro.sh lists/products.txt lronac_all --no-usgs-polar

# For USGS South Pole controlled anchor images (using custom polar SPICE kernels):
~/projects/sfs/batch_prepare_lro.sh lists/usgs_products.txt usgs_south --usgs-polar

# Single product manual ingest:
~/projects/sfs/prepare_lro_nac.py --outdir lronac_all M109041171LE
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
the sfs-azimuth skill and `sfs_query.sh`), then sort by the 0-360 azimuth
column and derive matching image and camera lists:

```bash
sort -k3,3 -n azimuth_tables.txt > lists/azimuth.txt
awk '{print $1}' lists/azimuth.txt > lists/azimuth_images.txt
sed 's/\.cal\.echo\.cub$/.cal.echo.json/' lists/azimuth_images.txt > lists/azimuth_cameras.txt
```

Azimuth sorting is what makes `--overlap-limit` in the next BA match images of
similar illumination (matched shadows), which is what co-registration needs.

**List Management & Git Tracking Policy.**
All lists (`lists/*.txt`) and metadata produced across the pipeline (azimuth tables,
sorted image/camera lists, cull/filter lists, outlier/bad camera lists, primary/secondary
SfS selections) MUST be kept in a dedicated `lists/` directory and committed to git
in the project repository. These text lists are tiny (< 100 KB), define the exact
lineage and reproducible inputs for every stage, and must always be version-controlled
so any step can be audited or reproduced. Never scatter lists in the root directory.
Only text lists (`*.txt`, `*.lis`, `*.csv`) belong in git. NEVER add generated binary
images, plots, or figures (such as `.png` rose plots or `.tif` rasters) to git; leave
them untracked on disk.

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

# On Pleiades/HPC: NEVER run multi-process filter_by_max on the head node.
# Submit as a quick 1-node PBS job with PROCS=28 (runs in ~4-5 minutes):
# qsub -q normal -m n -r n -N filter_max -l walltime=00:30:00 -W group_list=e2305 \
#   -j oe -S /bin/bash -l select=1:ncpus=28:model=bro_ele -v "PROCS=28" -- \
#   ~/projects/sfs/filter_by_max.sh lists/azimuth_map.txt lists/filtered_map.txt $(pwd) 0.005
~/projects/sfs/filter_by_max.sh lists/azimuth_map.txt lists/filtered_map.txt $(pwd) 0.005
```

`bundle_adjust.sh` rebuilds its image/camera/mapprojected lists from the ids on
each line of the list it is given, so passing `filtered_map.txt` keeps all three
in lockstep.

**Edge-case guardrail (empty sliver images):**
Images with only a few isolated lit pixels (e.g. 1 to 100 pixels) at the bounding
box edge can pass the `0.005` max threshold, but will fail in `bundle_adjust`'s
downsampled image statistics step (`ImageUtils.cc`, targeting 1M pixels with step
size ~14) with `ERROR: No valid pixels to compute statistics for`. Ensure that
surviving images have sufficient non-nodata pixels (e.g. > 1,000 pixels) before
launching parallel bundle adjustment.

**Step 5 - matches-only bundle adjust.** `NUM_ITERATIONS=0` harvests the match
files (written during matching, before any solve) without a drift-prone solve of
free cameras. `IMG_DIR` points at the cube/camera dir (default `img`). Forward the
env with `-v` (only set vars). Always pass explicit `-q normal` (Pleiades requires
queue specification) and `walltime=8:00:00` (`bro_ele` rejects >8h walltimes).

```bash
qsub -q normal -m n -r n -N ba -l walltime=8:00:00 -W group_list=e2305 \
  -j oe -S /bin/bash -l select=10:ncpus=28:model=bro_ele \
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
   held FIXED and pull the free cameras into their frame. Uses raw harvested matches
   (`--match-files-prefix ba/run`). This establishes the coordinate system. Output `ba_fix`.
2. **free** (`USE_CLEAN=1`, matchPrefix `ba_fix/run`): starting from `ba_fix`, ALL cameras are relaxed and
   refined together with no external constraint, letting the network settle to a
   consistent minimum. Output `ba_free`.
3. **dem** (`USE_CLEAN=1`, `REF_DEM` set, matchPrefix `ba_fix/run`): starting from `ba_free`, the reference terrain is added
   as a constraint for the final vertical/registration tighten. Output `ba_htdem`.
   `DEM_UNCERTAINTY` defaults to 20 m (per the manual); use 10 to trust the DEM
   more, up to 100 if the cameras are believed far from it.

**Clean-match reuse discipline**:
- Stage 1 uses the RAW harvested matches (`ba/run`), because the clean matches from a
  `NUM_ITERATIONS=0` harvest were outlier-filtered against un-optimized cameras and are over-culled.
- Stages 2 and 3 pass `USE_CLEAN=1` pointing `matchPrefix` to `ba_fix/run` (`--clean-match-files-prefix ba_fix/run`).
  These clean matches were filtered against a real registered solve, removing bad matches while preserving network connectivity.

**Assemble the lists offline, 1-to-1.** For stage 1 the image list is the
survivors (cub paths from the full dir), and the camera list is 1-to-1 with it,
but each anchor image points at its REGISTERED (USGS) `.json`, not the vanilla one.
`FIXED_LIST` is that same anchor image subset. Stages 2 and 3 take the previous
stage's `outDir/run-image_list.txt` and `run-camera_list.txt` (they already point
at the adjusted cameras BA wrote).

The reuse stages are serial `bundle_adjust` (matching is skipped), so one node
each. Run each on its own qsub; each waits for the previous:

```bash
# Stage 1: fixed - anchor (USGS) cameras hold the frame (uses raw ba/run matches)
qsub -q normal -m n -r n -N ba_fix -l walltime=8:00:00 -W group_list=e2305 \
  -j oe -S /bin/bash -l select=1:ncpus=20:model=bro_ele \
  -v "FIXED_LIST=lists/usgs_fixed_images.txt" -- \
  ~/projects/sfs/bundle_adjust_refine.sh \
  lists/filtered_images.txt lists/filtered_cameras_mixed.txt ba/run ba_fix $(pwd)

# Stage 2: free - relax ALL cameras (reusing ba_fix clean matches)
qsub -q normal -m n -r n -N ba_free -l walltime=8:00:00 -W group_list=e2305 \
  -j oe -S /bin/bash -l select=1:ncpus=20:model=bro_ele \
  -v "USE_CLEAN=1" -- \
  ~/projects/sfs/bundle_adjust_refine.sh \
  ba_fix/run-image_list.txt ba_fix/run-camera_list.txt ba_fix/run ba_free $(pwd)

# Stage 3: dem - final tighten to the terrain (reusing ba_fix clean matches)
qsub -q normal -m n -r n -N ba_htdem -l walltime=8:00:00 -W group_list=e2305 \
  -j oe -S /bin/bash -l select=1:ncpus=20:model=bro_ele \
  -v "USE_CLEAN=1,REF_DEM=ref/lola_1mpp_extra_noblur.tif" -- \
  ~/projects/sfs/bundle_adjust_refine.sh \
  ba_free/run-image_list.txt ba_free/run-camera_list.txt ba_fix/run ba_htdem $(pwd)
```

Validate each stage's `<outDir>/run-final_residuals_stats.txt`: the median
reprojection error per camera should fall to about 1-2 px (:numref:`sfs_usage`);
if not, the solve did not converge.

This supersedes the old `bundle_adjust_fix.sh` / `bundle_adjust_heights_from_dem.sh`
/ `bundle_adjust_reuse_matches.sh` (removed; the last used a stale isis5.0.1 env and
clean matches). `bundle_adjust_dem_gcp.sh` is a separate GCP-based variant, not part
of this chain.

## Post-bundle, pre-SfS evaluation (SUMMARY -> [[sfs-post-bundle-eval]])

Between the refine chain and actual SfS, run the go/no-go gate and clean the camera set.
Full detail is in the [[sfs-post-bundle-eval]] skill; the essentials:
- **Alignment gate:** mapproject all survivors with the final-stage cameras (per-chunk
  max-lit partials `map_htdem/max_mosaic_<beg>_<end>.tif`), then max-lit the two
  illumination HALVES (split by azimuth index) and a grand mosaic. Two disjoint halves that
  overlay without ghosting => cameras co-register. Red/green only on opposite crater walls
  is an illumination difference, not misregistration.
- **Whacky cameras:** a max-lit mosaic can show diagonal brush-stroke smears (or a blurry
  wash) with the terrain unmoved - a badly-posed camera draping stretched content. The
  bundle per-camera stats predict these WELL: `run-mapproj_match_offset_stats.txt`
  (meters-off-consensus) is the best smear detector; `run-camera_offsets.txt` + low match
  count catch drifted DROPOUTS; `run-final_residuals_stats.txt` (reproj px) is blind to a
  self-consistent-but-wrong pose. Build a suspect list, verify a couple visually, prune.
- **Rebuild cheaply:** the per-image `*.map.tr1.tif` survive, so drop the suspects from the
  chunk `map_list_*.txt` and re-run `dem_mosaic --max` per chunk -> halves -> total, NO
  re-mapproject (one devel qsub). Name by state (`_clean`/`_pruned`). VERIFY the streaks are
  gone AND craters did not move (else a good camera was dropped). Log removed ids in notes.

## SfS image selection (SUMMARY -> [[sfs-image-selection]])

Once the cameras are pruned, pick a minimal-but-covering SUBSET for the SfS solve (the full
set is too expensive). Recipe (ASP `image_subset`): break the box into overlapping
quadrants, group by Sun azimuth (50-150 imgs), feed LOW-RES sub images (sub8>sub4>sub2), run
`image_subset` per group (with `--t_projwin` = the quadrant), and a 2nd pass on the remainder
for 2x coverage. Tools in `~/projects/sfs`: `prepare_lowres.sh`, `split_quadrants.py`,
`image_subset_2x.sh`. Outputs are mapproj image lists (latest bundle cameras), converted to
cub+cam for SfS later. Full detail: [[sfs-image-selection]].

## Run SfS over a site + co-register images to it (SUMMARY -> [[sfs-run-align]])

With the subset chosen, run SfS and register every image to the result. Two phases: (1) tile
the reference DEM, run `parallel_sfs` per tile, `dem_mosaic`-assemble one site SfS DEM, and
inspect it (geodiff vs the reference + hillshade - no tile seams); (2) per image render an
SfS-simulated view, `image_align` it to measure the pixel shift (shift_report), and optionally
`gcp_gen` a GCP (use `ALIGN_THRESH=0` to force a GCP for EVERY image, not just >2 px ones).
Then hillshade-correlate the SfS DEM vs the reference (LOLA) for the GLOBAL horizontal shift
gate, and if re-registering, dem2gcp-from-matches -> a GCP -> a final bundle_adjust (or
jitter_solve) to pull the SfS result into the LOLA frame. Full detail + the proven params,
the metric expertise (GSD/mapproj-offset/sim-shift), and the queue/build gotchas:
[[sfs-run-align]], which in turn leans on [[sfs-post-bundle-eval]], [[jitter-solve]],
[[bundle-adjust]], [[dem-comparison]], [[pc-align]].

## Package the delivery (SUMMARY -> [[sfs-delivery]])

The final stage, after SfS + re-registration + blend. Gather the blended SfS DEM,
the max-lit AND the shadow-masked average ortho mosaics, LOLA, weight,
height-uncertainty, the image-id lists, the two ortho directories (1 m/pixel and
native-GSD), and the final jitter/bundle-adjust camera JSON into a results dir with
a filled inventory.yaml manifest and a readme. Codified in the SfsPipeline repo
(WORKFLOW.md steps 10 and 12, inventory.yaml). Ship the in-production camera version,
not an experimental peer run. Full detail: [[sfs-delivery]].

## Prior SfS matches/refinement projects (context, notes live in each dir)

When you need more context on this pipeline, the prior runs kept full work notes in
their own project dirs (read the `*_notes.sh` there):
`~/projects/sfs_m2m_ca`, `~/projects/sfs_m2m_sp`, `~/projects/sfs_m2m_mp` (the
mare/south-pole/cold-area m2m sites that first ran the harvest -> fixed -> dem chain),
and the current `~/projects/sfs_BCU2314-BDU1224-MM` (matches_pipeline_notes.sh). The
generic scripts in `~/projects/sfs/` are the reusable distillation; the per-site
notes carry the exact invocations, list-assembly, and what went wrong.

## pfe env: the hardcoded `asp_deps` ISISROOT in these scripts is DEAD but INERT (TODO: clean up)

The generic scripts (`sfs_exposures.sh`, `parallel_sfs.sh`, `dem_mosaic_list.sh`,
`bundle_adjust.sh`, `bundle_adjust_refine.sh`, `mapproject_chunk.sh`) all hardcode
`export ISISROOT=$HOME/miniconda3/envs/asp_deps`. That env NO LONGER EXISTS on pfe (only
`asp_deps_stale_nfs`; the live ISIS env is `isis10asp`). It does not matter in practice:
every ASP tool is a `bin/` WRAPPER that UNSETS inherited `GDAL_DATA`/`PROJ_DATA`, re-points
them at the bundle's `share/` (via `libexec/libexec-funcs.sh`), OVERRIDES
`ISISROOT="$TOPLEVEL"` (the bundle), and sets `LD_LIBRARY_PATH` + `CSM_PLUGIN_PATH`. So for
any wrapped tool the scripts' dead `asp_deps` ISISROOT/`$ISISROOT/bin`/`ALESPICEROOT` are
discarded before the real binary runs (`ISISDATA` happens to still resolve, 179 GB kernels,
unused by CSM). Verified live 2026-09-30: `bin/gdalinfo`/`bin/gdal_translate` on a polar
DEM run clean, no proj.db warning. TWO caveats worth remembering: (1) this only holds for
tools called THROUGH `$SP/bin/` wrappers, NOT bare `libexec/` ELF binaries nor non-ASP
python `osgeo` calls, so `tile_dem.py` (bare `gdal_translate` + `from osgeo import gdal`)
must be run under a real gdal env (`geo`) or with `$SP/bin` first on PATH; (2) the dead
strings are a future landmine if anyone adds a non-wrapped call. TODO (dunno, low priority,
cosmetic): make the env overridable in the shared scripts, e.g.
`export ISISROOT=${ISISROOT:-$HOME/miniconda3/envs/asp_deps}`, so each machine/job sets the
right one, rather than a hard flip to `isis10asp` (the Mac still has `asp_deps`).

## sfs_sim_align.sh: get a GCP for EVERY image, not just misaligned ones (ALIGN_THRESH)

`sfs_sim_align.sh` defaults `ALIGN_THRESH=2.0` px: after image_align measures the per-image
shift, if shift < 2 px it prints "image already aligned, stopping" and exits BEFORE gcp_gen,
so a default `batch_sfs_sim.sh` run emits a GCP only for the few images that exceed 2 px.
That is a "correct-only-if-needed" optimization, good for a "which images are misaligned?"
verify pass but WRONG when the goal is a GCP per image to feed a joint re-solve / trans_gcp.
Oleg's rule (2026-10-01): PREFER a GCP for EVERY image regardless of shift - a "stay put"
GCP (shift ~0.2 px) is still a valuable constraint and the joint solver benefits from the
full set. Two ways to force it (prior art: PNCB/pncb_registration.sh, 2026-04-20, same goal):
  - `ALIGN_THRESH=0` (env, pass-through via batch_sfs_sim `-v`): forces the FULL pipeline
    (sim + align + gcp_gen + per-image BA + re-mapproject) on every image. Works today, no
    code edit; extra cost is the per-image BA (~11-15 min/img). This is what PNCB used.
  - `--gcp-only` (sfs_sim_align.sh arg): bypasses the threshold AND stops right after gcp_gen
    (no BA/remap) - cheaper, cleaner when you only need the GCP set. BUT batch_sfs_sim.sh does
    NOT forward it yet; add a `GCP_ONLY=1` env pass-through to expose it.
Re-runs are cheap: sfs_sim_align.sh REUSES an existing per-image mapproject (.meas.map.tif)
and sim-intensity.tif if present, so a second pass only redoes align + gcp. DECISION
(Oleg, 2026-10-01): for any registration / joint-solve / trans_gcp run, SET `ALIGN_THRESH=0`
so a GCP is produced for EVERY image - make that the norm, do NOT rely on the default 2.0
(which drops the sub-2px majority). `--gcp-only` is the cheaper variant (no per-image BA) but
needs a `GCP_ONLY=1` pass-through added to batch_sfs_sim first; ALIGN_THRESH=0 works today.
TODO (dunno): consider flipping sfs_sim_align.sh's default to 0 (or wiring batch_sfs_sim to
require the knob explicitly) so the cutoff can't silently drop GCPs again.

## SBU accounting - compute it when a job finishes (especially SfS)

When any pfe job completes, especially an SfS / parallel_sfs run, COMPUTE its SBU cost and
log it in the project notes: SBU = nodes x walltime_hours x model_rate. Rates (from
`/u/scicon/tools/bin/node_stats.sh`, "SBU rate per node type"): bro_ele 1.0, sky_ele 1.59,
cas_ait 1.64, rom_ait 4.06, mil_ait 4.38, mil_a100 37.86. Pull per-job nodes + walltime
from `/PBS/bin/qstat -x -f <jobid>` (Resource_List.nodect, resources_used.walltime) and sum
across the stage's jobs. After a BIG job (e.g. a 20-tile parallel_sfs run) ALSO run
`acct_ytd | grep <gid>` and note what it reports (Used / Allocation / Remain) even though it
normally lags ~24h - record BOTH the computed figure and the acct_ytd figure (they should
match once accounted; a mismatch means jobs are still unaccounted). Watch the fiscal-year
rollover (Oct 1): the allocation can change sharply (e2305 was 25000 SBU in FY2026 but
1250 in FY2027), so "% of budget" must use the CURRENT-FY allocation from acct_ytd, not a
remembered number. Rule of thumb: a 20-tile lunar SfS run is ~350 node-hours (~350 SBU on
bro_ele) - budget the next run against the live acct_ytd remaining.

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
* **`sfs_blend.sh`**: Blends an SfS DEM back toward the reference (LOLA) DEM in permanent shadow with `sfs_blend`, using smooth transition weights and crater size filtering (:numref:`sfs_blend`).
* **`hillshade_corr.sh`**: Standalone DEM-to-DEM hillshade correlator using `parallel_stereo --correlator-mode` with `asp_mgm` (dh/dv shift measurement without modifying DEMs).
* **`bundle_adjust_dem_gcp.sh`**: Bundle adjustment constrained by DEM surface and ground control points.

Scripts are maintained in `~/projects/sfs/` and unified into the public repository `~/projects/SfsPipeline` (`bin/`).
