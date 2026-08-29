---
name: ctx-processing
description: End-to-end MRO CTX linescan processing in ASP - fetch EDRs (ODE REST volume lookup + planetarydata.jpl.nasa.gov), preprocess per ctx.rst (mroctx2isis, spiceinit, ctxcal, ctxevenodd), build CSM linescan cameras with isd_generate, bundle_adjust, parallel_stereo with local_epipolar + num-matches-from-disparity, point2dem, pc_align to a reference DTM/MOLA, and jitter_solve. Load before any CTX work - fetching/ingesting CTX images, making CTX cubes or CSM JSON, running CTX stereo/DEM, or CTX jitter correction. For jitter_solve parameter detail defer to the ASP manual jitter_solve.rst (:numref:`jitter_ctx`).
---

# MRO CTX processing (fetch → cub → CSM → bundle → stereo → jitter)

The full linescan pipeline. Authoritative docs live in the ASP manual:
`docs/examples/ctx.rst` (:numref:`ctx_example`) for fetch + preprocessing, and
`docs/tools/jitter_solve.rst` (:numref:`jitter_ctx`) for the maintained jitter
recipe and the **latest best parameter use** — read jitter_solve.rst before every
jitter run; the knobs below are a starting point, not the last word.

Reference implementation of every stage, on pfe (the recent, working pipeline):
`~/projects/cassis_asp/ctx_jitter_study_scripts/` (mirrored on pfe at
`ctx_jitter_study/scripts/`): `00_fetch_ingest.sh`, `02_bundle.sh`,
`03_stereo.sh`, `04_jitter.sh`, plus `env_isis.sh` / `env_asp.sh`. Copy and adapt
these rather than writing from scratch.

## 0. Fetch the EDRs (the part that keeps breaking)

CTX raw products are `<PRODUCT_ID>.IMG` EDRs on PDS. Two gotchas:

1. **The host moved.** Old `pds-imaging.jpl.nasa.gov/.../mars_reconnaissance_orbiter/ctx/...`
   is dead. Current host: `https://planetarydata.jpl.nasa.gov/img/data/mro/ctx/<VOL>/data/<ID>.IMG`
   (no `mars_reconnaissance_orbiter` path segment). `<VOL>` is a `mrox_NNNN` volume
   that increases with acquisition time — you cannot guess it, look it up.
2. **Look up the volume via the ODE REST API — with `results=f`.** Without
   `results=f` the response has `Count:1` but an EMPTY `Products` block (this wastes
   time). Correct call:
   ```
   curl -s "https://oderest.rsl.wustl.edu/live2/?query=product&results=f&pt=EDR&productid=<ID>&output=JSON"
   ```
   then pull the `.IMG` URL from
   `ODEResults.Products.Product[.Product_files.Product_file[].URL]` (Product may be a
   dict or a list — handle both). That URL is the real download link.

Download with `wget -nv <url> -O <ID>.IMG`. Use `[ -s file ]` not `[ -f file ]` to
decide whether to skip — a failed fetch leaves a 0-byte file that `-f` treats as
present.

## 1. Preprocess to a calibrated cube (ctx.rst)

```
mroctx2isis from=<ID>.IMG      to=<ID>.cub
spiceinit   from=<ID>.cub      web=true          # or use a local $ISISDATA mro area
ctxcal      from=<ID>.cub      to=<ID>.cal.cub
ctxevenodd  from=<ID>.cal.cub  to=<ID>.cal.eo.cub   # even/odd detector destripe (recommended)
```
`spiceinit web=true` fetches kernels on the fly; on pfe the MRO kernels + gridded
MOLA already live in `$ISISDATA` (`~/projects/isis3data`), so `web=true` is optional.
Keep only the final `.cal.eo.cub`; drop `.cub`/`.cal.cub` so a `data/*.cub` glob
resolves to exactly one cube per image.

## 2. CSM linescan camera (strongly recommended over ISIS cameras)

```
isd_generate -k <ID>.cal.eo.cub  <ID>.cal.eo.cub     # -> <ID>.cal.eo.json
```
`-k` reads SPICE straight from the cube (:numref:`create_csm_linescan`). The `.json`
is a CSM model-state linescan camera; ASP tools take `img.cub img.json`. CSM is much
faster and is required for `jitter_solve`. On pfe use the `isis10` conda env for
ingest (NOT `isis10asp` — it lacks libLinearMath and spiceinit dies); set `ISISROOT=$CONDA_PREFIX`,
`ISISDATA=~/projects/isis3data`, `ALESPICEROOT=$ISISDATA`. See `env_isis.sh`.

Fetch + preprocess + isd_generate are light single-thread work — **fine on the pfe
head node**. Everything below (bundle/stereo/jitter) is heavy — **qsub a compute node**.

## 3. Bundle adjust (before stereo)

```
bundle_adjust --ip-per-image 20000 --max-pairwise-matches 100000 \
  --tri-weight 0.1 --tri-robust-threshold 0.1 --camera-weight 0 \
  --remove-outliers-params '75.0 3.0 10 10' \
  <cubs...> <jsons...> -o ba/run
```

## 4. Stereo + DEM + align (local_epipolar)

```
parallel_stereo --processes <P> --threads-multiprocess 4 \
  --bundle-adjust-prefix ba/run \
  --stereo-algorithm asp_mgm \
  --num-matches-from-disparity 40000 \
  --alignment-method local_epipolar \
  img1.cub img2.cub img1.json img2.json stereo/run
point2dem --stereographic --auto-proj-center --errorimage stereo/run-PC.tif
```
- **`--num-matches-from-disparity 40000`** writes dense, well-distributed matches
  from the disparity — these are the matches `jitter_solve` consumes
  (`--match-files-prefix stereo/run-disp`). Do this in the stereo step so jitter can reuse them.
- **NAS/PBS nproc trap:** inside a PBS job `nproc` and `PBS_NODEFILE` both report 1
  (the node actually has 128–256 cores), which silently forces `--processes 1` =
  fully serial. **Pass `--processes` explicitly** (derive from the qsub `select`, or
  `/proc/cpuinfo`), never auto-detect. Single node → no `--nodes-list`.
- Alternative alignment: mapprojected images (ctx.rst default; most reliable for
  correlation). For jitter work local_epipolar keeps the geometry native.
- Re-triangulate cheaply after new cameras with `--prev-run-prefix stereo/run`
  (reuses the correlation; only re-triangulates).

**Align to a reference** (dense cloud first so cameras get the INVERSE transform):
```
pc_align --max-displacement <D> stereo/run-DEM.tif <ref> \
  --save-inv-transformed-reference-points -o stereo/run-align
point2dem --stereographic --auto-proj-center stereo/run-align-trans_reference.tif -o stereo/run-align
bundle_adjust --input-adjustments-prefix ba/run \
  --initial-transform stereo/run-align-inverse-transform.txt \
  --apply-initial-transform-only <cubs> <jsons> -o ba_align/run
```
`<ref>` = MOLA (463 m, always available) or a USGS controlled CTX DTM (20 m). Pick
`--max-displacement` from the actual DEM-to-ref offset.

**pc_align argument order + datum — two traps that silently corrupt the result:**
- **Order is `pc_align <REFERENCE> <SOURCE>`** (pc_align.rst): the FIRST arg is the
  reference, the SECOND (source) is what MOVES. `--save-transformed-source-points`
  saves the SECOND cloud regridded into the reference frame; `run-transform.txt` maps
  source(2nd)→ref(1st) and is the one to feed `bundle_adjust --initial-transform
  --apply-initial-transform-only`. So to align your DEM to a reference DTM:
  `pc_align ref.tif your-DEM.tif --save-transformed-source-points`, then point2dem
  `run-trans_source.tif`, and apply `run-transform.txt` to the cameras. If you put
  your DEM FIRST by mistake, pc_align silently moves the REFERENCE into your frame
  (the "aligned DEM" stays at your original elevation) and the camera transform points
  the wrong way. (The reverse order is also valid if you instead use
  `--save-inv-transformed-reference-points` + `run-inverse-transform.txt` — but never
  MIX the two conventions.)
- **Datum:** ASP `point2dem` writes ELLIPSOID (sphere R=3396190) heights; the USGS
  controlled CTX DTMs, HRSC blend, and vendor products are AREOID (orthometric),
  offset by the geoid separation (~1500–1700 m at Jezero, and it VARIES spatially).
  Before pc_align / geodiff / jitter `--heights-from-dem`, put everything in ONE
  datum: convert the areoid references to ellipsoid with
  `dem_geoid --reverse-adjustment --geoid MOLA ref.tif -o ref_g` (→ `ref_g-adj.tif`;
  needs `ISISROOT` set and the `mola_areoid.tif` in the ASP `share/geoids`, present in
  the pfe package). A leftover ~1500 m areoid/ellipsoid mismatch will make pc_align
  invent a bogus vertical shift and corrupt jitter's `--heights-from-dem`.

## 5. Existing CTX DTM products (references, no stereo needed)

USGS-generated, ASP-made, MOLA-controlled CTX DTMs + orthos at 20 m/px, one per
stereo pair, on the Astrogeology STAC:
`https://stac.astrogeology.usgs.gov` collection `mro_ctx_controlled_usgs_dtms`
(assets on the `astrogeo-ard` S3, public HTTPS). Good alignment references and
mapproject helper DEMs. The MGS MOLA global DEM (~463 m) is the standard coarse baseline.

## 6. Jitter correction — see jitter_solve.rst (:numref:`jitter_ctx`) for the latest

CTX linescan cameras exhibit jitter. `jitter_solve` refines per-line poses.
**Always read `docs/tools/jitter_solve.rst` for current best use** — this is a
starting template only (from `04_jitter.sh`):
```
jitter_solve img1.cub img2.cub img1.json img2.json \
  --input-adjustments-prefix ba_align/run \
  --match-files-prefix stereo/run-disp \
  --max-pairwise-matches 100000 \
  --max-initial-reprojection-error 20 \
  --num-iterations <N> \
  [--num-lines-per-position <nlpos>] [--num-lines-per-orientation <nlorient>] \
  --heights-from-dem <ref> --heights-from-dem-uncertainty <hUnc> \
  --heights-from-dem-robust-threshold 0.05 \
  [--camera-position-uncertainty <h>,<v>] \
  [--num-anchor-points <A> --num-anchor-points-extra-lines <E> \
   --anchor-dem <ref> --anchor-dem-uncertainty <aUnc>] \
  --tri-weight <tw> \
  -o jitter_<tag>/run
```
Knob intuition (smaller uncertainty = stronger pull):
- `--heights-from-dem` + `--anchor-dem` should be the SAME reference DEM (a CTX
  mosaic or MOLA). Anchor points hold the strip ends; `--num-anchor-points-extra-lines`
  places anchors BEYOND the image ends to kill the strip-edge instability.
- **Ordering of trust** (typical): GCP (via `dem2gcp`, small `--gcp-sigma`, most
  trusted) pulls harder than `--heights-from-dem-uncertainty`, which pulls harder
  than `--anchor-dem-uncertainty`. Let cameras move (`--camera-position-uncertainty`
  generous) to conform to the trusted ground.
- **Too-fine pose sampling overfits.** Native (no resampling) gives thousands of
  pose params (~18 min/Ceres-iter for a full swath) and "scary" DEMs; the honest
  cure for residual jitter is MULTIPLE crossing-track images, not more DOF
  (Oleg on ASP issue #396). Resample with `--num-lines-per-position/orientation`.
- After solving, the cameras are `jitter_<tag>/run-<img>.adjusted_state.json`.
  Re-triangulate with `--prev-run-prefix stereo/run` (cheap), then point2dem.
- **point2dem `--max-valid-triangulation-error`** (NOT `--max-triangulation-error`,
  which does not exist and makes point2dem dump help and exit non-zero, killing a
  `set -e` script): set it GENEROUS (e.g. 100–200 m) when jitter is large, else
  high-error but real lines get cut → missing lines in the DEM.

## Validation (judge by the reference, not by intersection error alone)

Absolute-vs-relative trap: a jitter run can shrink intersection error while the DEM
drifts from truth. Always geodiff the before/after DEM against the reference
(`geodiff --absolute <ref> run-DEM.tif`, robust median/NMAD), and do the
signed before-minus-after DEM diff (discontinuity test). See the `dem-comparison`
skill for dh/dv (horizontal, from hillshade correlation) vs dz (vertical geodiff),
and `local-epi-debug` for missing-tile / edge diagnostics. Complements
`asp-photogrammetry`, `pc-align`, `pfe-nas` (qsub), `project-workflow`.
