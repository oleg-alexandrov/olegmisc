---
name: cassis-pipeline
description: The TGO CaSSIS pushframe-as-framing stereo pipeline in ASP - the CassisPipeline repo and its bin/ stage scripts, the ms_cassis.py per-pair stereo engine (and its swap to the shipped multi_stereo tool), the pfe run layout and replicas, the build-floor date check, and where the CaSSIS notes live. Load whenever working on CaSSIS - CassisPipeline, ms_cassis, cassis_stereo/cassis_process, framelets, pushframe, or a CaSSIS DEM replica on pfe. Complements multi-stereo (the engine), ctx-processing (the CTX reference), pc-align, dem-comparison.
metadata:
  type: reference
---

CaSSIS (TGO) acquires PAN pushframe framelets that ASP treats as many small FRAMING
cameras. The pipeline builds a frame DEM per observation, tied to a CTX reference DEM.

**Repo: `~/projects/CassisPipeline`** (`NeoGeographyToolkit/CassisPipeline`, pushable;
also cloned on pfe at `~/projects/CassisPipeline` and on l1). All logic is shell +
Python in `bin/`. This is the real pipeline - use it from there, do not hand-roll.

Stage scripts in `bin/` (driven by `cassis_process.sh`, the top-level driver):
- `cassis_process.sh` - orchestrates the stages. Enforces a BUILD FLOOR: reads
  `parallel_stereo --version` "Build date:" (dashes stripped to YYYYMMDD) and refuses
  an ASP older than a required date. Bump this when a needed ASP feature lands.
- `cassis_stereo.sh` - the frame-DEM step: mapproject the framelets onto the blurred
  CTX at native GSD (~4.59 m), build a cross-look overlap list, run per-pair stereo +
  point2dem + DEM mosaic, then put the frame DEM on the CTX grid (+ hillshade, dz
  geodiff). Historically calls `ms_cassis.py` (see below) at the stereo+dem+mosaic step.
- `cassis_corr.sh` - correlation-only pass (`parallel_stereo --correlator-mode`).
- `cassis_ctx_stereo.sh`, `cassis_ctx_align.sh` - build/align the CTX reference DEM.
- `cassis_linescan_dem.sh` - linescan CTX DEM helper.
- `gen_gcp.sh`, `refit_transverse.sh`, `cassis_metrics_run.sh`, `cassis_pass.sh`.

**`ms_cassis.py`** (`CassisPipeline/bin/ms_cassis.py`, ~220 lines) is the CaSSIS
per-pair stereo ENGINE: overlap-list -> per-pair `parallel_stereo` (mapprojected) ->
`point2dem` on a fixed grid -> `dem_mosaic`, with a CTX-relative blunder filter that
drops a per-pair DEM whose mean departs from the reference. It was the prototype for
the shipped ASP `multi_stereo` dem_mosaic mode. Its options: `--overlap-list --dem
--ref-dem --out-dir --mode dem_mosaic --num-parallel --blunder-tol --stereo-options
--point2dem-options`. SWAP to the shipped tool: replace the `ms_cassis.py` call in
`cassis_stereo.sh` with `multi_stereo` (see [[multi-stereo]]), mapping `--out-dir` ->
`--out-prefix` (dir->prefix, so also fix the downstream mosaic paths in that script),
`--num-parallel` -> `--processes`, `--stereo-options` -> `--stereo_options`.

**pfe**: replicas live under `~/projects/cassis_dist_no_tilt` and `/nobackupp19/oalexan1`;
production is a qsub job (heavy 143-pair runs belong on pfe/l1, never the Mac). To
validate a new stereo engine, reuse an existing replica, move its old mosaicked DEM and
max-tri-err aside, inject only the new stereo step, and expect the same DEM.

**Distortion**: ALE emits the CaSSIS distortion (USGSCSM DistortionType CASSIS=9). A
residual cross-track BOWL is a two-look BA near-degeneracy, not a distortion bug; real
fix needs external low-freq control (LOLA/CTX). Alloc e2305.

**Notes** (our working memory, not shipped): `~/projects/cassis_asp/cassis_notes.sh`
(hub) -> `cassis_reprocess_relax_htdem_notes.sh` (reprocess plan, ms_cassis status),
`cassis_cleanup_plan.sh` (tape archive inventory + per-dir wipe log).
