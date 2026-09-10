---
name: multi-stereo
description: The ASP multi_stereo tool - distributed multi-view stereo that generalizes parallel_stereo by pooling the per-tile work of ALL image pairs into one load-balanced job. Carries the parallel_stereo --emit-tile-commands/--skip-spawn hooks it drives, the --out-prefix output layout, --conv-angle pair selection, the dem_mosaic vs mesh modes (shared stereo, different fusion), and how it relates to stereo_dist and parallel_stereo. Load whenever working on multi_stereo, multiview/distributed stereo over many pairs, tile pooling, --conv-angle pair selection, or harmonizing it with stereo_dist/parallel_stereo. Complements cassis-pipeline (a user), asp-photogrammetry, asp-regressions.
metadata:
  type: reference
---

`multi_stereo` (`src/asp/Tools/multi_stereo`, Python) runs stereo on many image pairs
and fuses the results. It is a GENERALIZATION of `parallel_stereo`: each tile of each
pair is one job, and all tiles of all pairs are pooled into ONE job so the load is
balanced across all pairs at once (not one pair drained at a time).

**Two modes, ONE shared stereo engine.** `run_pooled_stereo` does the pooled stereo for
both modes; they differ only in fusion:
- `dem_mosaic`: per-pair `point2dem` -> DEM mosaic. Optional `--errorimage` -> max
  triangulation-error mosaic (`dem_mosaic --max`); `--orthoimage` -> DRG mosaic
  (`dem_mosaic --first`, per-pair `L.tif` auto-supplied). Works for raw OR mapprojected
  images (the latter with a seed `--dem`); `--dem` is optional.
- `mesh`: per-pair `pc_filter` -> fused `voxblox_mesh`. Rig cameras (`--rig_config`,
  `--camera_poses`), no datum.

**Mechanism (how it drives parallel_stereo).** Two hooks were added to
`parallel_stereo` for this: `--emit-tile-commands <file>` (in spawn_to_nodes: write the
per-tile child commands to the file and exit, instead of spawning) and `--skip-spawn`
(run a stage's serial setup + finalize/VRT build without re-spawning tiles). Per tiled
stage (corr, blend, rfne, tri): emit each pair's tile commands -> concat into one pool
-> run the pool with GNU parallel (width `--processes`, spread over `--nodes-list` or
`$PBS_NODEFILE`) -> finalize each pair with `--skip-spawn`. Serial stages (pprc, fltr)
run per pair. Which tiled stages actually spawn depends on algorithm/subpixel (asp_mgm
pools BLEND, asp_bm pools RFNE); a stage that spawns none emits an empty pool and the
finalize just builds the VRT.

**GNU parallel over ssh nodes: use `libexec/parallel`, and blank `LD_LIBRARY_PATH`
(the ss_multi_stereo_mapproj fix, 2026-09-10).** When the pool spans nodes, the
real GNU `parallel` must be invoked, not ASP's `bin/parallel` wrapper: prepend
`libexec` to `PATH` so the pool call resolves to the genuine GNU tool. AND blank
`LD_LIBRARY_PATH` around the parallel call: ssh onto a node inherits the launcher's
`LD_LIBRARY_PATH`, which points at ASP's bundled libs and shadows the node's system
`ssh`/OpenSSL, so `ssh` fails to hand off tile commands (the failure that reddened
the nightly). Blanking it lets the remote `ssh` use its own system libs; the child
tile command re-establishes the ASP environment itself. Both hunks live in
`src/asp/Tools/multi_stereo`.

**Output layout: `--out-prefix`** (like parallel_stereo and stereo_dist; the old
`--out_dir` was removed). Outputs `<prefix>-DEM.tif`, `<prefix>-IntersectionErr.tif`,
`<prefix>-DRG.tif`; per-pair stereo under `<prefix>-pairs/<L>__<R>/run`.

**Pair selection.** Either `--overlap-list` (4 cols for dem_mosaic: left right
left_cam right_cam), OR, in dem_mosaic mode, `--conv-angle-prefix <ba_prefix>` +
`--conv-angle-range MIN,MAX` (comma, no quotes): read `<prefix>-convergence_angles.txt`,
keep pairs whose MEDIAN (50th pct) convergence angle is in range, cameras
`<prefix>-<image_stem>.tsai` or `.json` (bundle_adjust standalone cameras only).

**Relationship to siblings.** `stereo_dist` = distribute ONE pair's tiles (incl. the
serial steps) across nodes. `multi_stereo` = pool MANY pairs' tiles. Harmonize option
conventions with them (hyphens: `--point2dem-options`, `--nodes-list`; still-underscore
`--stereo_options`/`--first_step` are candidates to hyphenate).

**Tests + validation pattern.** `ss_multi_stereo_mapproj` (asp_mgm, overlap-list),
`ss_multi_stereo_mapproj_asp_bm` (asp_bm, conv-angle), `ss_multi_stereo` (mesh, rig data
on the Mac). Each `make_gold.sh` regenerates the gold from the PLAIN pairwise path
(parallel_stereo + point2dem on a FIXED `--tr`/`--t_srs` grid + dem_mosaic), so the tool
output is byte-identical to the gold - the way to prove distributed == pairwise.

**Notes**: `~/projects/multi_stereo_dist_notes.sh` (design, work log, pfe runs).
