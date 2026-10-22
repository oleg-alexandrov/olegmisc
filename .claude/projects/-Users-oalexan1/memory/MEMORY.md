# Claude Code Memory - oalexan1 Home Dir Project

Temporary scratchpad for in-flight reminders and active project pointers.
Durable engineering rules and workflows belong in skills (~/.claude/skills/)
and CLAUDE.md per the auto-memory lifecycle policy.

## Immediate Reminders
- ipfind --nodata-radius slowness (issue 255): DONE and CLOSED 2026-10-21. The reported
  orders-of-magnitude slowness was the pre-2018 per-pixel access on a lazy
  ImageViewRef<PixelMask> (image_mask(x,y).valid() up to 8 probes per point). VW commit
  a5af0a48 (2018-08-04) already fixed it by moving to vw::ip::remove_ip_near_nodata
  (InterestPointUtils.h), which crops one window into memory from the block-cached
  DiskImageView and scans it flat. Issue filed 2019-04 was a release-lag report. Oleg's
  Cache.cc hypothesis checked and REJECTED: Core/Cache.cc speedups are 2025-11 (lock/stats
  trim), unrelated; and shrinking VW cache to 8MB did NOT reslow current code, so speed is
  the bulk crop, not cache size. SEPARATELY fixed a real latent-UB bug in that function
  (erase-then-ip-- is undefined when the first point is erased, --begin()); rewrote to
  advance-only-when-kept, with auto iterators and i++ per Oleg's new style rule. Counts
  byte-identical (4514/4210/8403). COMMITTED 968ac52a and PUSHED to origin + god.
  cpp-editing skill now carries the i++-over-++i and auto-for-iterators rules. Issue 255
  closed with explanation + AI disclaimer. Nothing left open.
- dem_mosaic coverage-aware border blending (issue 241): DONE and PUSHED (origin + god,
  2026-10-21, commit 1bf3a9092). Coverage-aware grassfire seeding (priorityBlend fades
  only at no-data pixels another DEM covers, free edges stay sharp) is now the DEFAULT
  for priority blending. The `--no-border-blend` option was REMOVED entirely (code +
  docs + NEWS misc item). ss_dem_mosaic_priority regolded on l1 (benign float-noise
  diff, max 0.001 m on real fine/coarse data, no coverage change), 3 regular-blend
  dem_mosaic tests re-verified bit-identical, so the nightly is safe. Issue 241 closed
  with AI disclaimer. The leftover "blending not nice" (ramp shape / weight_modifier
  burning pass) is a SEPARATE open issue, not yet addressed. Only my 5 files were
  committed (BundleAdjustCamera.cc and VW InterestPointUtils.h are other bots' work,
  left untouched).
- jitter pixel-reprojection speedup: DONE and PUSHED (god + origin, 2026-10-20). The
  analytic-Jacobian path (97c3d7f58) was ABANDONED and collapsed out of history. The
  winner is the trimmed local linescan model (commit 2c805a87e, with e428d5aba find-match
  stat removal and a58aec1ac phase timers): now the ONLY path, no env gate, out-of-window
  points fall back to the full model, byte-stable to ~1e-9 with no cross-iteration drift,
  validated at full scale (jitsafe2 25290765, 978 cams, reproj 0.5408 matching baseline, no
  crash). The old unsafe-scratch build warning is MOOT. Two genuine follow-ups remain, both
  needing Oleg's "peek at the VW impl" (NOT unattended): (a) make pfe/l1 production builds
  current from clean master before any production jitter run. (b) the real at-scale cost is
  VW-bound (build-control-network match load+triangulate ~23m, reporting ~12m, serial anchor
  ~11m) blocked by the VW block-cache exploding under concurrent DEM reads. See
  `~/projects/sfs_BCU2314-BDU1224-MM/jitter_analytic_eval_notes.sh`.
- jitter_solve reporting re-read: fixed, COMMITTED LOCALLY (ASP 0614a722e, NOT pushed).
  matchFilesProcessing now skips re-reading match files when no clean matches are written
  (jitter_solve), rebuilding per-pair pixels from the control network. Verified byte-
  identical stats baseline-vs-fix; bundle_adjust path untouched. Remaining idea (bundle_adjust,
  which needs the IP scale to write clean matches): save the scale on first read in
  build_control_network so files are not read twice. Follow-up in `~/projects/todo.sh`.
- SfsPipeline integration (Andrew Annex repo): DONE and PUSHED across all three repos
  (verified 2026-10-20) - SfsPipeline main clean and pushed, the ~/projects OBSOLETE markers
  pushed, the olegmisc skill repoint pushed as 7d1d1fd. Repo is self-contained, scripts
  migrated, 44 projects/sfs twins marked OBSOLETE, 9 skills repointed to SfsPipeline/bin,
  jitter_gcp/bundle_adjust params folded in. (The "await Oleg's push" line in
  SfsPipeline_integration_notes.sh is itself STALE - ignore it.) Scripts vs sfs_usage.rst
  (StereoPipeline/docs) are now SYNCED (2026-10-20): refine (bundle_adjust_refine.sh) and
  jitter (jitter_gcp.sh) already matched the doc; the one gap was the initial
  bundle_adjust, fixed by adding --tri-weight 0.05 / --tri-robust-threshold 0.05 to
  bin/bundle_adjust.sh and pushed to SfsPipeline main. The other old Andrew ADVICE items
  (heights-from-dem-robust-threshold, matches-per-tile) are absent from BOTH script and doc,
  so nothing to reconcile - never adopted on either side. SfS integration is now DONE.
  Detail in `~/projects/sfs/SfsPipeline_integration_notes.sh`.

## Feedback / Lessons
- [Load product skill before shipping](feedback-load-product-skill-before-shipping.md) — writing to a delivery/results dir means load the delivery skill first (alt-10 hillshade slip, 2026-10-14).

## Active Projects
- `~/projects/sfs_BCT0717-BCT2124-MM/sfs_bct0717_bct2124_mm_notes.sh`: Artemis Quad
  BCT0717-BCT2124 (Mons Mouton west) SfS DEM. At delivery stage. SfS-to-LOLA dh/dv numeric
  bands produced 2026-10-15 (dh -0.313 / MAD 0.934, dv -0.082 / MAD 0.865), results + pfe
  synced. Keep hcorr_redo/ until the delivery is accepted.
- `~/projects/sfs_BCU2314-BDU1224-MM/sfs_bcu2314_bdu1224_mm_notes.sh`: Artemis Quad
  BCU2314-BDU1224 (Mons Mouton north) SfS DEM. At delivery stage. dh/dv done 2026-10-15
  (dh -0.043 / MAD 0.918, dv -0.034 / MAD 0.948, well registered), results + pfe synced.
  Keep hcorr_redo/ until accepted.
- `~/projects/sdb/`: SDB (shallow depth bathymetry), FY26 USGS task.
- [ostfl](ostfl.md): OSTFL24, the grant to maintain and expand ASP (FY27 funded). Live
  notes `~/projects/ostfl_2026_notes.sh`. Full FY27 project list is in CLAUDE.md.
