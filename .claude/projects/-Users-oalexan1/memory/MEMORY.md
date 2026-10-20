# Claude Code Memory - oalexan1 Home Dir Project

Temporary scratchpad for in-flight reminders and active project pointers.
Durable engineering rules and workflows belong in skills (~/.claude/skills/)
and CLAUDE.md per the auto-memory lifecycle policy.

## Immediate Reminders
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
- Standing: remind Oleg to go on with the rescale logic plan for vw (rescale for int images).
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
