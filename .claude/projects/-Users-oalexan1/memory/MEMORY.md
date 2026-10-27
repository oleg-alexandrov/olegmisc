# Claude Code Memory - oalexan1 Home Dir Project

Temporary scratchpad for in-flight reminders and active project pointers.
Durable engineering rules and workflows belong in skills (~/.claude/skills/)
and CLAUDE.md per the auto-memory lifecycle policy.

## Immediate Reminders
- sfs_south RE-REGISTRATION + TILED SfS LIVE (2026-10-27). Pushed SfsPipeline fixes to
  origin/main (790b39f): launch_sfs_tiles.sh silent-submit fix (unset IMAGE_DIR/IMAGE_SUFFIX
  in a qsub -v list -> PBS rejects job silently; now defaulted + exit-status check),
  GCP_ROBUST_THRESHOLD knob in bundle_adjust_dem_gcp.sh, WORKFLOW.md tiling (5km domain tiles
  at ~2500 px, exposures reused across a re-run). **NOT YET SYNCED TO PFE** (stale copy there).
  **REMIND OLEG: next time we do SfS work, rsync ~/projects/SfsPipeline/bin/ to pfe BEFORE
  using launch_sfs_tiles there.** Live jobs on pfe: site3 stronger-GCP bundle 25314941
  (gcp-sigma 0.25 + robust 40) + 12 tiled SfS jobs (sites 1/2/4, 4 tiles each). 3-hour
  CronCreate monitor armed (tag sfssouth). Live notes + NEXT steps:
  ~/projects/sfs_south/overnight_plan.sh (re-read it top-to-bottom on resume).
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
