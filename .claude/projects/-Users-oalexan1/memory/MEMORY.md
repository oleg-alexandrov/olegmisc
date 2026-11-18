# Claude Code Memory - oalexan1 Home Dir Project

Temporary scratchpad for in-flight reminders and active project pointers.
Durable engineering rules and workflows belong in skills (~/.claude/skills/)
and CLAUDE.md per the auto-memory lifecycle policy.

## Immediate Reminders
- KH-9 FY27 sprint (2026-11-16): Tasks 1, 2, 3, 4 (a8e9716bd) and 6 plus the
  per-anchor GSD (f34989d90) are pushed and regolded on l1. Waiting on Shean: Task 7
  (DSI-21 slide, no word on the review), Task 5 spec, his tests on the 11/17 and 11/20
  builds. Dropped until he asks: per-camera optical bar model print. Dropped:
  --min-distortion plumbing. Deferred: the VW block-cache work. Live status:
  ~/projects/kh9_fy27/kh9_fy27_notes.sh (end of file).
- sfs_south: SfsPipeline/bin synced Mac -> pfe on 2026-11-16, so the pending sync
  reminder is done (the pfe checkout is behind its files, harmless). Notes:
  ~/projects/sfs_south/overnight_plan.sh.
- jitter_solve at-scale speed: all app-level wins are pushed (trimmed linescan model,
  parallel match read, no second read). The one open item is VW-bound: anchor placement
  and the reporting mapproject are slow on DEM reads through the VW block cache. Needs a
  bounded per-camera DEM crop or thread-safe DEM access in VW (Oleg's look). Same
  bottleneck as KH-9 Task 3's deferred fix. Notes:
  ~/projects/sfs_BCU2314-BDU1224-MM/jitter_analytic_eval_notes.sh (end).

- Ross reviews of the two Mons Mouton SfS sites (2026-11-16): BCT0717 = other bot,
  BCU2314 = this bot. BCU max-lit 2026-11-18: recommended C8
  maxlit_cmp/cascade_c8_xclean08_le0.8_le1.0_c7T2_c7T3_c8T6_all334_f0.7_0.95.tif, gallery
  https://claude.ai/artifact/CwZyV7dek9FzgjcPdqiTtx v8, waiting on Oleg's look. BCU ghosting study done 2026-11-17: a few-images problem, not
  global. Joint jitter with GCPs does not visibly help, dropping 10 offenders does
  (maxlit_cmp/delivered_no_offenders.tif). Oleg decides drop vs SfS rerun. Staged cubs
  tar wiped from pfe. HELD, not committed: ASP sfs_blend.cc smooth-min change (walk Oleg
  through it before any commit: git -C ~/projects/StereoPipeline diff
  src/asp/Tools/sfs_blend.cc). REMIND Oleg: re-blend each WHOLE site once it lands.
  SfsPipeline 0711091 PUSHED 2026-11-17: fixed-camera trap doc + ba_fixed_check.py,
  vwip-only cleanup, LOW_LIGHT 0.03 now ON by default in launch_sfs_tiles.sh (shipped
  BCU/BCT SfS ran without it, readmes say 0.03). pfe SfsPipeline copy synced 2026-11-17.
  Notes: end of
  ~/projects/sfs_BCU2314-BDU1224-MM/sfs_bcu2314_bdu1224_mm_notes.sh.

- BCT max-lit (2026-11-17): recommended C4s_kd maxlit_cmp/cascade_ext_le1.0_le1.2_le1.3_fix9b_426_f0.75_1.0_kd.tif,
  gallery https://claude.ai/artifact/GZkogVwHBuvSSwUtELTTqA, waiting on Oleg's look. HELD, not committed:
  ASP jitter_solve --fixed-image-list (Mac + l1 trees, deployed on pfe). Notes: end of
  ~/projects/sfs_BCT0717-BCT2124-MM/sfs_bct0717_bct2124_mm_notes.sh.
- NEXT SfS (any site): MUST use --low-light-threshold (Oleg 2026-11-17). launch_sfs_tiles.sh
  passes 0.03 by default since SfsPipeline 0711091; never set LOW_LIGHT=none. Also HELD by Oleg,
  do not push yet: jitter_solve --fixed-image-list and sfs_blend.cc.

## Feedback / Lessons
- [Load product skill before shipping](feedback-load-product-skill-before-shipping.md) — writing to a delivery/results dir means load the delivery skill first (alt-10 hillshade slip, 2026-10-14).
- [Push authorization is per change](feedback-push-authorization-per-change.md) — a "push" covers only the change it was given for (per-anchor GSD slip, 2026-11-16).

## Active Projects
- `~/projects/sfs_BCT0717-BCT2124-MM/sfs_bct0717_bct2124_mm_notes.sh`: Artemis Quad
  BCT0717-BCT2124 (Mons Mouton west) SfS DEM. At delivery stage. SfS-to-LOLA dh/dv numeric
  bands produced 2026-10-15 (dh -0.313 / MAD 0.934, dv -0.082 / MAD 0.865), results + pfe
  synced. Keep hcorr_redo/ until the delivery is accepted.
- `~/projects/sfs_BCU2314-BDU1224-MM/sfs_bcu2314_bdu1224_mm_notes.sh`: Artemis Quad
  BCU2314-BDU1224 (Mons Mouton north) SfS DEM. At delivery stage. dh/dv done 2026-10-15
  (dh -0.043 / MAD 0.918, dv -0.034 / MAD 0.948, well registered), results + pfe synced.
  Keep hcorr_redo/ until accepted.
- Scott McMichael's three M2M SfS sites (Oleg reviews, Scott runs, pfe user smcmich1,
  work in /nobackupp18/smcmich1/data/<site>). One Mac dir + notes per site:
  BIG `~/projects/BDS0912-BDS1819-Nobile_NW_Rim/bds0912_bds1819_nobile_nw_rim_notes.sh`
  (ba_gcp done 2026-11-16, but SfS NOT rerun on those cameras, so sfs_dem and the blend
  are still ~4.6 m off LOLA, blurry areas open). SMALL, both with bad bundle adjust:
  `~/projects/AYN1112-AYN1516-de_Gerlache_NE_Rim/ayn1112_ayn1516_de_gerlache_ne_rim_notes.sh`
  (we can read it) and `~/projects/AXN2014-AYN0018-de_Gerlache_NW_Rim/axn2014_ayn0018_de_gerlache_nw_rim_notes.sh`
  (locked to us, Scott must chmod). NE root cause found 2026-11-17: plain SPICE cameras
  held fixed instead of usgs_south ones (WORKFLOW.md 5b doc trap, fix not yet OK'd). Scott stops end of Nov 2026, then KH-9 for Shean.
- `~/projects/sdb/`: SDB (shallow depth bathymetry), FY26 USGS task.
- [ostfl](ostfl.md): OSTFL24, the grant to maintain and expand ASP (FY27 funded). Live
  notes `~/projects/ostfl_2026_notes.sh`. Full FY27 project list is in CLAUDE.md.
