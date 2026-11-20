# Claude Code Memory - oalexan1 Home Dir Project

Temporary scratchpad for in-flight reminders and active project pointers.
Durable engineering rules and workflows belong in skills (~/.claude/skills/)
and CLAUDE.md per the auto-memory lifecycle policy.

## Immediate Reminders
- [nasa-mac-todo](nasa-mac-todo.md): OPEN nasa_mac leftovers (wipe the Miniforge installer,
  confirm claude/gh auth, first test of tunnel.sh on neon) once the tunnel is back.
- KH-9 FY27 sprint (2026-11-18 late): per-sensor --intrinsics-to-share DONE (ASP
  a353801d1 + docs 515be374e/4873d1335 pushed, new nightly test
  ss_bundle_adjust_per_sensor_share pushed + gold seeded on l1, reply posted to
  Shean). Option C effectively closed (no gain at 1/16). NEXT: orbit constraint
  prep (Task 4, Oleg promised). Waiting: shared-velocity DEM comparison, build
  tests, Task 5 spec, Task 7 slide. PUSHED 2026-11-20: cost print 8d5983188 + orbit 816c431e5
  (god + origin). sfs_blend 0f9a5cca9 local, the SfS bot's, held by Oleg.
  THIS BOT OWNS ALL KH-9 WORK (Oleg 2026-11-19), only sfs_blend is not ours. REMIND
  Doc posts DONE by Oleg 2026-11-20 (cost print + orbit blurb), await Shean.
  2026-11-19: Shean confirmed 8 items in the doc, 4 resolved, no blockers, no reply
  owed. ORBIT CONSTRAINT (option B, jitter_solve --orbital-group-list) IMPLEMENTED,
  UNCOMMITTED in Mac ASP (JitterSolveOrbit.{h,cc}, test, docs, news). Casa Grande
  eval DONE 2026-11-19 (works as designed, does not fix heights, ECEF = stereographic, commit the ECEF version). Ran on pfe via a private pkg StereoPipeline_orbit (l1 build in a separate
  clone StereoPipeline_orbit). Resume from "CASA GRANDE ORBIT EVAL ON pfe" at the
  end of ~/projects/kh9_fy27/kh9_fy27_notes.sh.
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
  tar wiped from pfe. ASP sfs_blend.cc smooth-min change COMMITTED LOCALLY 149951a5f
  (2026-11-18, Oleg's call), NOT pushed. REMIND Oleg: re-blend each WHOLE site once it lands.
  SfsPipeline 0711091 PUSHED 2026-11-17: fixed-camera trap doc + ba_fixed_check.py,
  vwip-only cleanup, LOW_LIGHT 0.03 now ON by default in launch_sfs_tiles.sh (shipped
  BCU/BCT SfS ran without it, readmes say 0.03). pfe SfsPipeline copy synced 2026-11-17.
  Notes: end of
  ~/projects/sfs_BCU2314-BDU1224-MM/sfs_bcu2314_bdu1224_mm_notes.sh.

- BCT max-lit (2026-11-19 late): jitter vs bundle per-image scan done (jitter better overall, 12 images bent
  by jitter). Conservative core max-lits (293/339) building. Dense-control jitter re-solve bctjdense 25411511
  (2M tri, 358k GCP, 1M anchors, max-pairwise 1000, from ba_htdem) -> jitter_ba_dense/, then remap + scan +
  nuke/replace. jitter_solve --fixed-image-list is IN god/master (0de185333). Notes: sections 29-30 of
  ~/projects/sfs_BCT0717-BCT2124-MM/sfs_bct0717_bct2124_mm_notes.sh.
- NEXT SfS (any site): MUST use --low-light-threshold (Oleg 2026-11-17). launch_sfs_tiles.sh
  passes 0.03 by default since SfsPipeline 0711091; never set LOW_LIGHT=none. HELD by Oleg,
  do not push yet: sfs_blend.cc (5e06259ff). --fixed-image-list already pushed (0de185333).

## Feedback / Lessons
- [Load product skill before shipping](feedback-load-product-skill-before-shipping.md) — writing to a delivery/results dir means load the delivery skill first (alt-10 hillshade slip, 2026-10-14).
- [Push authorization is per change](feedback-push-authorization-per-change.md) — a "push" covers only the change it was given for (per-anchor GSD slip, 2026-11-16).
- [Galleries are local HTML](feedback-galleries-local-html.md) — image galleries open locally in the browser, never as an Artifact (too big, 2026-11-20).

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
  held fixed instead of usgs_south ones (WORKFLOW.md 5b doc trap, since fixed).
  2026-11-18: second doc bug, section 7 mapproject passed a camera list that replaced the
  ba_htdem cameras (fixed, SfsPipeline 15c830c). Waiting on Scott: did he rerun step 7
  without it? NE notes end. Scott stops end of Nov 2026, then KH-9 for Shean.
- `~/projects/sdb/`: SDB (shallow depth bathymetry), FY26 USGS task.
- [ostfl](ostfl.md): OSTFL24, the grant to maintain and expand ASP (FY27 funded). Live
  notes `~/projects/ostfl_2026_notes.sh`. Full FY27 project list is in CLAUDE.md.
