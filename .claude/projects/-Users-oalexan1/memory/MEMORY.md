# Claude Code Memory - oalexan1 Home Dir Project

remind user in a day or two to go on with rescale logic plan for ww, so rescale for int images in vw. There should be some stuff later here as to what to do.

## Current Main Project

- [project_sdb.md](project_sdb.md) - **SDB (shallow depth bathymetry)**, FY26
  USGS task with PI Palaseanu, Co-Is Bhushan/Shean/Swinski. Work dir
  `~/projects/sdb/`. Sites: KeyWest1/2, PR1, WCaboRojo, CocosLagoon (WorldView-3).
  Allocation s3319. Masking benchmark (NDWI/RNDVI/OSI + Otsu) in progress.

## Recent Projects

`~/projects/sli_fusion/sli_fusion_notes.sh` - **SLI fusion** (2026-06-16, with
Shashank/sbhusha1). Orthoimage geolocation-assessment tool via ASP ip-matching:
ref + source orthos -> match points with dx/dy offsets to a GeoPackage (no
alignment). Cross-wavelength (lidar-intensity/NAIP vs WV/Planet) is the hard
part. Data (16 GB) `/nobackup/sbhusha1/sli_fusion/data`, alloc s3319. Detail +
inventory in the notes.

`~/projects/cassis_asp/cassis_notes.sh` - TGO CaSSIS (Mars, push-frame as
FRAMING) stereo in ASP (2026-06-10). Jezero pair MY37_028515_017_1/2. Distortion
now emitted by ALE (USGSCSM DistortionType CASSIS=9). Residual cross-track BOWL
is pipeline-specific (two-look BA near-degeneracy); real fix needs external
low-freq control (LOLA/CTX). Alloc e2305. Full diagnosis in the notes.

`~/projects/PNCB/pncb_registration.sh` - PNCB re-registration (spring 2026,
active as of 2026-04-21). Plan reorganized 2026-04-21 (context top, steps
in execution order below). See [project_pncb_rereg.md](project_pncb_rereg.md)
for key paths: `ref/pncb_lola_gcp.gcp` (2.5M LOLA-aligned GCP, reusable),
`ba_final/` (amended 301 cameras post re-reg).

`~/projects/binary_csm/binary_csm_notes.sh` - Binary CSM state format using
msgpack for ISDs and model states. FY26 USGS task. PR usgscsm#501 **merged
2026-03-25**. USGSCSM side complete. Remaining: update ASP CsmModel.cc to
use populateModel()/getModelJson() binary API instead of string serialization.

`~/projects/isis_jp2/isis_jp2_notes.sh` - Remove Kakadu from ISIS, use GDAL for
JPEG2000 reading. FY26 USGS task.

`~/projects/isis_mapproject/isis_mapproject_notes.sh` - ISIS cam2map / ASP mapproject
parity project with Kelvin Rodriguez (USGS). ASP_MAP mode in cam2map for per-pixel
exact projection. PR #5988. Mostly complete - in review.

`~/projects/spot5_csm/spot_csm_notes.sh` - SPOT5 CSM migration. **Merged
2026-03-29** on master (13 commits). CSM is the default path; old
SPOTCameraModel deleted. Uses TRANSVERSE distortion with f=1.0 angle-unit
detector. Shared helpers landed: refineCsmLinescanFit, isLinescanCsmSession,
CsmUtils resample. Reuse these for any SPOT 1-4 work.

`~/projects/spot1/spot1_notes.sh` - SPOT 1-4 (HRV) camera support plan (session
"spot14"). DIMAP v1.1 analysis, four transforms needed, readiness inventory.
Vendor doc `SPOT_Geometry_Handbook_CNES_S-NT-73-12-SI.pdf`. Deferred, not started.

`~/projects/csm_resample/csm_resample_notes.sh` - ALE ISD subsampling.
PR DOI-USGS/ale#677 **merged 2026-03-23**. Completed.

`~/projects/isis_jigsaw_isd/isis_jigsaw_isd_notes.sh` - **ACTIVE current project.**
Jigsaw with external ISD support. Read/write ISDs separately from cubes. In-memory
CSMState blob injection so jigsaw pipeline stays unchanged. Related to ISIS #5609,
#5998. FY26 USGS task with Kelvin Rodriguez. PR DOI-USGS/ISIS3#6004 open,
branch jigsaw_isdlist. Code reviewed and tested. Awaiting CI and review.

## Key Shell Files in ~/projects/ (Build & Test Reference)

| File | Purpose |
|------|---------|
| `isis_mapproject/isis_mapproject_notes.sh` | **Latest active work** - ISIS/ASP map projection parity (see above) |
| `install_asp_notes.sh` | **THE master build guide** - conda env setup, cmake flags, cross-compilation, ISIS versions, deps management |
| `asp_regression_tests.sh` | **Canonical ASP test suite reference** - suite layout, configs, tolerances, failure triage, release-vs-dev workflow, gold regen. Extend this, don't spawn parallel docs. |
| `update_cloud_tests.sh` | Mac nightly CI (GitHub Actions) - trigger flow, artifact download, tarball release update |
| `asp_refactor.sh` | Active refactoring work - PreFilter de-templatization, CorrEval changes, technical debt items |
| `vw_refactor.sh` | VW header splitting to reduce compile times (Manipulation, Algorithms, AlgorithmFunctions) |
| `vw_stereo_refactor.sh` | vw/Stereo/ cleanup - dead code removal, de-templatization |
| `rebuild_theia_deps.sh` | TheiaSfM rebuild in deps tarballs, cross-compilation ARM->x86_64 |

## ASP Build / Test Quick Refs

- Build order + cmake flags (VW first, then ASP, conda clang from asp_deps):
  `~/projects/install_asp_notes.sh`.
- Gold generation + dev-vs-release validate workflow, and Mac ARM64 pytest
  (`pytest -n 4 --config dev_mac_arm.conf`, ~234 pass / ~58 known-fail):
  `~/projects/asp_regression_tests.sh`.
- Conda channel priority for asp_deps: nasa-ames-stereo-pipeline,
  usgs-astrogeology, conda-forge, defaults (in that order).

## projwin_fix Work

All plan and work notes in `~/projects/projwin_fix.sh` (parts 1-14).
- 7 tests: 5 must not change on l1, 2 need gold regenerated
- dem_mosaic changes reverted (not shipped)
- All 7 pass on Mac with dev build

## VW default_rescale Migration

- [project_vw_rescale.md](project_vw_rescale.md) - DiskImageResource::default_rescale=true causes silent bugs when reading integer images as float. Report and migration plan in `~/projects/vw_rescale_default.sh`. Deferred to next session.

## Float Tolerance in Test Validation

Use `max_err.pl --relative --max-err 1e-6` for acceptable floating point diffs.
Refactoring often produces small float noise from evaluation order changes.

## Feedback

- [feedback_deliver_hosted_artifact.md](feedback_deliver_hosted_artifact.md) - For report/results deliverables, publish a hosted HTML Artifact (claude.ai link) via the Artifact tool, not just a local file.
- [feedback_scripts_in_workdir.md](feedback_scripts_in_workdir.md) - Write project scripts in the actual work dir (git-tracked), not the session scratchpad; scratchpad only for throwaway experiments.
- [feedback_dont_cover_bugs.md](feedback_dont_cover_bugs.md) - Don't mute a symptom with a workaround (symlink/fallback/copy/fixture); name and fix the actual defect. The Qt6 plugins symlink hid a one-line EnvUtils.cc bug for months.
- [feedback_stop_asking_permission.md](feedback_stop_asking_permission.md) - Don't ask permission to edit CLAUDE.md, config files, etc. Just do it.
- [feedback_inspect_before_filter.md](feedback_inspect_before_filter.md) - Set a filter/threshold only AFTER visually inspecting (colorize+eyeball) the raster; stats hide a coherent systematic signal (CaSSIS max-disp 10 cut real shift, left a tilt).
- [feedback_autonomous_safe_commands.md](feedback_autonomous_safe_commands.md) - When working autonomously, avoid command shapes that trip the sandbox (e.g. variable-path rm); cd + relative paths so you don't stall mid-stream.
- [feedback_casual_out_of_docs.md](feedback_casual_out_of_docs.md) - Keep jokes/banter out of notes, commits, PRs, docs, anything public; chat can be playful, the record stays professional.
- [feedback_batch_ssh_pfx.md](feedback_batch_ssh_pfx.md) - Batch all remote ops into ONE ssh pfx call; MOTD overhead is ~10s per call.
- [feedback_qsub_log_realtime.md](feedback_qsub_log_realtime.md) - qsub scripts should redirect output to a project-dir log file (not /dev/null) so user can tail -f while job runs.
- [feedback_qsub_umask_022.md](feedback_qsub_umask_022.md) - Every NAS qsub script needs `umask 022` (PBS default 0077 -> outputs 0600, owner-only); run the qsub_convention checklist when authoring/repurposing a script.
- [feedback_wipe_and_git_rm.md](feedback_wipe_and_git_rm.md) - When wiping scripts on pfx, always also git rm tracked paths on Mac in the same step.
- [feedback_precise_instructions.md](feedback_precise_instructions.md) - Do exactly what was asked, no extras. When unsure, ASK rather than do extra.
- [feedback_follow_explicit_plan.md](feedback_follow_explicit_plan.md) - When Oleg gives an explicit plan/directive, execute it; don't re-propose alternatives he already weighed.
- [feedback_give_honest_substantive_input.md](feedback_give_honest_substantive_input.md) - Oleg wants my real ideas/insight and disagreement (his intuition is "weak", he often doesn't know). Don't defer/flatter; push back when warranted; resolve with cheap parallel experiments.
- [feedback_only_sh_and_metadata_in_git.md](feedback_only_sh_and_metadata_in_git.md) - Default tracking is .sh + metadata/docs only. Data files of ANY kind (including small JSON state files, per-image lists, .txt inventories) stay untracked unless explicitly requested.
- [feedback_stop_when_in_ballpark.md](feedback_stop_when_in_ballpark.md) - Camera-model ports: stop at "plausible and pointing down" rather than churn through inherent vendor-doc/convention uncertainty.
- [feedback_qsub_script_chmod.md](feedback_qsub_script_chmod.md) - After rsync Mac to pfx, verify `chmod +x` on the pfx copy; PBS exits 254 in ~2s if missing.
- [feedback_full_libexec_rsync.md](feedback_full_libexec_rsync.md) - When syncing L1 ASP build to pfx, rsync the FULL install/bin → libexec, never selective. One stale stereo_* worker silently breaks the whole pipeline.
- [feedback_no_unprompted_issues.md](feedback_no_unprompted_issues.md) - Never `gh issue create` (or comment/close) without an explicit user instruction. "Track this" / "log this" means local notes only.
- [feedback_avoid_jargon_in_docs.md](feedback_avoid_jargon_in_docs.md) - In USGS/ISIS docs, changelogs, commits: avoid casual jargon. "spiceinit'd" -> "run through spiceinit"; "plumbing" -> "approach".
- [feedback_concise_vs_impl_detail.md](feedback_concise_vs_impl_detail.md) - PR bodies/summaries want what+why+validation, not algorithm derivation; impl detail lives in code. Lead concise, expand on request.
- [feedback_one_ai_disclaimer_per_pr.md](feedback_one_ai_disclaimer_per_pr.md) - One AI-assistance disclaimer per PR (in the body); don't repeat it on each comment.
- [feedback_ai_disclaimer_bot_only.md](feedback_ai_disclaimer_bot_only.md) - AI disclaimer describes the bot only; never mention the user's circumstances (hour, schedule, asleep).
- [feedback_say_passes_not_green.md](feedback_say_passes_not_green.md) - Say "the tests pass"/"the PR passes", not "CI is green".
- [feedback_no_caps_for_emphasis.md](feedback_no_caps_for_emphasis.md) - Don't ALL-CAPS words for emphasis (reads like shouting); real identifiers like PATH stay.
- [feedback_nested_ssh_quoting.md](feedback_nested_ssh_quoting.md) - Doubly-hopped ssh (pfe21 -> athfe01) eats $N awk fields and quotes; write a heredoc-with-quoted-EOF script file instead of inlining.
- [feedback_logs_at_workdir_top.md](feedback_logs_at_workdir_top.md) - Helper-script log files go at top of work dir (output_<jobName>.txt), not nested in subdirs - Oleg watches via ls+tail.
- [feedback_no_blocking_questions_when_autonomous.md](feedback_no_blocking_questions_when_autonomous.md) - When autonomous, never blocking AskUserQuestion; log decisions + default. Non-mandatory prose questions when done are fine.
- [feedback_docs_self_contained_sections.md](feedback_docs_self_contained_sections.md) - In RST/ASP docs, each section must stand alone; repeat key info in full + add a :numref: citation, don't slim to a bare cross-ref. Readers jump to one section and read in a hurry.
- [feedback_adapt_existing_workflow.md](feedback_adapt_existing_workflow.md) - When a project already has scripts/workflow, read and adapt them surgically (smallest diff); never improvise a new parallel workflow from scratch.
- [feedback_check_ground_truth_before_artifact.md](feedback_check_ground_truth_before_artifact.md) - Don't call a structured/repeating raster pattern a processing artifact without checking ground truth; it may be real (lunamaps painted-concrete target).
- [feedback_test_on_real_data.md](feedback_test_on_real_data.md) - Test on real representative data and eyeball the output; a synthetic test with degenerate inputs (identity geotransform) masked the sparse_disp pixel-vs-map-coords bug another bot caught by looking.

## Reference

- [reference_geodiff_no_csv_datum.md](reference_geodiff_no_csv_datum.md) - geodiff has NO --csv-datum (that is stereo_gui only); silently prints help and exits. Use --csv-format and let the DEM projection set the datum.

