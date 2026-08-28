---
name: asp-regressions
description: Evaluate and triage a FAILED ASP nightly/regression - decide acceptable vs real, reset-to-god first, run-vs-gold coverage judgement (-tap + hillshade + dz median/NMAD), the zero-tolerance stat-diff trap, distinguishing a cloud BUILD break from test drift (fetch GH Actions logs), regold, relaunch launch_master, and iterate. Load whenever an ASP nightly fails and you must judge whether the failures are OK, fix a build break, regold, or re-run the nightly. Complements build-env (mechanics), local-epi-debug (tile notches), dem-comparison.
---

# Evaluating a failed ASP nightly / regression

The judgement layer on top of the build-env skill's mechanics. The question is
almost always: are these failures BENIGN (regrid/edge drift, regold) or REAL (a
missing DEM chunk, a build break, wrong heights)? Do not guess - inspect.

## STEP 0 - reset local to what the nightly built (do this FIRST)

The nightly builds **god/master**; your local l1 clone can be STALE (stuck behind,
with dead uncommitted experiments from a peer/Mac bot). A local re-run then uses
stale code and disagrees with the nightly. So before concluding anything:
- `cd ~/projects/StereoPipeline && git fetch god` (and VW too).
- Compare: `git log --oneline god/master..HEAD` (local-only) and
  `HEAD..god/master` (behind). If a peer bot pushed local_epi/other commits, local
  is BEHIND.
- If local has stale uncommitted experiments AND is not ahead: save the diff to
  scratch (`git diff <files> > /tmp/.../stale.diff`), then `git reset --hard
  god/master`, and remove stale untracked cruft (build logs, `.bak`) by explicit
  path. (See the repo-sync skill for the full stale-local-nuke pattern.)
- Rebuild+install before re-running any test. (Burned 2026-07-06: local VW 2 commits
  behind -> tests wrongly "passed". Burned 2026-08-23: local ASP stuck at an old
  HEAD while god had 15 newer local_epi commits.)

## Where the results live (and the traps)

- `~/projects/BinaryBuilder/status_master.txt` = per-platform overall (localLinux,
  cloudMacX64, cloudMacArm64, cloudLinuxArm64). On full success its last line is the
  releases URL; launch_master writes that ONLY when all pass.
- `status_<platform>.txt` = `<tarball> test_done Success|Fail` (or `now_building` /
  `now_testing` while live).
- `~/projects/StereoPipelineTest/report.txt` = the localLinux pytest summary
  (`FAILED test_run.py::test_run[<name>] ...`).
- The REAL per-test output is `ss<name>/run/` vs `ss<name>/gold/`. **TRAP:
  `ss*/validate_out.txt` is STALE manual scratch (check mtime) - ignore it; the honest
  diff is re-running `validate.sh` on `run/`.**
- Cloud (Mac/LinuxArm) run a SMALL bundled subset (`CLOUD-MAC TEST` marker,
  StereoPipelineTest.tar); their golds are in that tarball, compared TOLERANTLY
  (`max_err.pl`), unlike localLinux's exact diff.

## The zero-tolerance stat-diff trap (local_epi/mapproj DEM tests)

Many `validate.sh` do an EXACT `diff` of `gdalinfo -stats` (size, origin, pixel
size, min/max/mean/stddev, valid_percent). ANY nudge fails them. So a "fail" after
an intentional stereo/tiling change is EXPECTED and is NOT evidence of a real
regression. Judge the DEM itself, not the diff.

## Judge acceptable vs real: run-vs-gold coverage + dz

For each failing DEM test, put run and gold on ONE common grid and look:
- `gdalwarp -te <union> -tr <coarser> -tap -r near` BOTH run and gold to a common
  grid. **run and gold share the CRS -> do NOT pass `-t_srs` a raster path (it errors
  "Invalid SRS"); just use -te/-tr/-tap.** `-tap` is essential or the occupancy diff
  is garbage.
- Coverage: count valid px in each; LOST = valid-in-gold-but-not-run (the notch
  concern), gained = the reverse. dz where both valid: report **median + NMAD**
  (robust), not mean/std.
- Render: `gdaldem hillshade -multidirectional -compute_edges` both, plus a coverage
  map (red=lost, green=gained, gray=both). LOOK at it.

ACCEPTABLE (regold): terrain identical; differences are a sub-pixel origin/pixel-size
shift trimming one edge and adding the opposite (clean 1-px border in the coverage
map), or boundary speckle in naturally low-texture/patchy regions (balanced
lost/gained), small median dz. This is Oleg's "corners clipped/added by regridding".

NOT ACCEPTABLE (show-stopper - stop, report, do NOT regold): a COHERENT rectangular
missing block (a failed local_epi tile = a notch - see local-epi-debug), or wrong
heights (large median dz / a shifted surface). A missing DEM chunk from a failed tile
is a real bug.

SfM / Theia tests (e.g. ssPinHole_SfM_Moon) are documented NON-bit-exact; a clipped
marginal-overlap corner with identical heights (dz median ~0) is acceptable, regold.

## Remote failures are OFTEN a BUILD break, not test drift - CHECK

A cloud platform "failing" with every test erroring `run/run-DEM.tif does not exist`
usually means the BUILD produced no binaries. Fetch the GH Actions log and look:
```
GH=$(ls -d $HOME/*conda3/envs/gh/bin/gh)
$GH api repos/NeoGeographyToolkit/StereoPipeline/actions/runs/<RUNID>/logs > z.zip
mkdir z && (cd z && unzip -oq ../z.zip); grep -rinE "error:|Error 2|packages//bin does not exist" z/
```
`Directory: .../packages//bin does not exist. Build failed.` = the build died; all
downstream test "failures" are spurious. Find the real compile error above it. A
classic cause is a DEPS VERSION SKEW: ASP calls a symbol present only in a newer
lib on some platforms' asp_deps tarball (e.g. usgscsm `isUsgsCsmIsd` from PR 502 -
present on l1/Mac-ARM, absent on the older Mac-x64/Linux-ARM tarballs). The platform
that passed has the newer dep.

Fix a too-new-deps-symbol break by DECOUPLING ASP: add a small local copy of the
symbol inside ASP (pure, self-contained) instead of rebuilding every tarball -
faster and platform-robust. Leave a TODO to revert once all tarballs carry it.
Verify the fix is transparent (re-run a representative test; output should be
bit-identical). (Done 2026-08-23: local `aspIsCsmIsd` in CsmModel.cc.)

## Regold, relaunch, iterate

- Regold benign failures: `python2 ~/projects/StereoPipelineTest/bin/runs_to_golds.py
  <names>` (does `rm -rf gold; cp -r run gold`). Then re-run each `validate.sh` and
  confirm it passes. **gold/ and run/ (all test data) are NEVER committed to any
  repo - IRONCLAD policy, do NOT even ask.** Regolding writes gold/ on l1 disk only
  (gitignored, ~40 GB); it never enters git. Commits carry source + docs only (the
  lone binary exception is documentation figures). So a first nightly reddening on a
  brand-new test whose gold does not yet exist is EXPECTED - regold on l1 and move on;
  never propose adding the gold/run to git.
- Relaunch the nightly from scratch:
  `cd ~/projects/BinaryBuilder && nohup bash auto_build/launch_master.sh > output_master.txt 2>&1 &`
  (no arg = full rebuild+retest all platforms; `resume` = skip platforms already at
  test_done Success). Confirm the build's fresh clone is at your commit:
  `git -C build_asp/build/stereopipeline/stereopipeline-git log --oneline -1`.
- Monitor: arm a CronCreate heartbeat + OS watchdog (autonomous-ops skill). A
  platform is done when `status_<p>.txt` says `test_done Success|Fail`; all-green
  writes the releases URL into status_master.txt.
- Cloud tolerant compare often ABSORBS local_epi drift -> no cloud regold needed. If
  only minor Mac cloud drift fails: iteration-2 = regold the cloud subset + rebuild &
  push a new StereoPipelineTest.tar (build-env "Cloud tests") + relaunch. Up to two
  iterations.

## Pointers
- Mechanics, cloud-tarball format, regold/republish runbook: **build-env** skill and
  `~/projects/nightly_regression.sh`, `~/projects/asp_regression_tests.sh`.
- Tile notches / why a local_epi tile fails: **local-epi-debug** skill.
- DEM-vs-DEM dh/dv/dz depth: **dem-comparison** skill.
- Repo path/remote table, gh recipes: **git-repos** skill.
