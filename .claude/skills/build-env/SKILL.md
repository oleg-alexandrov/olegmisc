---
name: build-env
description: Building and packaging ASP and its deps - nightly build/regression, asp_deps cloud tarballs, release packaging, the asp_deps conda build env, the Qt6 plugins symlink ban, honest non-editable installs, conda channel cleanup, running the regression tests, cmake/build mechanics, ISIS ninja builds, and reading build warnings. Load before building ASP/VW/ISIS, editing CMake, packaging a release, or running the test suite.
---

## Nightly Build and Regression Tests

Cron job on lunokhod1 at 23:05 runs the full build/test/release pipeline
for Linux (local) + macOS x64/arm64 (GitHub Actions). Full reference:
`~/projects/nightly_regression.sh`. Key files in
`~/projects/BinaryBuilder/auto_build/` (launch_master.sh, build.sh,
run_tests.sh, utils.sh). Email via msmtp on completion. Mac CI gold
updates: `~/projects/update_cloud_tests.sh`.

## Nightly asp_deps Cloud Tarballs (CRITICAL - canonical location)

The Mac/Linux cloud nightlies build ASP against a prebuilt asp_deps tarball per
platform, stored as BinaryBuilder GitHub releases (conda-pack of the asp_deps
env, plus python_isis10):
- `asp_deps_mac_arm64_v4`, `asp_deps_mac_x64_v4` (x64 may be split p1/p2 for the
  2 GB limit), `asp_deps_linux_arm_v1`, and the linux-intel one.
CANONICAL per-platform worklogs (the recipe to build/repackage/upload these):
`~/projects/env_update_06_2026_{mac_arm,mac_intel,linux_arm,linux_intel}.sh`, plus
the repackaging plan `~/projects/mac_arm_deps_tarball.sh`. (A CaSSIS-capable-deps
sub-job lives in `~/projects/cassis_asp/asp_cassis_deps_plan.sh`; that is a
specific task, not the general recipe.)

- **The ACTUAL fetch/extract/relocate mechanics live in
  `StereoPipeline/.github/workflows`** (`build_test.sh`, `build_test_linux_arm.sh`,
  `save_mac_deps.sh`, `save_linux_deps.sh`) and MUST be respected: the tarball
  naming (`asp_deps_p1.tar.gz` [+p2], `python_isis10.tar.gz`), the tag synced in
  build_test.sh, `conda-unpack` on the runner, and the MAC-ARM-ONLY ad-hoc
  `codesign --force -s -` re-sign after conda-unpack (arm64 SIGKILLs binaries
  whose signature conda-unpack invalidated). Do not change the tarball format
  without updating those scripts.
- Stereo correlation plugins (mgm, mgm_multi, msmw, msmw2, libelas) live in the
  packed env's `plugins/stereo/<algo>/bin`. `plugins/` MUST be an honest real
  directory, NEVER a symlink to `lib/qt6/plugins` (see the Qt6-plugins rule
  above). Plugin binaries must be relocatable: on Mac their `LC_RPATH` is
  `@loader_path/../../../../lib/` (Linux uses `$ORIGIN/../../../../lib`), matching
  the existing mgm plugin, NOT an absolute build-time path.
- To ADD a plugin binary to an existing tarball (no full deps rebuild): NO
  version bump. Download the release asset, extract, drop in the platform-built
  relocatable binary, ensure `plugins/` is a real dir, re-tar, and re-upload to
  the SAME tag with `gh release upload <tag> --clobber`.
- Watch storage: each tarball is ~1-2 GB; extract in a scratch dir, wipe it after,
  and never leave stray conda-pack scratch or half-extracted envs around.

## ASP Release Packaging

```bash
cd ~/projects/BinaryBuilder
./make-dist.py ~/projects/StereoPipeline/install \
  --asp-deps-dir /swbuild/oalexan1/miniconda3/envs/asp_deps \
  --python-env /swbuild/oalexan1/miniconda3/envs/python_isis9
```
First arg = dev build install dir (real ELF binaries, NOT a packaged release with
wrapper scripts). `--asp-deps-dir` = ASP deps conda env. `--python-env` = the
small `python_isis9` (~320 MB), NOT full `asp_deps` (~6 GB) or it bloats. Build
details: `~/projects/install_asp_notes.sh`.

## Building the asp_deps Conda Build Env

Make a clean `asp_deps` build env from a fresh `stereo-pipeline` conda package
(all deps + compiler, then strip vw/asp): `~/projects/make_asp_deps_env.sh`.
One rule to remember without reading: do NOT use `conda remove --force-remove`
(it also removes bundled third-party libs like libnabo that can't be reinstalled).
Instead, surgically `rm` only `libAsp*.so`, `libVw*.so`, `include/{asp,vw}`, and
ASP tools from `bin/` (using the dev install as the reference list). See the
notes file for the exact commands.

## NEVER symlink `$PREFIX/plugins` to Qt6 plugins (CRITICAL)

Qt6 plugins live in `$PREFIX/lib/qt6/plugins`, NOT `$PREFIX/plugins` (that was
the Qt5 location). ASP's OWN stereo correlation plugins (the external algorithms
mgm, mgm_multi, msmw, msmw2, libelas) live in `$PREFIX/plugins/stereo`. These are
two different owners of two different paths and they do NOT collide. There was a
bad workaround (make_asp_deps_env.sh, and a stray symlink on asp_deps) of
`ln -s lib/qt6/plugins plugins`, done only to make `$PREFIX/plugins` exist so
ASP would not throw "Cannot find Qt plugins" - it silently CLOBBERS
`plugins/stereo`, so `parallel_stereo` loses every external algorithm. That was
never acceptable. NEVER do that symlink. Keep `$PREFIX/plugins` a REAL dir
holding only `stereo/`. The real fix (belated bugfix, 2026-07): ASP's
`src/asp/Core/EnvUtils.cc` `set_asp_env_vars()` now sets `QT_PLUGIN_PATH` to
`lib/qt6/plugins:plugins` (Qt6 first, Qt5 fallback), matching the tarball wrapper
`BinaryBuilder/dist-add/libexec/libexec-funcs.sh`. Full write-up:
`~/projects/mgm_multi_notes.sh`.

## Honest Install into asp_deps, NEVER an Editable/`-e` Link (CRITICAL)

NEVER `pip install -e` (editable) a dev package (ale, usgscsm, etc.) into the
shipping `asp_deps` env. An editable install only drops a `.pth` that redirects
`import <pkg>` to your source tree - the env does NOT contain the package, it
borrows it. Worse, it deletes the conda package's real files, so `conda-pack`
then fails ("Cannot pack an environment with editable packages") and the tarball
carries a dangling `.pth` pointing at a path no consumer has. The `--ignore-
editable-packages` conda-pack flag is a HACK that hides this - do NOT reach for it.
Rule: when developing ale/usgscsm/etc. and you want the change in `asp_deps`, do
an HONEST non-editable install every time (more work - you must remember to
reinstall after each edit, but it is honest and self-contained):
  pip uninstall -y <pkg>
  pip install --no-deps --no-build-isolation ~/projects/<pkg>
For a truly legit conda env, rebuild the conda package from the local source
(bump the build string) and `conda install --force-reinstall` it. Do editable
DEV work in a SEPARATE env or a throwaway clone, never in the env that becomes
the deps tarball. (Bit us 2026-07-16..20: an editable ale in asp_deps forced
`--ignore-editable-packages` and shipped a dangling `.pth` in the linux tarball.
Again 2026-07-27: a standalone `ale` conda env was a `pip install -e` editable
overlay with no built ale_c, so `import ale` was broken - wiped it and installed
ale honestly from source into asp_deps.) **ALE build + honest-from-source-install
recipe (the ale_c SWIG ext is Eigen/json only, easy with clang+swig; verify by
`isd_generate`, not by importing ale_c), plus the no-pip-hack rule and the
standalone-env cleanup: `~/projects/isis_ale_rebuild_notes.sh`.**

## Conda Channel Cleanup (prune old asp_N builds)

After a re-spin, prune superseded `asp_N` conda builds on the
nasa-ames-stereo-pipeline channel with `~/bin/wipe_old_asp_conda.sh`:
keeps ONLY the highest `asp_N` per (package, platform), dry-run by default
(`--go` to apply). Guarded so it never removes a build unless the keeper exists
(`anaconda show && anaconda remove`), and removals are subdir-qualified so it
never crosses platforms. Always wipe per-platform, never touch non-asp/older
stable builds.

**Conda channel_priority MUST be `flexible`** on every machine (l1, pfe, Mac).
`strict` blocks cross-channel resolution (e.g. bullet from conda-forge when
our channel is listed first). Our `=asp*` build-string pins protect against
conda-forge swapping our packages; `flexible` just lets deps like bullet
resolve from whichever channel has them. Check: `conda config --show
channel_priority`. Fix: `conda config --set channel_priority flexible`.

## Running Tests

**Suite:** `~/projects/StereoPipelineTest`. Full guide - layout, **the CRITICAL
env setup** (conda + ISISROOT + PATH, else parallel_stereo/validate.sh crash),
tolerances, triage, gold regen, finding test dirs by tool:
`~/projects/asp_regression_tests.sh`. Mac CI: `~/projects/update_cloud_tests.sh`.
Run a test: `cd` in, `bash run.sh > output.txt 2>&1`, then `bash validate.sh`
(exit 0 = pass). NOT pytest.
- **Need test inputs (cameras/DEMs/images) to exercise an ASP tool or to
  reproduce/validate a bug? LOOK IN THE TEST SUITE FIRST** - `StereoPipelineTest/ss*`
  (`grep -rl <tool> ~/projects/StereoPipelineTest/ss*/run.sh`), plus in-repo
  `StereoPipeline/examples/` and `src/**/tests/`. Hundreds of ready cameras/DEMs
  exist; reuse or adapt one (e.g. punch a nodata hole into an existing DEM). Better
  still, if you have the real inputs that triggered the bug, replicate with THOSE,
  not a synthetic stand-in. Hand-roll a fake case only as a last resort. Detail:
  `~/projects/asp_regression_tests.sh` ("FINDING TEST INPUTS" section).
- **When asked to evaluate FAILING regressions, FIRST `git fetch` + rebase the
  latest from the remote for BOTH VW and ASP** (`god/master`). The local source
  can be behind what the nightly built, so a local re-run silently uses stale
  libs and disagrees with the nightly. Rebuild+install the updated repo before
  concluding anything. (Burned 2026-07-06: local VW was 2 commits behind a
  ray-DEM intersection change, so local tests wrongly "passed".)
- **A FAILED NIGHTLY - triage, regold, republish (ONE runbook, do not
  rediscover):** which platform + which tests = `report.txt` (test root, the
  full localLinux suite) and `~/projects/BinaryBuilder/status_master.txt`
  (per-platform); the REAL per-test output is the `ss*/run/` dirs. **TRAP: the
  `ss*/validate_out.txt` files are STALE manual scratch (check mtime), they do
  NOT reflect the nightly - ignore them; the honest diff is re-running
  `validate.sh` on `run/`.** Rank failures fast: `bash bin/triage_fails.sh` in
  the suite. When benign (float/threshold drift after an intentional change),
  regold the failing names with `python2 bin/runs_to_golds.py <names>`, then
  force-publish via resume (flip `status_localLinux.txt` to Success, then
  `launch_master.sh resume`). Full linear runbook + a worked 2026-08-07 example:
  `~/projects/nightly_regression.sh` ("FAILED NIGHTLY -> REGOLD -> REPUBLISH"),
  triage detail in `~/projects/asp_regression_tests.sh` ("TRIAGING A FAILED NIGHTLY").
- **MANDATORY: run regression tests after every ASP code change** - find ALL
  matching dirs (`grep -rl <tool> ~/projects/StereoPipelineTest/ss*/run.sh`) and
  run them all, not just one; flag if a changed path has no test coverage.
- **NEVER git add `run/` or `gold/`** (~40 GB, gitignored); only `run.sh` /
  `validate.sh` are tracked. `chmod +x` new ones.
- **Cloud tests (CRITICAL): the 3 cloud nightlies (Mac ARM64, Linux ARM) run only
  a SMALL subset bundled in `StereoPipelineTest.tar`** (release 0.0.1 on the
  NeoGeographyToolkit/StereoPipelineTest repo). A test is in the subset iff its
  `run.sh` has the `CLOUD-MAC TEST` marker; the list lives in
  `StereoPipelineTest/README.txt`. To add a cloud test: put the dir in the repo
  (so it runs on l1 too), add the marker, list it in README.txt, and rebuild the
  tarball (download, add the dir WITH its gold and any new `../data`, re-tar,
  `gh release upload --clobber`). Reuse data already in the tarball to keep it
  small; data lives in `../data`, never in the test dir; `validate.sh` uses a
  tolerant `max_err.pl` compare. `ssCSM_seedMode3` is the sparse_disp guard. Full
  detail: `~/projects/asp_regression_tests.sh`.

## CMake and Build Mechanics

Full cmake/build mechanics (glob/touch rules, native vs cross-compile build
dirs, building docs): `~/projects/cmake_build_notes.sh`. Read it before
non-trivial build work. Bare minimum to remember without reading:
- Adding/removing/moving a source file: `touch` the CMakeLists in that dir AND
  the parent to force a re-glob. Don't touch for content-only edits.
- That touch is LOCAL only - NEVER commit a CMakeLists change just to force a
  re-glob for others (git ignores mtime; other contributors don't build like us).
- Native = `build/` + `install/`; cross-compile = `build_linux/` + `install_linux/`.
  NEVER mix them - it destroys the other build.
- NEVER run ASP tools (esp. Python ones) from the source tree - it litters
  `src/asp/Python/__pycache__/`, `src/asp/Tools/__pycache__/`. `make install` and
  run from `install/bin`.

## Advise on Build and Tool Warnings, Never Silently Ignore

When a build, compile, link, or tool run emits warnings (even when it succeeds),
do not skip past them. Read them, explain the root cause in plain terms, say
whether they are harmless or a real problem, and recommend a fix. Explain
warnings, do not sweep them under the rug. Example: the macOS
"dylib was built for newer macOS version (16.0) than being linked (11.0)" linker
warnings traced to conda deps built for the host OS floor instead of the
intended 11.0 floor. Even cosmetic warnings deserve a one-line "this is
harmless because X" rather than silence.

## ISIS Builds Use Ninja, Not Make (CRITICAL - stop rediscovering this)

ISIS3 build uses **Ninja**, not make: `ninja -j8 install` from the build dir
(`make install` errors). CMake source root is `ISIS3/isis` (`cmake ../isis`), not
the repo root. To build libs+apps without tests you MUST set `-DBUILD_CORE_TESTS=OFF`
(`-DBUILD_TESTING=OFF`/`-DbuildTests=OFF` alone are insufficient - gtest still
fails to link). Building with tests needs the gtest submodule
(`git -C ~/projects/ISIS3 submodule update --init --recursive`) and `ISISROOT`
set (discovery runs the test binary at build time). Full flags and gotchas:
`~/projects/isis_2026/isis_2026_notes.sh`; also
`~/projects/isis_mapproject/isis_mapproject_notes.sh` and `~/projects/env_update.sh`.

**NEVER install a coverage-instrumented ISIS into `asp_deps` (CRITICAL).** Always
build `-DbuildCoverage=OFF`. Coverage instruments the WHOLE ISIS lib set (~142 libs:
libisis ~676 MB plus every mission/camera/projection plugin `.so`, all with baked
`.gcda` paths). Any that get bundled into the ASP nightly HANG ASP tools for minutes
at exit on pfe/Athena (libisis hangs all tools, plugin libs hang on demand). After any
ISIS build verify no ISIS lib is dirty: `cd $CONDA_PREFIX/lib; for f in lib*.so; do
strings $f|grep -q '\.gcda' && echo DIRTY $f; done` (prints nothing; libisis ~26 MB not
~676 MB). Full incident, fix, and `GCOV_PREFIX=/tmp` workaround: `~/projects/isis_2026/isis_2026_notes.sh`.
