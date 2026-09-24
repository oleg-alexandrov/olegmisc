---
name: isis-build
description: >-
  Build, test, and run DOI-USGS ISIS3 from source - the isis_dev conda env (NEVER
  asp_deps), the canonical Ninja build + install recipe, the coverage-instrumented-libisis
  disaster that hangs every ASP tool, the ninja-install deadlock on a stale env libisis and
  its PRE_TEST fix, running one gtest via runISISTests, ctest, the two ISISROOT values, the
  blocking-Qt-GUI gotcha, and the gh-api-not-gh-view workaround. Load before building,
  installing, testing, or running ISIS3, editing its CMake, or debugging a stale/hung ISIS
  build. Deep detail + per-issue history: ~/projects/isis_2026/isis_2026_notes.sh; PR /
  changelog mechanics: the isis-commit skill.
---

# Building, testing, and running ISIS3

Canonical build recipe, env, and gotchas. Exhaustive flag detail and the day-to-day
per-issue log live in `~/projects/isis_2026/isis_2026_notes.sh` (deep reference); PR and
towncrier-changelog mechanics live in the **[[isis-commit]]** skill.

## Env: isis_dev, NEVER asp_deps (CRITICAL)

ALWAYS build and test ISIS in the `isis_dev` conda env. NEVER install ISIS into `asp_deps`:
that is the ASP production deps env, and overwriting its libisis/tools corrupts ASP. Only
target `asp_deps` when deliberately patching the ASP production env (e.g. after an ale/usgscsm
bump), and re-verify it is clean afterward (see the coverage disaster below).

Two DIFFERENT `ISISROOT` values, do not mix them:
- BUILDING/gtests: `ISISROOT` = the BUILD dir (`cd ~/projects/ISIS3/build && export
  ISISROOT=$(pwd)`) - needed for gtest discovery.
- RUNNING installed tools: `ISISROOT` = the conda env (`export
  ISISROOT=$HOME/anaconda3/envs/isis_dev`), plus `$ISISROOT/bin` on PATH.

Always also set `ISISDATA=$HOME/projects/isis3data`,
`ISISTESTDATA=$HOME/projects/isis_test_data`, `SPICEQL_CACHE_DIR=/tmp/spiceql_cache`. NEVER
delete isis3data (~110-179 GB kernels) or isis_test_data (~19 GB) - both are used by every
ctest run and take a very long time to re-fetch.

## Build + install (Ninja, not make)

Source root is `ISIS3/isis`, generator Ninja; `make install` errors. Init the gtest submodule
once after a fresh clone: `git -C ~/projects/ISIS3 submodule update --init`. `$PREFIX` is the
TARGET conda env (isis_dev for dev/ctest). Python is 3.13.

```
eval "$($HOME/anaconda3/bin/conda shell.zsh hook)"
PREFIX=$HOME/miniconda3/envs/isis_dev
conda activate "$(basename $PREFIX)"
export ISISDATA=$HOME/projects/isis3data
export ISISTESTDATA=$HOME/projects/isis_test_data
export SPICEQL_CACHE_DIR=/tmp/spiceql_cache
cd ~/projects/ISIS3/build && export ISISROOT=$(pwd)
cmake -DJP2KFLAG=OFF -GNinja ../isis \
  -DCMAKE_BUILD_TYPE=Release \
  -DbuildTests=OFF -DbuildCoverage=OFF \
  -DBUILD_CORE_TESTS=OFF -DSENSOR_UTILITIES_BUILD_TESTS=OFF \
  -Dpybindings=OFF \
  -DCMAKE_INSTALL_PREFIX=$PREFIX \
  -DVulkan_INCLUDE_DIR=$PREFIX/include \
  -DOPENCV_INCLUDE_DIR=$PREFIX/include/opencv4 \
  -DPython3_EXECUTABLE=$PREFIX/bin/python \
  -DPython3_INCLUDE_DIR=$PREFIX/include/python3.13 \
  -DPython3_LIBRARY_RELEASE=$PREFIX/lib/libpython3.13.so
ninja -j16 install
```

- **ALWAYS `ninja install`, never bare `ninja`** - the test binary and tools rpath-load
  libisis from the CONDA ENV, so without install your edits run against the STALE env libisis
  and appear to do nothing (no effect, debug prints do not fire).
- Need ninja on PATH; if `$PREFIX` lacks it, APPEND `~/miniconda3/envs/tools/bin` (keep the
  env's own cmake first - it must be >= 3.20; the tools cmake 3.15 is too old).
- To BUILD THE GTESTS, drop the four `*TESTS/coverage=OFF` flags (build WITH tests) but keep
  `-DbuildCoverage=OFF` (see the coverage disaster). Flag gotchas: `buildCoverage` defaults ON
  and FORCE-caches `-g --coverage` into `CMAKE_CXX_FLAGS` (a ~645 MB instrumented libisis, and
  it fails configure without gcovr) - if you ever configured with it ON, `rm build/CMakeCache.txt`
  and reconfigure clean. `buildTests=OFF` does not stop SensorUtilities building its own gtest
  (needs `-DSENSOR_UTILITIES_BUILD_TESTS=OFF`); `-DBUILD_CORE_TESTS=OFF` is a third switch. conda
  Qt6 needs `-DVulkan_INCLUDE_DIR=$PREFIX/include` or configure errors.

## A coverage-instrumented libisis in asp_deps HANGS every ASP tool (CRITICAL)

If a coverage-instrumented ISIS lib set lands in `asp_deps` (buildCoverage ON, then packaged),
EVERY ASP tool hangs for minutes at process EXIT: the gcov runtime tries to write `.gcda` to a
build path that does not exist on the run host and blocks in fs syscalls. It is NOT just
libisis - the WHOLE ISIS lib set is instrumented (~142 libs: libisis*, every mission lib, and
every dlopen'd camera/projection plugin), so all must be replaced. ASP/VW libs are clean; it is
an ISIS-only packaging leak, and it already shipped in a nightly once (2026-07-11).

VERIFY after EVERY ISIS build, before anything can package asp_deps (scan the WHOLE lib set by
baked `.gcda`, not just libisis):
```
cd $CONDA_PREFIX/lib; for f in lib*.so; do c=$(strings "$f"|grep -c '\.gcda'); \
  [ "$c" != 0 ] && echo "DIRTY $f $c"; done   # must print nothing
ls -la $CONDA_PREFIX/lib/libisis10.0.0.so     # ~26 MB clean, ~676 MB dirty
```
If dirty: `rm build/CMakeCache.txt`, reconfigure with `-DbuildCoverage=OFF`, `ninja install`,
re-verify. Emergency workaround for an already-shipped dirty build: run ASP tools with
`GCOV_PREFIX=/tmp GCOV_PREFIX_STRIP=99` (gcov writes land in node-local /tmp, tools exit clean).

## ninja install deadlock on a stale env libisis, and the real fix

Symptom during `ninja install`: `FAILED: tests/runISISTests[1]_tests.cmake` with a
`Symbol not found` in the env's `libisis10.0.0`. Cause: `ninja install` runs
`gtest_discover_tests` on `runISISTests` at BUILD time; that binary rpath-loads libisis from
the conda env, and if the env lib is older than the freshly-built sources (a new symbol was
added) discovery fails, ninja stops, and the libisis install rule is never reached -
self-perpetuating (install can't run because env is stale, env stays stale because install
can't run).

REAL FIX (do once per build dir): move discovery to TEST time so install never runs the test
binary against the stale env lib:
```
cd ~/projects/ISIS3/build && cmake -DCMAKE_GTEST_DISCOVER_TESTS_DISCOVERY_MODE=PRE_TEST .
```
(built-in CMake global, 3.18+). Then a plain single `ninja install` always works, even right
after adding a new libisis symbol. FALLBACK if you already hit the deadlock: `cmake --install .`
(bypasses the dep chain), then `ninja install` again.

## Tests

- After install, run ctest by pattern: `cd ~/projects/ISIS3/build && ctest --output-on-failure
  -R <pattern>`.
- ONE gtest, fast dev loop: the gtest sources compile into a single binary target
  **`runISISTests`** (NOT `runTests`). Rebuild lib + that binary, then filter:
  `ninja -j16 install runISISTests` then
  `./tests/runISISTests --gtest_filter='CSMSerialNumber.*'` (from the build dir). Prefer the
  gtest binary over driving apps like campt by hand for camera round-trip checks - it never
  pops a GUI.
- Platform FP fragility: many ISIS tests use near-exact or ~1e-8 tolerances and fail on Mac
  ARM64 against x86-generated truth. A handful of failing camera/calibration FP tests
  (cassini vims, DawnVir, Clipper PushBroom, MRO ctxcal, ...) are usually PRE-EXISTING and
  platform-related, NOT caused by your change - confirm by reverting your file, rebuilding,
  and re-running the same tests (same failures = not yours).

## Running ISIS tools - the blocking-GUI gotcha (CRITICAL for automation)

An ISIS app launched with NO command-line options pops up the Qt GUI and BLOCKS (hangs the
shell, must be killed); a malformed call that leaves a required arg empty can also trigger it.
Always pass options (`from=`, `to=`, `sample=`, ...) or `-help`/`--help` to stay
command-line. If something hangs: `pkill -9 -x <app>` (e.g. `campt`).

## gh on DOI-USGS/ISIS3: use `gh api`, not `gh view` (CRITICAL)

`gh issue view` and `gh pr view` error out with a GraphQL "Projects (classic)" deprecation
message on DOI-USGS repos - unusable. Go via REST (gh is not on PATH; use its env path):
```
gh=/Users/oalexan1/anaconda3/envs/gh/bin/gh
$gh api repos/DOI-USGS/ISIS3/issues/<N> --jq '{title, state, body, labels: [.labels[].name]}'
$gh api repos/DOI-USGS/ISIS3/pulls/<N>  --jq '.body'
```
`gh issue list/close/create`, `gh pr list/create`, `gh run list/view` all still work.

## Related

- **[[isis-commit]]** - opening a PR to DOI-USGS/ISIS3: towncrier changelog fragment, the PR
  template + checkboxes, the testing/reproducibility expectations, AI attribution, and the
  push-to-`oleg`-never-`origin` rule (origin = DOI-USGS upstream, oleg = fork).
- **[[isis-data]]** - mission cubes, spiceinit, kernel downloads.
- **[[build-env]]** - the broader ASP/VW/deps build and packaging.
- Deep reference + per-issue history: `~/projects/isis_2026/isis_2026_notes.sh`.
