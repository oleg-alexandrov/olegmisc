# Claude Code Memory - oalexan1 Home Dir Project

## Key Shell Files in ~/projects/ (Build & Test Reference)

| File | Purpose |
|------|---------|
| `install_asp_notes.sh` | **THE master build guide** - conda env setup, cmake flags, cross-compilation, ISIS versions, deps management |
| `build_test_asp.sh` | Test infrastructure - gold generation workflow, pytest config, single/batch test running |
| `asp_testing.sh` | Mac ARM64 test results summary - 234 pass / 58 expected fail out of 321 |
| `asp_refactor.sh` | Active refactoring work - PreFilter de-templatization, CorrEval changes, technical debt items |
| `vw_refactor.sh` | VW header splitting to reduce compile times (Manipulation, Algorithms, AlgorithmFunctions) |
| `vw_stereo_refactor.sh` | vw/Stereo/ cleanup - dead code removal, de-templatization |
| `rebuild_theia_deps.sh` | TheiaSfM rebuild in deps tarballs, cross-compilation ARM->x86_64 |

## ASP Build Order & CMake Flags (Mac ARM64)

Build VW first, then ASP. Both use conda clang from `asp_deps` env.

```bash
# VW
cmake ../src -DASP_DEPS_DIR=$HOME/miniconda3/envs/asp_deps \
  -DCMAKE_INSTALL_PREFIX=$HOME/projects/StereoPipeline/install \
  -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++

# ASP (after VW is installed)
cmake ../src -DASP_DEPS_DIR=$HOME/miniconda3/envs/asp_deps \
  -DVISIONWORKBENCH_INSTALL_DIR=$HOME/projects/StereoPipeline/install \
  -DCMAKE_INSTALL_PREFIX=$HOME/projects/StereoPipeline/install \
  -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++
```

## Test Gold Generation Workflow

1. Download latest release tarball (e.g., from GitHub releases)
2. Set PATH to release build: `export PATH=/path/to/release/bin:$PATH`
3. Run test: `bash run.sh > output_gold.txt 2>&1`
4. Copy output to gold/: the gold dir is the reference
5. Switch to dev build: `export PATH=~/projects/StereoPipeline/install/bin:$PATH`
6. Run same test again, then `bash validate.sh` to compare against gold

## Mac ARM64 Batch Testing with Pytest

```bash
conda activate pytest
cd ~/projects/StereoPipelineTest
pytest -n 4 -q -s -r a --tb=no --config dev_mac_arm.conf
# Expected: ~234 passed, ~58 failed (known issues / missing data)
```

## Conda Channel Priority for asp_deps

Order matters: `nasa-ames-stereo-pipeline`, `usgs-astrogeology`, `conda-forge`, `defaults`

## projwin_fix Work

See `projwin_fix_notes.md` for baseline test results and l1 TODO.
- Plan: `~/projects/projwin_fix.sh`
- 7 tests: 3 tap (should not change), 4 non-tap (may change after fix)
- New test `ss_point2dem_projwin` created, gold generated from dev build
- `ss_dem_mosaic_gdal_tap` was missing gold/run.tif on Mac, created it
- All 7 pass on Mac with dev build (pre-fix baseline)
- Mac needs: `ISISROOT=$HOME/anaconda3/envs/asp_deps` + asp_deps/bin on PATH

## Float Tolerance in Test Validation

Use `max_err.pl --relative --max-err 1e-6` for acceptable floating point diffs.
Refactoring often produces small float noise from evaluation order changes.
