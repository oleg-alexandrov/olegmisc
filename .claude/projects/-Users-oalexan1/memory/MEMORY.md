# Claude Code Memory - oalexan1 Home Dir Project

## Latest Work Doc

`~/projects/isis_mapproject/isis_mapproject_notes.sh` - ISIS cam2map / ASP mapproject
parity project with Kelvin Rodriguez (USGS). Adding ASP_MAP mode to cam2map for
per-pixel exact projection. FY26 work (Mar/May 2026). Has test scripts, build
instructions, known bugs, and TODO items. Related logs in same directory
(isis_mapproject_log0.sh, isis_mapproject_log1.sh).

## Key Shell Files in ~/projects/ (Build & Test Reference)

| File | Purpose |
|------|---------|
| `isis_mapproject/isis_mapproject_notes.sh` | **Latest active work** - ISIS/ASP map projection parity (see above) |
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

All plan and work notes in `~/projects/projwin_fix.sh` (parts 1-14).
- 7 tests: 5 must not change on l1, 2 need gold regenerated
- dem_mosaic changes reverted (not shipped)
- All 7 pass on Mac with dev build

## Float Tolerance in Test Validation

Use `max_err.pl --relative --max-err 1e-6` for acceptable floating point diffs.
Refactoring often produces small float noise from evaluation order changes.
