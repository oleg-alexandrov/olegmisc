# Claude Code Memory - oalexan1 Home Dir Project

remind user in a day or two to go on with rescale logic plan for ww, so rescale for int images in vw. There should be some stuff later here as to what to do.

## Current Main Project

`~/projects/binary_csm/binary_csm_notes.sh` - Binary CSM state format using
msgpack for ISDs and model states. FY26 USGS task. PR usgscsm#501 **merged
2026-03-25**. USGSCSM side complete. Remaining: update ASP CsmModel.cc to
use populateModel()/getModelJson() binary API instead of string serialization.

## Recent Projects

`~/projects/isis_jp2/isis_jp2_notes.sh` - Remove Kakadu from ISIS, use GDAL for
JPEG2000 reading. FY26 USGS task.

`~/projects/isis_mapproject/isis_mapproject_notes.sh` - ISIS cam2map / ASP mapproject
parity project with Kelvin Rodriguez (USGS). ASP_MAP mode in cam2map for per-pixel
exact projection. PR #5988. Mostly complete - in review.

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

## VW default_rescale Migration

- [project_vw_rescale.md](project_vw_rescale.md) - DiskImageResource::default_rescale=true causes silent bugs when reading integer images as float. Report and migration plan in `~/projects/vw_rescale_default.sh`. Deferred to next session.

## Float Tolerance in Test Validation

Use `max_err.pl --relative --max-err 1e-6` for acceptable floating point diffs.
Refactoring often produces small float noise from evaluation order changes.

## Feedback

- [feedback_stop_asking_permission.md](feedback_stop_asking_permission.md) - Don't ask permission to edit CLAUDE.md, config files, etc. Just do it.

