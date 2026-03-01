# Claude Code Memory - oalexan1 Home Dir Project

## Git Push and Commit Rules (CRITICAL - READ EVERY SESSION)

**NEVER push without explicit permission.** Do not `git push` to any remote
(origin, god, or anything else) unless specifically told to push. Do not
commit unless told to commit. Do not bundle pushes or commits with other
operations. Wait for explicit instruction for each one.

This has been a recurring problem - context compaction loses the instruction
and the agent pushes or commits autonomously. **Stop and ask first.**

## Repository Layout Overview

There are several git repos in play:

1. **Home dir** (`~/.git`, remote: `olegmisc`) - dotfiles, `~/bin/` (tkdiff, scripts),
   and `.claude/` memory files.
2. **Projects repo** (`~/projects/.git`, remote: `oleg-alexandrov/projects`) - shell
   scripts at the base level (*.sh files). Many subdirs exist but most are their
   own repos - **NEVER add subdir contents to the projects repo**, especially if
   the subdir has its own `.git`. Only add base-level files.
3. **StereoPipeline (ASP)** (`~/projects/StereoPipeline/.git`) - the main product.
   Remotes: `origin` (fork), `god` (upstream NeoGeographyToolkit).
4. **VisionWorkbench (VW)** (`~/projects/visionworkbench/.git`) - image processing
   library ASP depends on. Remotes: `origin` (fork), `god` (upstream).
5. **BinaryBuilder** (`~/projects/BinaryBuilder/.git`) - build toolset and nightly
   CI. Remotes: `origin` (fork), `god` (upstream NeoGeographyToolkit).
6. **StereoPipelineTest** (`~/projects/StereoPipelineTest/.git`) - regression tests.
   Remote: `origin` (IS the org repo, NeoGeographyToolkit). **NEVER git add
   `run/` or `gold/` dirs** - large binary output, gitignored.

Other subdirs in `~/projects/` (ISIS3, PDAL, pcl, etc.) are third-party checkouts
or data dirs - not ours to commit to.

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
# Conda env for pytest
conda activate pytest
cd ~/projects/StereoPipelineTest
# Config file points to dev install dir
pytest -n 4 -q -s -r a --tb=no --config dev_mac_arm.conf
# Expected: ~234 passed, ~58 failed (known issues / missing data)
```

## Conda Channel Priority for asp_deps

Order matters: `nasa-ames-stereo-pipeline`, `usgs-astrogeology`, `conda-forge`, `defaults`

## ASP/VW Library Naming

- ASP libraries: `libAsp*.so` (e.g., libAspCore.so, libAspCamera.so)
- VW libraries: `libVw*.so` (e.g., libVwCore.so, libVwMath.so)
- `libasprintf` is GNU gettext, NOT ASP - don't wipe it when cleaning ASP artifacts

## Build Directory Safety (CRITICAL)

**NEVER confuse `build/` with `build_linux/` or `install/` with `install_linux/`.**

Both VW and ASP have two separate build/install trees:
- `build/` + `install/` = native Mac ARM64 build (the main dev build)
- `build_linux/` + `install_linux/` = cross-compiled Linux x86_64

When wiping or modifying build dirs, always double-check the suffix. Wiping
`build/` when you meant `build_linux/` destroys the native dev build. Same
for install dirs. Always use the exact directory name.

## Forward Declaration Style

```cpp
namespace vw { namespace cm {
  class Colormap;
}}
```

## Float Tolerance in Test Validation

Use `max_err.pl --relative --max-err 1e-6` for acceptable floating point diffs.
Refactoring often produces small float noise from evaluation order changes.
