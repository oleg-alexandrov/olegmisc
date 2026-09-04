---
name: asp-packaging
description: How to add and ship a NEW command-line tool in Ames Stereo Pipeline - where the source goes, the CMake install list, the crucial .py / external-python wrapper rule in BinaryDist.py, the BinaryBuilder whitelist, wiring the docs (tools glob, NEWS, bibliography), and a StereoPipelineTest regression test. Load when adding a new ASP tool (C++ or Python), packaging or installing a tool, editing PYTHON_TOOLS or the whitelist, or when a python tool needs conda dependencies. Complements build-env (build/release mechanics) and nightly-regression.
---

Everything below is for adding a tool to ASP (repo `~/projects/StereoPipeline`,
package plumbing in `~/projects/BinaryBuilder`, tests in
`~/projects/StereoPipelineTest`). Worked end to end on `glint_correct`, 2026-09-03.

## Where the source goes

- New `.cc`/`.h` files: copy the license header from a NEIGHBORING file, and set
  the copyright year to the CURRENT year (`Copyright (c) 2009-<thisyear>`). The
  old boilerplate frozen at `2009-2013` is stale. New source in 2026 should read
  `2009-2026`. Grep-check before commit: `grep -m1 Copyright <newfile>`.
- User-facing tools: `src/asp/Tools/`. C++ is `<tool>.cc`; Python is `<tool>`
  (no extension) or `<tool>.py`. See the .py rule below - it is not cosmetic.
- Shared Python library modules (imported, not run): `src/asp/Python/`
  (`asp_*.py`), installed to `libexec`, listed in that dir's `CMakeLists.txt`
  `PYTHON_FILES`.

## Installing the tool (CMake)

`src/asp/Tools/CMakeLists.txt`:
- Python tool: add its basename to the `PYTHON_TOOLS` list. It is installed to
  `bin` with execute permissions.
- C++ tool: `add_executable(<tool> <tool>.cc)`, `target_link_libraries(...)`,
  `install(TARGETS <tool> DESTINATION bin)`, like `aster2asp`.

## The .py / external-python wrapper rule (the key gotcha)

At packaging time `BinaryDist.py` `add_executable` decides how each `bin/` entry
is shipped. Two classes, and the choice is load-bearing:

- Tools that run with ASP's OWN bundled python (`stereo`, `parallel_stereo`,
  `mapproject`, `dg_mosaic`, `sparse_disp`, etc.): shipped to `libexec` plus a
  relocatable shell WRAPPER in `bin`. These have NO `.py` extension.
- Tools that must run with the USER's external python, because they need packages
  not in ASP's python (gdal, scipy, numpy, matplotlib): copied straight to `bin`,
  NO wrapper. In `BinaryDist.py` these are the names ending in `.py`, PLUS any
  basename in the `external_python_tools` list in `add_executable`.

So a Python tool with conda dependencies must be wrapper-exempt, or it gets the
wrapper and fails at runtime on the missing modules. Two ways to be exempt:
1. Name it `<tool>.py` (like `bathy_threshold_calc.py`, `orbit_plot.py`).
2. Name it `<tool>` (no extension) and add it to `external_python_tools` in
   `BinaryDist.py`. This is how `glint_correct` ships without the ugly `.py`.
   Do NOT instead exempt by detecting a python shebang - `stereo` etc. also have
   a python shebang and MUST keep the wrapper, so shebang detection would break
   the core tools. Keep it an explicit opt-in list.

The tool's shebang should be `#!/usr/bin/env python`. In a dev `make install`
it then runs with whatever python is on PATH; the doc tells the user to point at
their conda env explicitly.

## BinaryBuilder whitelist

`~/projects/BinaryBuilder/whitelist` lists what is kept in the package. Add
`bin/<tool>` (with `.py` only if the tool keeps the extension). A tool missing
from the whitelist is silently dropped from the release.

## Wiring the docs

- Write `docs/tools/<tool>.rst`. It is auto-included via the `tools/*` glob in
  `docs/tools.rst` - no toctree edit needed. Match a sibling for style and
  terseness (`otsu_threshold.rst`, `bathy_threshold_calc.rst`, `orbit_plot.rst`).
  Load the docs-writing skill for `:ref:` vs `:numref:`, underline lengths, etc.
- NEWS: add one bullet under "New features:" in the FIRST section of the root
  `NEWS.rst`, a brief claim plus `(:numref:`<tool>`)`.
- Citations: BibTeX entry in `docs/bibliography.bib`, cite with `:cite:`key``.
- Build to verify: `~/anaconda3/envs/sphinx/bin/sphinx-build -b html . _build/html`
  from `docs/`, then grep the log for `WARNING|undefined label|citation not found`.

## Regression test (StereoPipelineTest)

`ss_<tool>/` with `run.sh` and `validate.sh`. Only those two text files are
committed. Test inputs and `gold/` are DATA and are NEVER git-added (ironclad).
l1 regolds; you seed a local gold to prove the plumbing.

- `run.sh`: `rm -rfv run; mkdir -p run`, then invoke the tool on tiny inputs in
  `../data/`. For an external-python tool, invoke it the way the bathy tests do:
  `~oalexan1/miniconda3/envs/bathy/bin/python $(which <tool>) ...` (that
  miniconda3 path is l1's; on a Mac the env is under anaconda3, so run.sh as
  committed runs on l1, which is where these tests live).
- `validate.sh`: `source ../bin/setup_env.sh`, then compare each `run/` output to
  `gold/`. For a raster, use `cmp_stats.sh $file $gold` plus a strict diff of the
  `gdalinfo -stats` lines (mirror `ssASTER_Exact_alignAffEpp/validate.sh`). For a
  scalar in stdout, parse it and compare with a relative tolerance (mirror
  `ss_bathy_threshold_calc/validate.sh`).
- Make tiny inputs by cropping real data with `gdal_translate -srcwin x y w h`
  (a few hundred pixels). Prefer a real crop over synthetic, and inspect that the
  output is sensible (e.g. the expected mean shift), not just that it ran.
- Metadata-only tools need NO image files. A camera builder like `cam_gen`
  (`--extrinsics` roll/pitch/yaw path, or `--vendor esri`) only READS the text
  metadata (exterior-orientation table, camera CSV / sample `.tsai`) and matches
  each row to an image by its file NAME. It never opens the image to write a
  `.tsai`. So stage only the small metadata in `../data/`, and let `run.sh`
  `printf` an `image-list` of faux frame names that need not exist on disk. The
  parsing is fully honest; the test stays tiny (a couple of KB) with no imagery.
  `ss_cam_gen_extrinsics` (old roll/pitch/yaw way) and `ss_cam_gen_vendor_esri`
  (`--vendor esri`, EO + ESRI camera CSV) are the two models. Validate by an exact
  `diff` of each `run/*.tsai` against `gold/`.
- Seed gold from a VERIFIED camera, not just a self-consistent one. Before
  committing `gold/`, confirm the produced camera matches an independently
  known-good result (e.g. `cam_test` reporting a tiny `dR` against the reference
  camera you trust), so the gold locks in the CORRECT answer, not merely a
  reproducible one.
- Tests are auto-discovered by the pytest `runDirs = ss*` wildcard, so a new
  `ss_<tool>/` dir needs no registration anywhere; the harness picks it up.
- To verify locally before a full build: copy the tool into the Mac ASP install
  `bin` (`~/projects/StereoPipeline/install/bin`), put that on PATH so
  `which <tool>` resolves, run with the Mac env's python, seed `gold/`, run
  `validate.sh` (needs `gdalinfo` and `cmp_stats.sh` on PATH).

## Conda env for a python tool

Do NOT bundle the deps into ASP's release env. Tell the user to make (or reuse) a
small conda env, as `bathy_threshold_calc.py` and `orbit_plot.py` do. If the deps
match an existing tool's env (e.g. gdal/numpy/scipy/matplotlib is the `bathy`
env), reuse it. The `bathy` env is about 750 MB. Document the one-liner
`conda create -n <env> -c conda-forge <packages>` in the tool's doc, and give the
run example as `<env>/bin/python $(which <tool>)`.
