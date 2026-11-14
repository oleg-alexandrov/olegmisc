---
name: pfe-asp-build
description: >-
  The ASP build on NASA Pleiades (pfe) - where it lives, the runtime env needed
  to RUN its tools (isis10asp vs isis10, ISISROOT/ISISDATA, the bin/ wrapper and
  LD_LIBRARY_PATH), and how to PATCH or UPDATE the dev build from l1 (build on l1,
  overlay lib/libexec/bin onto the pfe package, scp-over-rsync NFS gotcha, verify
  through the wrapper, stale --version date). Load before running an ASP tool on
  pfe, setting the pfe ASP env, or sending/updating a build to pfe. The pfe sibling
  of mac-asp-build; companion to pfe-nas (qsub, head-node limits, ssh/tunnel) and
  build-env (deps, packaging, nightly). Deep recipe: ~/projects/pleiades_notes.sh.
---

The ASP build on pfe - how to RUN it and how to UPDATE it. This is the pfe sibling
of [[mac-asp-build]]. For pfe COMPUTE mechanics (qsub, head-node 1-thread limit,
ssh/pfx tunnel, deletion safety) see [[pfe-nas]]; for deps, packaging, and the
nightly see [[build-env]]. Deep recipe + the NFS gotcha detail: `~/projects/pleiades_notes.sh`
section "Syncing dev build to pfe".

## #1 RULE - l1 -> pfe COPY IS ALWAYS FULL, NEVER SELECTIVE (read this first)

**When copying a dev build from l1 to pfe, copy ALL of it, every time, with NO exceptions:
ALL of VW and ALL of ASP. That means every `install/lib/libVw*.so` AND every
`install/lib/libAsp*.so` into `lib/`, AND every binary in l1 `install/bin/` into
`libexec/` (plus `install/bin/*.py` into `bin/`). NEVER copy only the library or only the
tool you just edited.** Libs and binaries must all come from ONE l1 build. A new VW lib
changes the C++ ABI, so ANY binary left over from an older build dies with
`symbol lookup error ... undefined symbol _ZN2vw...`. Burned 2026-11-13: a selective
overlay (new libs + only `bundle_adjust`/`jitter_solve`) silently broke `stereo_parse`,
`point2dem`, `mapproject` and every other pfe stereo tool for everyone. Back up the old
`lib/` and `libexec/` files with a tar first, then do the full copy, then verify (below).

## Where the build lives

The working ASP on pfe is a packaged release at `pfx:~/projects/BinaryBuilder/StereoPipeline/`
- wrappers in `bin/`, real ELF C++ binaries in `libexec/`, libs in `lib/`. `$HOME/projects/BinaryBuilder`
is a SYMLINK (`-> /vast_swbuild/swbuild/oalexan1/projects/BinaryBuilder`), so a bare
`find` for a tool will NOT descend it - the tools are real files under `libexec/`, reached
through that symlink. (The symlink-`..` trap that kills qsub exec paths lives in [[pfe-nas]].)

## Runtime env to RUN the pfe build

**Working ASP runtime env on pfe (the dev BinaryBuilder build).** A bare `export PATH=.../StereoPipeline/bin` is NOT enough - the binaries link ISIS/CSM and die with `libusgscsm.so.1: cannot open shared object` then `IsisPreferences not found`. There is **no `asp_deps` env on pfe anymore** (only `asp_deps_stale_nfs`); the live ISIS env is **`isis10asp`**. The env that works (source a file like `asp_env.sh`):
```
SP=$HOME/projects/BinaryBuilder/StereoPipeline
export ISISROOT=/swbuild/oalexan1/miniconda3/envs/isis10asp
export PATH=$SP/bin:$SP/libexec:$PATH
export LD_LIBRARY_PATH=$ISISROOT/lib:$SP/lib:$SP/lib/csmplugins:$LD_LIBRARY_PATH
export GDAL_DATA=$SP/share/gdal PROJ_DATA=$SP/share/proj PROJ_LIB=$SP/share/proj
```
**ISIS on pfe needs `ISISROOT` AND `ISISDATA` set MANUALLY every session - `conda activate isis10`/`isis10asp` does NOT set them in a non-interactive ssh (both come back EMPTY). This keeps getting rediscovered (2026-10-29): always `export` them explicitly.** For ISIS tools (`tgocassis2isis`, `spiceinit`, `isd_generate`) also set `export ISISDATA=/nobackupnfs1/oalexan1/projects/isis3data` and `ALESPICEROOT=$ISISDATA`. The ISIS data + SPICE kernel tree lives on **`/nobackupnfs1`**, NOT `/nobackupp19` or `/swbuild` - a blind search of those finds nothing and wrongly concludes "no kernels on pfe" (burned 2026-10-29). TGO/CaSSIS kernels ARE present: `$ISISDATA/tgo/kernels/{ck,fk,ik,lsk,mk,pck,sclk,spk}`. ALE auto-discovers the ESA `em16_cassis.tm` metakernel from `ALESPICEROOT`, so no per-orbit metakernel is needed. **For standalone ISIS CLI apps (`tgocassis2isis`, `editlab`, `spiceinit`) ACTIVATE the `isis10` env, NOT `isis10asp` - isis10asp is MISSING `libLinearMath-float64.so.3.25` (not in its `lib/`), so ISIS apps die with `error while loading shared libraries: libLinearMath-float64.so.3.25` (burned 2026-10-29). isis10asp is only for use as `ISISROOT` when running ASP tools; isis10 is the complete ISIS env for ISIS commands.**

Setting `LD_LIBRARY_PATH` here is belt-and-suspenders, NOT required for tools run THROUGH the `$SP/bin/` WRAPPER. Verified 2026-10-28 on pfe: with only `ASPROOT`/`ISISROOT`/`ISISDATA` exported and `LD_LIBRARY_PATH` UNSET, `bin/mapproject` (loads the CSM plugin), `bin/bundle_adjust`/`parallel_bundle_adjust`, `bin/sfs --query`, and `bin/dem_mosaic` all ran clean on a compute node - the `bin/` wrapper sets `LD_LIBRARY_PATH`/`CSM_PLUGIN_PATH` itself before exec'ing `libexec/<tool>` (see "VERIFY the patched tool through the bin/ WRAPPER" below). So `$ISISROOT/lib` on `LD_LIBRARY_PATH` matters ONLY if you call a bare `libexec/` ELF binary directly (bypassing the wrapper); a wrapped call does not need it. The old claim that `mapproject`/`stereo`/`point2dem` "need it" held for direct-libexec calls, not wrapped ones. This is why `SfsPipeline/bin/sfs_env.sh` deliberately does NOT set `LD_LIBRARY_PATH` and still works.

## Patch or update the dev build from l1

Patch the pfe package from l1: build the FULL current source on l1 (`make -C ~/projects/StereoPipeline/build
-j16 && make install`; rsync the Mac working-tree changes to l1 first so l1 = the
code under test - a changed header like BaBaseOptions.h triggers a wide recompile,
give it time), then overlay the dev build onto the pfe package:
`install/lib/` -> `lib/`, `install/bin/` -> `libexec/`, `install/bin/*.py` -> `bin/`.
- **ALWAYS overlay BOTH the VW and the ASP libs together - NO EXCEPTIONS, even for a
  one-file ASP-only edit.** l1's `install/lib/` holds both `libVw*.so` (11) and
  `libAsp*.so` (12), all built against l1's current VW, which drifts AHEAD of the pfe
  package's bundled VW. A fresh `libAspCore.so` routinely references a VW symbol the
  OLDER packaged `libVw*.so` does not export, so overlaying only the ASP libs makes the
  tool die at runtime with `symbol lookup error: .../libAspCore.so: undefined symbol`
  (burned 2026-11-13: l1's `libVwMath.so` had `vw::gesdd` (a LAPACK SVD wrapper), the
  pfe `libVwMath.so` did not -> jitter_solve exited 127). FIX/RULE: scp the FULL set
  `install/lib/libVw*.so` AND `install/lib/libAsp*.so` (~31 MB, 23 files) every time,
  plus the rebuilt `libexec/` binaries. Tar the pfe `libVw*.so libAsp*.so` to a backup
  first so you can revert. This is the same lesson as [[mac-asp-build]]'s "Check VW
  FIRST", applied to the pfe overlay: VW and ASP ship together or not at all.
- **And the converse: once the new libs land, overlay ALL of l1 `install/bin/` into
  `libexec/`, never only the tool you edited.** New `libVw*.so` change C++ ABI (a template
  signature), so every OLD `libexec/` binary that links them breaks with `symbol lookup
  error ... undefined symbol _ZN2vw...` (burned 2026-11-13: an overlay that refreshed the libs
  plus only `bundle_adjust`/`jitter_solve` left `stereo_parse`, `point2dem`, `mapproject` etc.
  broken, so every pfe stereo failed). Libs + all binaries from ONE l1 build, always. Back up
  the old binaries with a tar first. After the overlay, sweep `libexec/` for leftovers:
  `ldd -r <elf> | grep -c 'undefined symbol.*_ZN2vw'` on each ELF older than the overlay
  (with `LD_LIBRARY_PATH=$SP/lib:$ISISROOT/lib`); tools absent from l1's tree (e.g.
  `rpc_gen`, `tif_mosaic`) stay broken until rebuilt.
- **l1 CANNOT ssh/scp to pfe directly** (the l1->pfe connection times out). Route the
  overlay l1 -> Mac -> pfe: `rsync` the libs/binaries l1 -> a Mac staging dir, then `scp`
  Mac -> pfx into `lib/` and `libexec/`. (Mac reaches both l1 and pfx; l1 and pfe do not
  see each other.)
- **PREFER `scp` over `rsync` for this overlay (NFS gotcha).** rsync l1->pfe can
  SILENTLY fail `mkstemp ... Permission denied` on the package `lib/` and `libexec/`
  NFS dirs (documented in pleiades_notes.sh); scp does not use mkstemp and always
  works. rsync MAY succeed (it did 2026-10-17, landing fresh libs+binary), so if you
  do use rsync, you MUST verify (below) - do not trust its exit code alone.
- **VERIFY the patched tool through the `bin/` WRAPPER, never a bare `libexec/` call.**
  `bin/<tool>` sets LD_LIBRARY_PATH/CSM_PLUGIN_PATH then execs `libexec/<tool>`; calling
  `libexec/<tool>` directly finds no libs and errors (looks like "flag missing / tool
  broken"). Confirm with a behavior check, e.g. `bin/jitter_solve --help | grep <new-flag>`.
- **`--version` build DATE is STALE after an incremental make** - the version string is
  generated at cmake-configure time, not per compile, so a freshly rebuilt binary can
  still print an older "Build date". Confirm freshness by the binary's MTIME or a
  new-flag/behavior check, NOT the `--version` date.
- `libusgscsm` lives in `lib/csmplugins/`; sync it only if usgscsm changed (the dev
  `install/` may not even have a `lib/csmplugins/` - a harmless rsync/scp miss otherwise).
- **NEVER wipe `libexec/` and replace it only with the dev `install/bin/`** - the dev build
  has ~100 ASP tools while `libexec/` has ~148 (it also carries the ISIS tools). Overlay
  onto it, do not substitute it. Full recipe in `~/projects/pleiades_notes.sh`.
