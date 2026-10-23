---
name: mac-asp-build
description: Build and install ASP or VisionWorkbench (VW) on the Mac mini after a local source edit - the exact conda env, the incremental make + make install commands, where the binaries land, checking VW first, and the benign linker warnings. Load whenever you edit a .cc/.h in ~/projects/StereoPipeline or ~/projects/visionworkbench and need to compile/build/rebuild/install/make it on the Mac, or verify a code change by running the rebuilt tool. Focused Mac-only companion to the broader [[build-env]] (packaging, nightly, deps, ISIS) and [[machines-tools]] (per-box commands).
---

## When to load

Any time a task edits C++ in ASP or VW on the Mac and the change must take effect
in the installed tools: "build", "make", "compile", "rebuild", "install", "build
and install", or "verify the change by running the tool". This is the reflex to
load BEFORE touching the build dir, so the steps below are not re-derived each time.

## The build (Mac mini, arm64)

Env first, every time (shell state does not persist between tool calls):

```
source ~/anaconda3/etc/profile.d/conda.sh && conda activate asp_deps
```

ASP lives at `~/projects/StereoPipeline`, VW at `~/projects/visionworkbench`. Each
has a ready `build/` (Unix Makefiles, compiler = arm64 clang from asp_deps) and
installs into its own `install/`. Cores: `sysctl -n hw.ncpu` (there is no `nproc`
on Mac; it was 10 here).

Incremental build + install (after editing one or a few source files):

```
cd ~/projects/StereoPipeline/build && make -j10 && make install
```

- ALWAYS `make install` after `make`. Bare `make` leaves the installed libs and
  `install/bin` tools stale, so a run would use old code. This is the most common
  self-inflicted "my fix did nothing" bug.
- Editing a single .cc relinks only that object plus dependents, so incremental
  `make` is fast (seconds to a couple minutes), not a full rebuild.
- A comment-only edit does not change the binary, so a binary built before the
  comment edit is still valid to test. Rebuild only when logic changed.

Binaries land in `~/projects/StereoPipeline/install/bin` (VW: its own install).
Confirm you are running the fresh one:

```
~/projects/StereoPipeline/install/bin/<tool> --version   # prints a Build ID
```

## Check VW FIRST

For what the VW repo contains (its libraries, its command-line tools, and the
features that live in VW rather than ASP, such as lens distortion models and
convert_pinhole_model), see the [[visionworkbench]] skill.

VW is a dependency of ASP; ASP links against VW's INSTALLED libs. Before building
ASP, check VW for local changes:

```
cd ~/projects/visionworkbench && git status --short
```

If VW has local edits, build and install VW the same way (`cd
~/projects/visionworkbench/build && make -j10 && make install`) BEFORE ASP.
If VW is clean, only ASP needs rebuilding.

## Benign linker warnings (ignore)

`ld: warning: dylib (.../asp_deps/lib/libXXX.dylib) was built for newer macOS
version (16.0) than being linked (11.0)` prints many times during the link. It is
harmless (asp_deps dylibs target a newer SDK than the link deployment target) and
is NOT a build failure. A build that ends with `[100%] Built target ...` and a
clean `make install` succeeded.

## Running the raw dev binary: set PROJ_DATA/GDAL_DATA first (planetary datum trap)

The dev tools in `install/bin` have NO wrapper. The packaged/release build runs
through a `bin/` wrapper that exports PROJ_DATA, GDAL_DATA, PROJ_LIB BEFORE the
binary starts, so PROJ caches the right `proj.db` from the start. The raw dev
binary skips that wrapper and inherits the shell env. ASP's own
`set_asp_env_vars()` (called via `check_command_line`) sets these too, but it runs
during arg-parse, AFTER PROJ may have already cached a bad path, so it is NOT a
reliable fix for a wrong shell value.

Symptom when the shell PROJ_DATA is wrong/stale (e.g. a leftover
`~/miniconda3/.../share/proj` on a box that uses anaconda3): a tool creating a
PLANETARY datum CRS (Mars `+proj=longlat +R=3396190 +no_defs`, the Moon, etc.)
fails late with `PROJ: proj_create: Cannot find proj.db` surfacing as
`Failed to parse: +proj=longlat ...` (e.g. jitter_solve `--save-cnet-as-gcp`).
Earth `+datum=WGS84` or explicit `+a/+b` do NOT hit the db, so they mask it.

So when running a raw dev binary (and in any dev test-run helper), export the
correct paths up front:
```
AE=~/anaconda3/envs/asp_deps
export PROJ_DATA=$AE/share/proj GDAL_DATA=$AE/share/gdal PROJ_LIB=$AE/share/proj
```
Confirm `$PROJ_DATA/proj.db` exists. This is a dev-run setup issue, not a product
bug: released/packaged ASP is fine via its wrapper.

## Heavy compute vs compiling

Compiling on the Mac is fine (it is a build box). The CLAUDE.md ban is on heavy
STEREO/bundle_adjust COMPUTE, not on `make`. Running a tool on tiny test data
(e.g. the sub16 regression inputs) is also light and fine. Send real stereo/jitter
runs to l1 or pfe. See [[machines-tools]].

## Validating a Mac change against StereoPipelineTest: baseline-regold FIRST

The Mac is not the nightly machine, so its StereoPipelineTest `gold/` is usually
STALE and fails as-is for reasons unrelated to your change. Before judging whether
your change broke a test, run the chosen tests on CURRENT code, shallow-check the
expected stale-gold failures, and REGOLD to a fresh baseline - THEN make your change
and compare against that. Full recipe (STEP 0b): the [[asp-regressions]] skill.
