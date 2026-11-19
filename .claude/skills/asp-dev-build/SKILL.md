---
name: asp-dev-build
description: >-
  The LOCAL DEV build of ASP and VisionWorkbench (VW) on our boxes (Mac mini mac_arm,
  NASA MacBook neon, Linux l1) - the per-box env/compiler table, setting up a fresh box
  from the published asp_deps tarball (fetch, unpack, conda-unpack, codesign, libelas,
  first cmake of VW and ASP), the incremental make + make install after a source edit,
  where the binaries land, checking VW first, the benign warnings, and the PROJ_DATA
  trap for raw dev binaries. Load whenever you build/rebuild/install ASP or VW on l1 or
  a Mac, set up a new box for ASP dev, or verify a code change by running the rebuilt
  tool. NOT the nightly or release packaging (that is [[build-env]] /
  [[nightly-regression]]). Per-box detail: [[machines-tools]].
---

## When to load

Any time a task builds ASP or VW on l1 or one of our Macs for dev/test/debug: "build",
"make", "compile", "rebuild", "install", "set up ASP on this box", or "verify the
change by running the tool". This is the reflex to load BEFORE touching the build
dir, so the steps below are not re-derived each time.

## Per-box env location (Macs, and l1 for comparison)

The dev build is the SAME on all three boxes: env `asp_deps`, Unix Makefiles, VW
and ASP both installed into `~/projects/StereoPipeline/install`, and ASP pointed at
it with VISIONWORKBENCH_INSTALL_DIR. Both CMakeLists force `CMAKE_BUILD_TYPE=Release`,
so the cache value does not matter. Only the conda root, compiler and core count
differ (checked against each box's CMakeCache.txt, 2026-11-18):

| box | env | compiler (in `$E/bin`) | cores |
|---|---|---|---|
| mac_arm (Mac mini) | `~/anaconda3/envs/asp_deps` | `arm64-apple-darwin20.0.0-clang`/`clang++` | 10 |
| neon (nasa_mac) | `~/miniconda3/envs/asp_deps` (tarball, 2026-11-18) | same as mac_arm | 10 |
| l1 (Linux x86_64) | `~/miniconda3/envs/asp_deps` | `x86_64-conda-linux-gnu-cc`/`c++` | 16 |

Activate: `source $E/bin/activate` works everywhere (on mac_arm also
`source ~/anaconda3/etc/profile.d/conda.sh && conda activate asp_deps`). Below,
`$E` is that env. Shell state does not persist between tool calls, so activate in
every build command. On l1 the incremental build is
`make -C ~/projects/StereoPipeline/build -j16 && make -C ~/projects/StereoPipeline/build install`
(VW first if it changed, same rule as below). A fresh Linux box follows the same
tarball recipe with the Linux tag (`asp_deps_linux_v2` for x86_64, set in
`save_linux_deps.sh`, and `asp_deps_linux_arm_v1` for arm, in
`build_test_linux_arm.sh`), and Linux needs no codesign step.

## Fresh Mac: set up from the published deps tarball

This is the fast way (about 15 minutes total). Do not build the deps with conda.
Use the same tarball the cloud CI uses. The CI recipe this follows is
`StereoPipeline/.github/workflows/build_test.sh`, so if the tag or asset names
change, they change there first.

1. Fetch from the BinaryBuilder release `asp_deps_mac_arm64_v4` (public, no gh
   auth needed): `asp_deps_p1.tar.gz` (1.26 GB, there is no p2 for mac arm64 as of
   2026-11) and `python_isis10.tar.gz` (135 MB, needed only for make-dist packaging).
   List the assets with
   `curl -s https://api.github.com/repos/NeoGeographyToolkit/BinaryBuilder/releases/tags/asp_deps_mac_arm64_v4`.
   Download into the scratchpad and run `gzip -t` on each file.
2. Unpack: `mkdir -p $E && tar xzf asp_deps_p1.tar.gz -C $E`, then
   `$E/bin/conda-unpack`. Do the same for `python_isis10`.
3. Re-sign with ad-hoc codesign (`codesign --force -s -`) every `*.dylib`/`*.so`
   and every executable in `bin/`, in both envs. conda-unpack invalidates the
   signatures, and arm64 SIGKILLs unsigned binaries (exit 137).
4. Check that `lib/libopenblas.dylib`, `libnabo.dylib` and `libpointmatcher.dylib`
   exist. CI recreates them if conda-pack dropped them. They were present on neon.
5. Delete the two downloaded tarballs. The unpacked env is 4.4 GB, plus 0.4 GB for
   python_isis10.
6. libelas: the tarball has no `plugins/stereo/elas` (the mgm/msmw plugins are
   there). Clone `NeoGeographyToolkit/libelas` into `~/projects/libelas`, then in
   `build/` run
   `cmake .. -DTIFF_LIBRARY_RELEASE=$E/lib/libtiff.dylib -DTIFF_INCLUDE_DIR=$E/include -DCMAKE_CXX_FLAGS=-I$E/include`,
   then `make -j10`, then copy `elas` to `$E/plugins/stereo/elas/bin/elas`. It
   prints about 20 harmless printf-format warnings from its own source.
7. First configure of VW, then ASP (Unix Makefiles). These match mac_arm's cmake
   cache. Both install into the ONE prefix `~/projects/StereoPipeline/install`:

```
I=~/projects/StereoPipeline/install
C=$E/bin/arm64-apple-darwin20.0.0
# In ~/projects/visionworkbench/build:
cmake .. -DASP_DEPS_DIR=$E -DCMAKE_INSTALL_PREFIX=$I \
  -DCMAKE_C_COMPILER=$C-clang -DCMAKE_CXX_COMPILER=$C-clang++
make -j10 && make install
# Then in ~/projects/StereoPipeline/build, the same plus:
#   -DVISIONWORKBENCH_INSTALL_DIR=$I
```

   On the M5 neon, a VW plus ASP build from scratch took about 10 minutes and
   produced 96 tools. Verify with `$I/bin/stereo --version` (it prints the Build
   ID = the ASP git sha).

## The build (incremental, after a source edit)

ASP lives at `~/projects/StereoPipeline`, VW at `~/projects/visionworkbench`. Each
has a ready `build/` (Unix Makefiles, compiler = arm64 clang from asp_deps), and
BOTH install into `~/projects/StereoPipeline/install` (VW's
CMAKE_INSTALL_PREFIX points there, and ASP finds VW via
VISIONWORKBENCH_INSTALL_DIR). Cores: `sysctl -n hw.ncpu` (there is no `nproc` on
a Mac. Both Macs have 10).

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

VW is a dependency of ASP. ASP links against VW's INSTALLED libs. Before building
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
clean `make install` succeeded. (The fresh neon build from the tarball env
printed none of these.)

ASP compile warnings that are also expected and harmless (neon, 2026-11-18):
`variable length arrays in C++ are a Clang extension`, the Qt6 `'localPos' is
deprecated: Use position()`, a couple of int-max-to-float constant conversions,
and one deprecated `sprintf`. VW compiled with zero warnings.

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
AE=~/anaconda3/envs/asp_deps   # mac_arm, on neon ~/miniconda3/envs/asp_deps
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
