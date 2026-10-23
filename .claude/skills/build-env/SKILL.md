---
name: build-env
description: Building and packaging ASP and its deps - nightly build/regression, asp_deps cloud tarballs, release packaging, the asp_deps conda build env, the Qt6 plugins symlink ban, honest non-editable installs, conda channel cleanup, running the regression tests, cmake/build mechanics, ISIS ninja builds, and reading build warnings. Load before building ASP/VW/ISIS, editing CMake, packaging a release, or running the test suite.
---

## ASP/VW C++ build - Mac AND l1 both work (do not assume C++ is l1-only)

`cd <repo>/build && make -jN && make install` builds ASP or VW against `asp_deps`,
on the Mac mini just as on l1. The Mac has a full working dev build at
`~/projects/StereoPipeline/build` (and `~/projects/visionworkbench/build`); its
`install/bin` holds the C++ tools you run. So compile and test C++ locally on
whichever box you are on. l1 is the box for regolding and heavy runs, not the only
one that compiles. Per-box build commands: [[machines-tools]].

## Build/install ale and usgscsm from source into asp_deps, NOT isis_dev

Source builds of ale and usgscsm (the coupled pair) install into the **asp_deps**
conda env. isis_dev is ONLY for ISIS work and carries the conda ale/usgscsm; do
not source-install into it. Recipe (from cassis_asp/ale_usgscsm_respin_notes.sh):
ale is `cd ~/projects/ale/build; cmake .. -DALE_BUILD_LOAD=ON -DCMAKE_INSTALL_PREFIX=$CONDA_PREFIX; make -j8 && make install`
then `cd ~/projects/ale; pip install . --no-deps` (use a REGULAR install, not
`pip install -e .` - the PEP 660 editable finder leaves `ale.__file__` None and
fails to resolve `ale.drivers` submodules; also copy the built
`build/lib.*/ale/_ale_c.*.so` into `ale/ale/` if `import ale` cannot find `_ale_c`).
usgscsm is `cmake .. -DUSGSCSM_EXTERNAL_DEPS=ON -Dale_DIR=$CONDA_PREFIX/lib/cmake/ale -DCMAKE_INSTALL_PREFIX=$CONDA_PREFIX; make -j8 && make install`.
NOTE this modifies asp_deps, the nightly env; do it deliberately and be ready to
reinstall the conda package to restore it.

GOTCHA (isd_generate local SpiceQL search): `isd_generate <cube>` with no `-k`/`-w`
uses `pyspiceql.searchForKernelsets`, which returns `("", None)` (empty) for EVERY
mission in our local isisdata setup (near, clementine1, cassini, juno, lro all
empty), so ale's `KernelSet(None)` throws and the driver is rejected. This is a
SpiceQL local-db-search setup limitation, NOT ale staleness (rebuilding ale from
source 1.3.0 did not change it) and NOT a per-mission `spiceql_mission` issue. The
working paths are `isd_generate -k <cube>` (furnish from the spiceinit'd cube) or
`-w` (web SpiceQL, when the server is up).

## Bumping the conda `ale` package: libale ABI vs the pre-built isis/usgscsm (CRITICAL)

The conda `ale` package ships **both** `libale` (a C++ lib) and the python `ale`.
The pre-built conda `isis` and `usgscsm` **link `libale`** and carry only a LOOSE
pin `ale =1.2.0=asp*`, so `conda` auto-selects the newest `ale` build number. If a
new `ale` build changes the `libale` **ABI** (e.g. PR #726 grew `ale::Orientations`
by adding `m_quatW/X/Y/Z` members), the un-rebuilt conda isis/usgscsm allocate the
OLD object size and the new constructor writes past it -> heap corruption / double
free on every ISIS camera load. This shipped as the ale `asp_8` regression (crashed
`spiceinit`/`campt`); the fix was `asp_9` = the `asp_7` C++ source (pre-#726 libale,
byte-identical, 292640 on osx-arm64) + only the PYTHON odtk fix on top.

RULE when respinning the conda `ale` package:
- If the change is python-only, build from a branch whose `libale` C++ is
  **unchanged** from the currently-shipped build (verify: `libale` size/`git diff
  --stat ... -- '*.h' '*.cpp'` EMPTY vs the build isis/usgscsm were compiled
  against - NOT vs some intermediate umbrella branch; the baseline is what USERS
  have). Then no isis/usgscsm rebuild is needed.
- If `libale` ABI must change, you MUST rebuild AND republish `isis` and `usgscsm`
  against the new `ale` in the SAME respin (bump their build numbers), or users get
  the ABI crash. A green nightly does NOT catch this - the cloud test subset barely
  loads ISIS cameras; test with a fresh `conda create stereo-pipeline=<v> ale=<new>`
  then `campt from=<cube>`.
- Prevent-recurrence (deferred as of 2026-09-30): give `ale` a `run_exports` exact
  build pin and tighten isis/usgscsm off the loose `=asp*`.
Full incident + the asp_9 recipe: ~/projects/isis_ale_rebuild_notes.sh.

## Nightly build and regression tests -> nightly-regression

The nightly build/test/release pipeline (l1 cron, localLinux + cloud children, the
`auto_build/` files launch_master.sh/build.sh/run_tests.sh/utils.sh, email + release) is
owned by the **nightly-regression** skill (it owns driving the nightly); judging failures
and regold by **asp-regressions**. Deep reference: `~/projects/nightly_regression.sh`. Mac
CI gold updates: `~/projects/update_cloud_tests.sh`.

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
- **VERIFY the `lib/csmplugins` usgscsm symlinks resolve before publishing a deps
  tarball, and ship NO `*.bak*` libs.** conda-pack snapshots symlinks verbatim, so a
  usgscsm rebuild/respin that renames the real lib but does not regenerate the soname
  links bakes DANGLING links into the release. The lib's DT_SONAME is
  `libusgscsm.so.1` (Mac `libusgscsm.1.dylib`), so a broken `libusgscsm.so.1` link
  makes the runtime loader fail to find usgscsm at all. Correct chain:
  `libusgscsm.so -> libusgscsm.so.1 -> libusgscsm.so.2.1.0` (both links must resolve
  to the real file). Check the PUBLISHED asset, not just the live env, with
  `curl -sL <url> | tar tzv | grep csmplugins/libusgscsm`. Bit us 2026-07-16: the same
  usgscsm respin that caused the editable-ale `.pth` (see below) left the linux-intel
  tarball (`asp_deps_linux_v2`) with both links pointing at an absent `libusgscsm.so.2.0.1`
  plus two stray `libusgscsm.so.2.1.0.bak_*` copies. The mac/linux-arm tarballs were
  packed before the rename and stayed correct. Fix in place via the extract/re-tar/
  `--clobber` mechanic above: recreate the two links, delete the `.bak_*` files, no
  version bump.
- **`conda-pack` ships the conda-meta-RECORDED package files, NOT dev-overlay files
  make-installed over the prefix.** This is the #1 deps-tarball trap and it bit us hard
  2026-10-22. If you `make install` a dev-built ale/usgscsm/geoid OVER the conda package
  without updating conda-meta, the live env's bin+lib are the new consistent dev set, but
  conda-meta still records the OLD package. conda-pack follows conda-meta (it must, to
  rewrite baked prefixes for relocation), so it pulls the OLD cached binary and pairs it
  with your respun lib -> a bin<->lib SYMBOL SKEW in the tarball that the live env never
  shows. Scott's crash: shipped `usgscsm_cam_test` needed `getUsgsCsmModelFromJson` but the
  shipped `libusgscsm.so` only exported `getUsgsCsmModelFromJsonState` -> "undefined symbol"
  + a SIGSEGV in `ale::Orientations` during `constructStateFromIsd`. DIAGNOSE with
  `nm -D -u bin/usgscsm_cam_test | grep getUsgsCsm` vs `nm -D lib/csmplugins/libusgscsm.so.*
  | grep ' T '` - any undefined-in-bin symbol absent from the lib's exports is the skew.
  The CSM json is NOT the problem.
- **FIX a skew by making conda-meta + cache + prefix agree, then conda-pack.** Reinstall a
  real matched conda package (e.g. `usgscsm 2.1.0 cassis_0` + `ale 1.2.0 cassis_0`, which
  are built together and self-consistent). If the package exists only as an EXTRACTED pkgs
  cache dir with no archive, rebuild an installable `.tar.bz2` straight from it:
  `tar cjf name-version-buildstring.tar.bz2 -C ~/miniconda3/pkgs/<extracted-dir> .` then
  `conda install -p <env> --freeze-installed ./a.tar.bz2 ./b.tar.bz2` (explicit local files
  need no channel/index; `--freeze-installed` guarantees only those packages change - verify
  with a conda-meta file-list diff before/after).
- **Do it in a CLONE, never the live nightly env.** `conda create --clone asp_deps -n
  asp_deps_ship`, reinstall the matched packages there, conda-pack the clone. The result is
  the IDENTICAL tarball to packing the live env (conda-pack follows conda-meta either way),
  but the nightly env is never mutated and there is no restore step. Bonus: a clone can only
  contain conda-meta-tracked files, so it is inherently free of manual `.bak_*` overlay
  cruft. ALWAYS back up the full live env first anyway (`tar cf env.tar asp_deps`, ~6.5 GB).
- **`libarcher.so.bak` is a REAL `llvm-openmp` package file, not a stale backup** - it is
  recorded in conda-meta. A blanket `find -name '*.bak*' -delete` over-strips it and leaves
  the live env missing a package file. Only reject `.bak` files that are NOT in any
  conda-meta `files` list (those are the manual `.bak_pre_respin`/`.bak_preGeoT` overlays).
- **PUBLISH by `--clobber`, NEVER delete-then-create.** `gh release upload <tag> -R <repo>
  --clobber asp_deps_p1.tar.gz [asp_deps_p2.tar.gz] python_isis10.tar.gz` (gh =
  `~/*conda3/envs/gh/bin/gh`). A `gh release delete` + `gh release create` that fails between
  the two steps leaves the release GONE (burned 2026-10-22: a no-`set -e` script fell through
  a failed conda-pack into the delete and wiped the published release; recovered from a
  backup). WARNING: the production `save_linux_deps.sh`/`save_linux_arm_deps.sh`/
  `save_mac_deps.sh` still use delete-then-create - convert them to `--clobber` (create once
  if the tag is absent). Back up the CURRENT published assets before any clobber. If a repack
  changes the part count, fix the asset set: a new 2-part split must ADD `asp_deps_p2.tar.gz`;
  a shrink back to one part must DELETE a stale `asp_deps_p2.tar.gz` (else `cat
  asp_deps_p*.tar.gz` appends garbage). Consumer side: `build_test.sh` fetches p1 (required)
  + p2 (optional) then `cat asp_deps_p*.tar.gz | tar xzf -`.
- **VERIFY by fetch-back, not just the local pack.** Download the PUBLISHED assets fresh,
  `cat asp_deps_p*.tar.gz | tar xzf -`, run `conda-unpack`, re-check the symbol skew, and
  load a real model (`usgscsm_cam_test --model <json>` under `LD_LIBRARY_PATH=ext/lib:
  ext/lib/csmplugins`; exit 0, no "symbol lookup error", no segfault). The matched Jul-13
  cassis_0 build loads fine but logs a benign `constructModelFromISD ... key 'geotransform'
  not found` ERROR before loading the model-state (exit still 0) - cosmetic, does NOT appear
  on ASP's normal model-state load path. Repro fixture: `~/projects/ls_csm_bug/`.
- **conda-pack non-interactively: use `-p <PREFIX>`, not `-n <name>`.** A non-login
  shell has no `conda` on PATH, so `conda pack -n asp_deps` fails with "Failed to
  determine path to environment". Use `~/miniconda3/bin/conda-pack -p
  ~/miniconda3/envs/asp_deps -o asp_deps.tar.gz ...`. Always run the packer under
  `set -euo pipefail` with the upload GATED on the pack and tests passing, so a pack
  failure can never fall through to the publish step.
- **PRE-PACK GATE: run the overlay auditor so a dev overlay never ships silently.**
  `StereoPipeline/.github/workflows/audit_env_overlays.py <env-prefix>` compares every
  tracked file's on-disk size to conda-meta's `size_in_bytes` (relocation null-pads
  binaries, so a size mismatch = a real content overlay conda-pack will mis-ship). It
  is multi-claimant aware (a path owned by two pkgs passes if it matches EITHER), skips
  `*.pyc`/`__pycache__` and `etc/conda/test-files`, exits 0 on a clean env and 1 with a
  list on an overlaid one. Wired into `save_linux_deps.sh` before conda-pack. Do NOT
  compare to the package cache file (it is the pre-relocation placeholder copy, differs
  by size on every relocated binary = thousands of false positives). The 2026-10 live
  linux-intel env had FOUR overlay families it flagged: a non-conda ISIS build (514
  binaries, from `~/projects/ISIS3`, over conda `isis-asp_4` - violates the isis_2026
  "NEVER install ISIS into asp_deps" rule), the dev ale + usgscsm (cassis), and a
  dev `libegm2008.so` (geoid). An overlay in a clean clone is a package-metadata quirk
  (e.g. mysql-connector-c records sizes that mismatch even a fresh install), not ours -
  diff live-minus-clone to separate the two.
- **The honest fix for any such overlay: make it a real conda package (its own build
  string), install via conda, re-audit.** Do NOT keep the make-install overlay and
  "patch the tarball after conda-pack" - that is the per-env hack that silently rots.
  For CaSSIS, our custom builds are the `=cassis_N` build-string series on
  nasa-ames-stereo-pipeline (ale/usgscsm/isis), never touching the stable `=asp`
  packages. Pin the exact closure: the cassis build string carries a platform suffix
  (`cassis_0_linux_64`), so a spec is `ale=1.2.0=cassis_0*`. BEWARE closure skew: the
  newer `usgscsm cassis_2` needs `libsqlite >=3.53.3` and will NOT install into an env
  pinned to 3.53.2 - pick the cassis build that matches your env's closure (cassis_0
  did on linux-intel) or rebuild the whole deps env for the newer one.
- **The linux-intel asp_deps tarball is a CONCATENATED OVERLAY: many paths appear
  TWICE** (e.g. a 2026-06-09 base set and a later 2026-09-26 set of `share/geoids/*`,
  `libmysqlclient*`, GL/X11 headers). `tar x` is last-wins, so extraction yields the
  LAST copy (here the June set) and the earlier/newer listed copy is shadowed dead
  weight. So a member-NAME diff of a repack vs the original shows thousands of bogus
  "differences" (dup paths collapsed to one, plus explicit dir entries) - do NOT panic:
  compare the EXTRACTED TREE, not tar member lists. Re-taring ONCE collapses each path
  to one copy (leaner tarball) - GOOD, but **GOTCHA: a naive extract keeps the LAST copy,
  which here is the OLDER June set, so the newer Sep geoids are SHADOWED and a naive
  repack would re-ship the STALE data.** When patching a tarball, WATCH for such
  duplicate/overlay sets and deliberately KEEP THE LATEST, WIPE THE STALE: the newer copy
  is listed FIRST, so `tar --occurrence=1 -x share/geoids/<f>` pulls the latest and
  overwrite the extracted (stale) one before re-taring. Verify sizes/dates after. If the
  repack fits under ~1.9 GB it becomes a single `asp_deps_p1.tar.gz` - then DELETE the
  stale `asp_deps_p2.tar.gz` release asset, else `cat asp_deps_p*.tar.gz` appends garbage.
  (2026-10: fixed the geoids to the latest set for the linux-intel tarball. Audited the
  other three the same day - mac_arm64, mac_x64, linux_arm are CLEAN: symlinks resolve and
  the geoids are a single copy already at the latest Sep set. The overlay/stale-set/dangling
  weirdo was unique to the linux-intel tarball, so only it needed a repack.)
- Watch storage: each tarball is ~1-2 GB; extract in a scratch dir, wipe it after,
  and never leave stray conda-pack scratch or half-extracted envs around.

## Linux tarball glibc floor (do not let it creep)

The x86_64-Linux tarball's glibc floor is the MAX `GLIBC_x.y` symbol over ALL bundled
ELFs. 3.6.0 was 2.17 (ran everywhere), 3.7.0/3.8.0 regressed to 2.34 and users on
old-glibc HPCs got `/lib64/libm.so.6: version GLIBC_2.29 not found` (Maud, Ben).
- Culprits are conda-forge deps (glib) and OUR own package builds (spiceql, isis, ale,
  usgscsm) compiled on a newer toolchain. ASP's own code is 2.17 (built against the
  conda sysroot 2.17). As of 2026-10, spiceql asp_4 and the cassis ale/usgscsm are all
  GLIBC_2.14 (fine); the live regression was a DEAD duplicate glib 2.72 FILE shipped
  beside the good 2.88 (the active soname is 2.88 = 2.17, so the loaded lib was fine).
- FIX: solve the packaging env with `CONDA_OVERRIDE_GLIBC=2.17` so conda-forge deps come
  2.17-floor and no 2.34 glib file is present. Build OUR conda packages against
  `sysroot_linux-64=2.17`, else they silently raise the floor AND may not declare it
  (conda then installs them on a 2.17 host and they crash at runtime = Maud's libm error).
- objdump max-GLIBC-symbol OVER-ESTIMATES the floor (a dead 2.34 file reads as a 2.34
  floor though nothing loads it; pfe shows 2.34 yet runs on glibc 2.28). The ONLY honest
  acceptance test is RUNNING the tarball in a real glibc-2.17 (centos7) container. Add
  that as a CI gate so the floor cannot creep silently again.
- **The classic stray: make-dist bundling a SYSTEM lib beside our conda one.** Symptom:
  the tarball has BOTH `libglib-2.0.so.0.8800.1` (conda 2.88, the soname target, used)
  AND a dead `libglib-2.0.so.0.7200.4` (the Ubuntu build host's /usr/lib glib 2.72,
  floor 2.34, nothing links it). Root cause (fixed 2026-10-23, BinaryBuilder make-dist.py
  commit 55bd7bd0): the `LIB_SHIP_PREFIX` force-ship step added each soname from its
  ldd-RESOLVED path, and ldd resolves the glib family to /usr/lib on the build host. Fix:
  force-ship from the first `SEARCHPATH` hit (the conda env precedes the system dirs),
  not the ldd path. To confirm a stray is dead before stripping: `readelf -d` every
  shipped ELF for a NEEDED on the exact versioned name (0 = nothing links it), then hide
  it and run real tools (cam_test, stereo_gui) - they should still run. Pin WHERE a stray
  came from with `find / -name '<lib>.so.<ver>'` (the hit under /usr is the culprit).
Detail: `~/projects/asp_glibc_floor_notes.sh`.

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
- **A FAILED NIGHTLY -> triage, regold, republish:** owned by the **asp-regressions**
  skill (judge acceptable-vs-real, the `ss*/validate_out.txt` stale-scratch trap, regold
  with `python2 bin/runs_to_golds.py <names>`, republish via `launch_master.sh resume`) and
  **nightly-regression** (drive/retrigger). One command that lives only here: rank failures
  fast with `bash bin/triage_fails.sh` in the suite. Deep reference (worked 2026-08-07
  example): `~/projects/nightly_regression.sh`; triage detail in
  `~/projects/asp_regression_tests.sh`.
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

## ISIS builds/tests -> see the isis-build skill

ISIS3 builds with **Ninja** (`ninja -j install` from the build dir; `make install`
errors; source root `ISIS3/isis`, not the repo root), in the `isis_dev` env, NEVER
`asp_deps`. The full recipe, cmake flags (the `-DBUILD_CORE_TESTS=OFF` etc. non-test
build), the run-ONE-gtest loop (`runISISTests`, not `runTests`), the two `ISISROOT`
values, the ninja-install deadlock + PRE_TEST fix, and the run-time GUI/gh gotchas are
all in **[[isis-build]]** (deep reference `~/projects/isis_2026/isis_2026_notes.sh`).

**Packaging safety (keep here): a coverage-instrumented ISIS must NEVER reach `asp_deps`
(CRITICAL).** Coverage instruments the WHOLE ISIS lib set (~142 libs: libisis ~676 MB plus
every mission/camera/projection plugin `.so`, all with baked `.gcda`), and any that get
bundled into the ASP nightly HANG every ASP tool for minutes at exit on pfe/Athena. Always
build `-DbuildCoverage=OFF`, and after ANY ISIS build verify no lib is dirty:
`cd $CONDA_PREFIX/lib; for f in lib*.so; do strings $f|grep -q '\.gcda' && echo DIRTY $f;
done` (prints nothing; libisis ~26 MB not ~676 MB). Incident + `GCOV_PREFIX=/tmp` workaround:
**[[isis-build]]**.
