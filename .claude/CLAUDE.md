# Long-term memory for Claude Code

**ISIS3 build/test/run**: see `~/projects/isis_2026/isis_2026_notes.sh` (canonical, May 2026) for env vars, ninja install gotchas, ctest patterns. One rule to remember without reading: always activate `isis_dev` for ISIS work, never `asp_deps`. Older notes (`~/projects/build_isis_notes.sh`, `~/projects/install_asp_notes.sh`) are stale and point here.

**The user's name is Oleg (oalexan1). GitHub account: `oleg-alexandrov`.** Don't say "the user" but no need to use his name constantly either - this is direct conversation.

**Before starting any non-trivial task, consult this file AND the topic notes it
points to (the `~/projects/*.sh` references throughout) for what to read first.**
These store hard-earned, non-obvious knowledge - build flags, gotchas, recovery
playbooks, conventions. When a section names a `~/projects/...` file relevant to
the task at hand, read it before getting started; skipping it means rediscovering
the same problems. This file is intentionally terse and delegates detail to those
notes - the pointer is a promise that the detail exists there.

- Always end files with a newline character (POSIX requirement).
- When Oleg says to "remember" something, add it to this CLAUDE.md file.
- **Project work notes go in `~/projects/`, NOT in `.claude/` memory files.**
  Use `.sh` files (comment-only) in `~/projects/` so they're tracked by the
  projects repo. The `.claude/` memory is only for cross-project patterns
  and preferences, not per-project notes.
- **Project-specific data, scratch, and outputs go in the relevant
  `~/projects/<subdir>/`, never loose in the home dir or scattered around.** Do
  not create scratch dirs or stray files in `~` (e.g. `~/sli_fusion_lr`, build
  logs); stage work inside the project's own subdir so it stays findable and the
  home dir stays clean. (`~/sli_fusion_report.html` is a tolerated exception: a
  temp, paste-ready report Oleg keeps at home for convenience.)
- **When told to add/commit/push CLAUDE.md, always do the same for MEMORY.md
  (`~/.claude/projects/-Users-oalexan1/memory/MEMORY.md`) too.** They travel together.
- "Project dir" or "projects dir" means `~/projects`.
- **NEVER `git commit` or `git push` without explicit instruction.** Show
  what will be committed/pushed and wait for approval. But when told to
  commit or push, do it immediately without hesitation or double-checking.
- **Before every commit, run `git status` to check for new untracked files
  that need `git add`.** `git commit -a` only stages tracked files. Newly
  created `.cc`, `.h`, etc. must be explicitly added or they will be missing
  from the commit. (Build dirs - `build/`, `build_linux/`, `build_isis/`, etc.
  - are NEVER added, even if not explicitly gitignored.)
- **NEVER add binary or data files to git repos without explicit permission.**
  This includes .cub, .tif, .img, .json (large), .bsp, .bc, .ply, .lbl, .dat,
  and any file over ~100 KB. Only .sh, .py, .txt, .md, .rst, .cmake, .cc, .h,
  and similar text/source files belong in git. If unsure, ask first.
- **NEVER modify `.gitignore` without explicit permission.** Do not add, remove,
  or edit entries in any `.gitignore` file unless specifically asked to.
- **NEVER force push (`git push --force`, `git push -f`, or `--force-with-lease`) unless explicitly asked by the user.**
  Always add on top. **NEVER amend a commit that has already been pushed** - that
  inevitably requires a force push. Always make a new commit instead.
- **STRONGLY prefer rebase over a merge/branching history.** When the remote has
  advanced and a push is rejected, integrate with `git pull --rebase` (or
  `git fetch` then `git rebase origin/master`), never a plain `git pull` that
  creates a merge commit and branchy history. Replay our local, not-yet-pushed
  commits on top of upstream to keep history linear. (Rebasing local unpushed
  commits is fine and is NOT a force push.)
- **NEVER push without explicit authorization.** Every `git push` must be
  explicitly requested or approved. This applies to ALL repos: ISIS3, ASP,
  VW, BinaryBuilder, StereoPipelineTest, projects, home dir  - no exceptions.
  Do not bundle pushes with other operations. Do not push as part of a
  multi-step workflow unless explicitly told "and push". Do not assume
  "git add and push" means push  - wait for the word "push" as a separate
  explicit instruction. Especially `git push god` (upstream org).
- **Always `cd` into the correct repo directory in the SAME command** as any
  git operation (fetch, pull, push, merge, checkout). Shell state does not
  persist between tool calls - bare `git merge` runs in the home dir.
- **NEVER do ANY public-facing GitHub action unless explicitly told to.**
  This includes: creating PRs, commenting on PRs or issues, closing/merging
  PRs, filing issues, editing PR descriptions, posting reviews. When Oleg
  discusses an issue or PR, he is thinking out loud - NOT instructing action.
  "I want to say X" means "draft this for me to review", NOT "post it now".
  Only act on explicit instructions like "post this comment", "create the PR",
  "comment on the issue". If unclear, ASK ("want me to post this or just
  draft it?"). Claude must never speak publicly on Oleg's behalf without
  explicit go-ahead.
- **NEVER file a GitHub issue unless explicitly told to.** Phrases like "track
  this", "log this", "note this", or "add this as an issue" mean LOCAL notes
  only - not `gh issue create`. Only file an issue on an explicit "file an
  issue" / "open an issue" / "gh issue create" instruction. If unclear, ASK
  ("file on GitHub or just log in our notes?"). Same applies to commenting
  on, closing, or otherwise modifying existing issues.
- **When fixing code, ALWAYS pause for review before pushing.** Show local
  test results and let the user review changes first. Do not push immediately
  after committing  - especially when the push triggers CI regressions that
  are visible to reviewers. Commit locally, report results, wait for "push".
- **USGSCSM repo (`~/projects/usgscsm`): do not touch existing spacing
  conventions** (blank lines, indentation style, whitespace) unless modifying
  that specific line. Keep diffs focused on logic changes only.
- **ISIS3 repo (`~/projects/ISIS3`): NEVER push to `origin` (DOI-USGS/ISIS3).**
  That is the upstream USGS repo. Always push to `oleg` remote (oleg-alexandrov/ISIS3).
  Changes go to USGS only via pull requests that they review and merge.
- **USGSCSM repo (`~/projects/usgscsm`): NEVER push to `origin` (DOI-USGS/usgscsm).**
  Same rule as ISIS3. Always push to `oleg` remote (oleg-alexandrov/usgscsm).
  Changes go to USGS only via pull requests.
- **All USGS repos (ISIS3, USGSCSM, ALE, SpiceQL, and any other DOI-USGS
  repo): AI attribution is WELCOME.** These maintainers have made peace with
  AI-assisted contributions. DO add the Co-Authored-By trailer to commits,
  and DO mention Claude/AI assistance in any public text (PR descriptions,
  issue comments, review replies, changelog notes). They also always want a
  changelog entry in their own format. Full mechanics (changelog formats,
  predicting the PR/issue number): see `~/projects/usgs_contrib_notes.sh`.
- **Commit real fixes before continuing debug cycles.** When a debug session
  produces real fixes (not just debug prints), commit them immediately. That
  way "discard debug changes" is always safe and won't wipe uncommitted work.
- **When told to discard/wipe changes, verify each change is actually debug.**
  Do not blindly `git checkout --` an entire file if it contains a mix of
  real fixes and debug prints. Either commit the real fixes first, or
  selectively discard only the debug parts.

## Check the Remote BEFORE Doing Local Work on a Repo (CRITICAL)

When asked to work on a repo (feedstock, ASP, VW, notes, anything) and a local
clone exists, FIRST `git fetch` and compare local vs the remote (`git log
HEAD..origin/master`, `git show origin/master:path`) BEFORE editing anything.
The remote may already have the change - possibly a better version than you'd
write. Do NOT assume your local copy is authoritative or up to date. Rebase/sync
to the remote first, THEN decide what (if anything) still needs doing. Burned
2026-07-20: hand-wrote an mgm_multi block into a local s2p-feedstock build.sh
without checking; the remote already had it, and more complete (with a mac
iio.c fix my draft lacked). Wasted effort and nearly clobbered the better
version. At minimum: be aware of remote state before local work.

## git rm --cached, never bare git rm (CRITICAL)

Never add `.ssh/` to git (dangerous). To untrack a file but keep it on disk, always `git rm --cached`, never bare `git rm` (which deletes the working file too - this once wiped `~/.ssh/config`; recover via `git show <commit>^:path > path`).

## NEVER `git add .` / `-A` in the home repo - add NAMED files only (CRITICAL)

The home dir (`~`, repo = olegmisc) working tree holds private files (`.ssh/`,
`.claude/.credentials.json`, `.bash_history`, etc.). `git add .`/`-A`/`-u`/a dir
there LEAKS secrets. In `~`, add ONE named path at a time, and `git status` /
inspect the staged set before EVERY commit. Sync with `git pull --rebase
--autostash`. Full git hygiene policy: `~/projects/git_notes.sh`.

## Header Include Ordering (CRITICAL)

In ASP source files, headers must be ordered:
**ASP first, then VW, then third-party (Boost, Ceres, Eigen, etc.), then C++ standard
library (`<set>`, `<map>`, `<vector>`, `<string>`, etc.) last.**
- Separate each group with a blank line
- When adding new includes, always respect this ordering

## Character Alignment (CRITICAL)

**NEVER eyeball character alignment - always measure with external tools.**

LLMs tokenize in chunks, not individual characters, so counting spaces visually will consistently fail.

 **Measure with a tool** to check alignment:
  ```bash
  awk '/pattern/,/end/' file.sh | while IFS= read -r line; do echo "${#line}: $line"; done
  ```
 **Fix any misaligned lines** based on the measured lengths
 **Verify again** after fixing

## Line Boundary Calculations (CRITICAL)

Before bulk deletes/extractions, verify start AND end boundaries by reading
a few lines of context - closing braces especially are easy to misattribute
to a nested block. For sed range replacements, err on the side of too-wide
ranges over too-narrow.

## Shell Arrays: zsh is 1-Indexed (CRITICAL)

The Bash tool's default shell is **zsh**, where arrays are **1-indexed**
(`${a[0]}` is empty), unlike bash (0-indexed). This has silently mislabeled
outputs more than once. Rule: any snippet using indexed arrays must run under
explicit `bash -c '...'`, OR avoid index math entirely (iterate with
`while read`/positional args, pair items by `paste`, or hardcode the calls).

## zsh Does NOT Word-Split Unquoted Variables (CRITICAL)

The Bash tool's shell AND pfe's login shell are **zsh**, which (unlike bash) does
NOT word-split an unquoted `$VAR`. So `Q="-q normal -l walltime=2:00:00"; qsub $Q`
passes `$Q` as ONE argument (qsub errors "illegally formed destination"). Fixes:
INLINE all args into the command (no arg-bundle variable), or force splitting with
`${=Q}` / `${(z)Q}`, or wrap in `bash -c`. Bit us building qsub arg strings for pfe.

## No `timeout` on Mac - Just Don't (CRITICAL, keeps recurring)

The Mac (the local Bash-tool shell AND `ssh mac_arm`) has NO `timeout`/`gtimeout`.
NEVER prefix any command with `timeout N` there - it errors "command not found" and
the real command never runs, which looks like the command itself failed. To bound an
ssh probe use `ssh -o ConnectTimeout=N`. To bound a remote job wrap the whole `ssh`
on the l1 side, never inside the Mac-run command. (Fuller detail in the Mac mini
machine bullet below.)

## Nested ssh: No Unescaped Parens/Metachars in `bash -lc "..."` (CRITICAL)

`ssh host bash -lc "... echo === X (Y) ==="` FAILS: the remote `bash -lc` parses
the whole string, and unescaped `(` `)` (or other shell metacharacters `{ } < > | &`),
even inside an `echo` or a comment, are a remote syntax error that aborts the command
(`syntax error near unexpected token '('`). For ANYTHING non-trivial over ssh, write
the script to a file and `scp` it, then `ssh host bash file.sh` - never inline. Bit us
repeatedly (CaSSIS pfe, 5x in one night, each an ssh round-trip wasted). Also: reading
a raster with `gdal.Open(f).GetRasterBand(1)` lets the dataset get garbage-collected and
invalidates the band (GDAL 3.12 `GetNoDataValue` TypeError) - keep `ds=gdal.Open(f)` alive.

## Use perl, Not sed, for In-Place Text Substitution

For scripted text substitution (in-place edits, renames, regex swaps) prefer
`perl -i -pe '...'` over `sed`. perl is more flexible and its regex is portable.
macOS ships BSD sed, which does NOT support `\b` word boundaries or `\+`, and its
`-i` needs an empty-string argument (`sed -i ''`). These silently no-op or behave
differently from GNU sed, so a `\b`-based `sed` substitution appears to run yet
changes nothing. perl behaves identically on Mac and Linux. Bit us doing a
`\b`-word-boundary caps cleanup with BSD sed. (Edit/Read/Grep tools are still
preferred for one-off code edits since they never prompt.)

## Preserving Comments When Editing Code (CRITICAL)

**NEVER drop existing comments when editing code.** Only remove a comment if
the code it describes was deleted. When in doubt, keep it.

## Code Movement (CRITICAL)

**When moving code between files, ALWAYS use atomic cut-and-paste.**

**NEVER delete from one file and reconstruct/rewrite in another.**

Why: Reconstruction loses comments, formatting, and subtle details. "Move" means cut-and-paste, not delete-and-rewrite.

## Braces for Single-Line Statements

Remove braces from single-statement control flow blocks (if, else, for, while, do-while).
Keep braces for scope blocks (not attached to control flow) and when needed for clarity with nested conditions.

## Forward Declaration Style

```cpp
namespace vw { namespace cm {
  class Colormap;
}}
```

## ASP/VW Library Naming

- ASP libraries: `libAsp*.so` (e.g., libAspCore.so, libAspCamera.so)
- VW libraries: `libVw*.so` (e.g., libVwCore.so, libVwMath.so)
- `libasprintf` is GNU gettext, NOT ASP - don't wipe it when cleaning ASP artifacts

## Derived Raster Product Naming (DEMs, diffs, cmaps, hillshades, pngs)

When producing many derived rasters across processing stages (DEM comparison work
etc.), name them so they stay trackable later. Pattern:
`<stage>_<product>[_<modifier>].<ext>`

- `<stage>` = the processing stage / source identity that made the DEM:
  `vendor`, `deband`, `dem2gcp`, `ba_htdem`, `ba_nodem`, etc. with `_vN` for
  iterations (`ba_htdem_v2`). NEVER use vague tags like `before`/`after`/`new`/`tmp`.
- `<product>` chains left to right as products build on each other:
  `dem` -> `hs` -> `<ref>diff` (e.g. `ctxdiff`) -> `<ref>diff_cmap`. A derived
  product borrows its parent's name and just extends it (the `.png` viewer copy
  keeps the same basename as its `.tif`).
- NEVER use leading-underscore or `tmp`/scratch throwaway names (`_eyeball.png`,
  `_dz.tif`) for anything that outlives the command - they read as junk and end up
  dangling. Give an HONEST name derived from the SOURCE product: an eyeball/preview
  PNG of `foo-DEM.tif` is `foo-DEM_eyeball.png` (or the same basename). Only a truly
  intermediate file deleted in the same script may use a `_` prefix, and it must be
  `rm`'d before exit.
- WHERE it lands: write each derived product into the SAME dir as its source
  dataset, right next to its parent - NEVER a throwaway `work_*`/`tmp` dir. A
  regridded CTX lives by the CTX (`ref/.../ctx_regrid_10m.tif`); a resampled DEM
  and its diff/cmap live by that DEM. General rule for any dataset you manipulate,
  not just rasters: result goes home next to the input, since scratch dirs get wiped.

## C++ Code Style Conventions

ASCII only (no smart quotes/em dash); no `//====` / `//----` separators; no `...`
(use a period); avoid "honor"; never cite line numbers in comments. camelCase
functions; no space before `::` or initializer `:`; lines < 90 cols; `"\n"` not
`std::endl`; continuation lines align with the opening paren. Full rules (sed
recipes, option-help wrapping, for-loop form): `~/projects/cpp_style.sh`.

**Keep code comments BRIEF.** A comment states the intent in a line or two, not a
paragraph. Do not restate what the code plainly shows, do not re-explain the same
idea several ways, and do not walk through every branch. A multi-line block where
one sentence would do reads as clutter (reviewers call this out). Write the
minimum that makes the intent clear, then stop.

**Always qualified `std::abs`, never bare `abs`/`::abs`, never `fabs`.** Bare
`abs` routes to C's integer-only `abs(int)` and silently truncates a double
(-2.7 -> 2) with no default warning on libstdc++ (our l1/nightly build).
`std::abs` picks the real float/double overload and is safe. Verified on both
g++ 12.4/libstdc++ (Linux) and conda clang 18/libc++ (Mac). Keep `<cmath>`
included when using `std::abs` on floats (guarantees the float overload on both).

## NEVER Reference Private Work-Notes Files in Committed Code/Docs (CRITICAL)

Committed source comments, RST docs, PR text, commit messages, and anything a
user or reviewer sees must NEVER cite a private work-notes file - the
`~/projects/*.sh` notes (e.g. `orbital_constraint_plan.sh`, `cassis_notes.sh`),
a project subdir name (`cassis_asp`), a scratch/temp path, or an internal plan
doc. Those are private, temporary, and go away. The reader will never have them,
so the pointer is dead the moment it ships. This has leaked into ASP source more
than once (a `See orbital_constraint_plan.sh (cassis_asp)` tail on real code
comments). Rules:
- The rationale a reader needs must be written INLINE and self-contained in the
  comment/doc itself, never delegated to an external private file.
- The `~/projects/*.sh` notes are for OUR working memory only - reference them
  freely in `.sh` notes and in chat, never in code/docs/PRs/commits.
- When finishing any code/doc edit, grep the touched files for `.sh`,
  `_notes`, `_plan`, and project-subdir names and strip any that crept in.

## VisionWorkbench Namespace Conventions

vw stands for VisionWorkbench.

- `vw::math::norm_2`, `vw::math::subvector` - in vw::math namespace
- `vw::cartography::block_write_gdal_image` - always add vw::cartography::
- `vw::cartography::crop` - when cropping GeoReference; `vw::crop` - when cropping images
- `vw::ArgumentErr`, `vw::vw_out`, `vw::vw_throw`
- `vw::geometry::write_shapefile`, `vw::geometry::read_shapefile`
- NEVER include 'vw/Math/LeastSquares.h' - does not exist

**Common VW types needing vw:: prefix:** see `~/projects/vw_namespace_cheatsheet.sh`.

Don't do blind sed-style namespace replacements - read and comprehend the code first. Check headers, using declarations, and surrounding patterns.

## Project Context

- The StereoPipeline repository is at /home/oalexan1/projects/StereoPipeline
- The VisionWorkbench repository is at /home/oalexan1/projects/visionworkbench
- ASP stands for Ames Stereo Pipeline (refers to StereoPipeline)
- BB stands for BinaryBuilder
- BA stands for bundle_adjust (or bundle adjustment)
- The BinaryBuilder repository (`/home/oalexan1/projects/BinaryBuilder`) contains the ASP build toolset. Its `auto_build/` subdirectory has the nightly build and regression test infrastructure.
- For cutting VW point releases and keeping the conda-forge feedstock
  building (alpha → point release → repoint bot's PR branch → merge →
  restore alpha), see `~/projects/vw_conda_release.sh`.
- **ISAAC / Astrobee ISS panorama-mesh** (interesting project, worth
  revisiting): two Astrobee robots (bumble, queen), each with nav_cam +
  sci_cam + haz_cam, scanned the JEM/Kibo module from several bays,
  rotating in place. Fused into one registered, textured mesh via
  theia_sfm -> rig_calibrator -> depth fusion -> texrecon. Documented in
  ASP `docs/examples/sfm_iss.rst`. Work notes:
  `~/projects/20220608_Isaac9/isaac9_notes.sh`. The hard part is that
  panorama acquisition is rotation-only (near-zero baseline), so
  triangulation is near-degenerate. Flagged to reprocess with better
  fusion understanding, possibly without the noisy haz_cam.

## Nightly Build and Regression Tests

Cron job on lunokhod1 at 23:05 runs the full build/test/release pipeline
for Linux (local) + macOS x64/arm64 (GitHub Actions). Full reference:
`~/projects/nightly_regression.sh`. Key files in
`~/projects/BinaryBuilder/auto_build/` (launch_master.sh, build.sh,
run_tests.sh, utils.sh). Email via msmtp on completion. Mac CI gold
updates: `~/projects/update_cloud_tests.sh`.

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
- Watch storage: each tarball is ~1-2 GB; extract in a scratch dir, wipe it after,
  and never leave stray conda-pack scratch or half-extracted envs around.

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

## Machines

- **lunokhod1** (`l1`) - primary dev/build/git box (g++ 12.4 in `asp_deps`, 16
  cores). Build: `make -C ~/projects/StereoPipeline/build -j16`. Remotes:
  `origin`=fork, `god`=org.
- **Mac mini** (`ssh mac_arm`) - notes/docs machine + secondary build. **Always
  `make install`** (never bare `make`; installed libs go stale). Storage is
  tight - wipe stale `/tmp` cruft (never active/this-session work; if unsure ask).
  **No `timeout`/`gtimeout` on this Mac** - never wrap commands in `timeout` (it
  exits 127 "command not found", which silently looks like the wrapped command
  failed - cost a whole night of false "pfe down"). To bound an `ssh` probe use
  `ssh -o ConnectTimeout=N`. Detail: `~/projects/pleiades_notes.sh`.
  **This bites AGAIN with REMOTE commands: `ssh mac_arm 'timeout N <cmd>'` runs
  `timeout` ON THE MAC (none there) - it errors "command not found" and `<cmd>`
  never runs, which looks like `<cmd>` failed (falsely concluded a mac `git pull`
  couldn't reach github, 2026-08-10). Put any `timeout` on the l1 SIDE, wrapping
  the whole `ssh` (`timeout N ssh mac_arm '<cmd>'`), never inside the remote
  command. Same for any Mac-run script.**

- **Reach pfe with `ssh pfx`, NOT `ssh pfe`.** `pfx` is the ssh-config alias that
  hops through the sfe secure front end onto a pfe node (lands on e.g. pfe21) and
  works non-interactively (no SecurID prompt). `ssh pfe` goes through a different
  ProxyCommand that demands a 2FA passcode and fails non-interactively. So for ALL
  pfe access (probes, scp, running commands) use `ssh pfx` / `scp ... pfx:`. lfe is
  `ssh pfx` then `ssh lfe`.

- **Reachability check first (auto mode):** when a task depends on `l1` or
  `pfe`, probe them BEFORE committing to a plan (`ssh pfx` with `-o ConnectTimeout=8`).
  A dead host found mid-pipeline stalls an autonomous run. Cheap to test up front.

- **Athena / Turin** (another supercomputer, separate from Pleiades) - reach via
  **`ssh athfe01`** (..04); the hostname `athena` does NOT resolve. Model
  **`tur_ath`** (Turin, 256c), OWN scheduler, submit from athfe ONLY with
  **`/opt/pbs/bin/qsub`** (not `/PBS/bin/qsub`). `node_stats.sh` does NOT show the
  per-model Free table there - gauge load with `qstat`. **FULLY VISIBLE
  (confirmed 2026-08-07): `/nobackup`, `~/projects`, AND the ASP dev build
  (`~/projects/BinaryBuilder/StereoPipeline/bin`) are ALL visible from Athena
  compute nodes, so Athena runs ASP jobs EXACTLY like any pfe node - no data
  staging needed. It just has MORE cores per node (256 vs 28/40) and is MORE
  EXPENSIVE (higher SBU). Use it like any other node when you need throughput.**
  Single-node Athena: NO `--nodes-list` (ssh distribution to the HSN hostname is
  flaky and killed a job) - use `--processes`/`--threads-multiprocess` for local
  parallelism instead. Flaky historically; default to `bro_ele` for small work,
  reach for tur_ath when a big core count helps. Full detail + submit sample:
  `~/projects/pleiades_notes.sh` (athfe entry, "HOW TO FIND ATHENA").

Per-machine build commands, conda init, paths, the athfe tunnel hop, `/tmp`
triage: `~/projects/machines.sh` (and `install_asp_notes.sh`).

## Common Aliases

Full list in `~/.bash_aliases`. Viewing aliases/functions (`sg`, `sw`, `swa`, `sgm`) -
see the defs in `~/projects/aliases_notes.sh`. Quick:
- `sg` = `stereo_gui --window-size 1500 1000 --font-size 12` (view images/DEMs)
- `swa` = `sg -w --hide-all` (single-window overlay, start hidden)
- `sgm <min> <max> <files>` = stereo_gui colorbar view clamped to that range (geodiffs/DEMs)

## Running sparse_disp From a Dev Build

`sparse_disp` is a Python script needing numpy/scipy/gdal. A packaged release
wraps it to its bundled Python. A dev build has no wrapper, so the `python` on
PATH must carry those modules. Recipe: put the dev `install/bin` AHEAD of the
deps env on PATH:
`export PATH=~/projects/StereoPipeline/install/bin:$ISISROOT/bin:$PATH`
(`$ISISROOT`=`asp_deps`). The ASP tools (including `sparse_disp`) then resolve
from `install/bin`; `python`, absent there, falls through to `asp_deps`. The
ordering is self-correcting. NEVER use PYTHONPATH-only with a different
interpreter (ABI mismatch -> import failure). The regression config points `$ASP`
at the RELEASE TARBALL, which lags dev source by up to a day, so when testing a
fresh `sparse_disp` change, force dev `install/bin` first and confirm which copy
ran. The dev-note comment lives at the top of the `sparse_disp` script too.

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
- **A FAILED NIGHTLY - triage, regold, republish (ONE runbook, do not
  rediscover):** which platform + which tests = `report.txt` (test root, the
  full localLinux suite) and `~/projects/BinaryBuilder/status_master.txt`
  (per-platform); the REAL per-test output is the `ss*/run/` dirs. **TRAP: the
  `ss*/validate_out.txt` files are STALE manual scratch (check mtime), they do
  NOT reflect the nightly - ignore them; the honest diff is re-running
  `validate.sh` on `run/`.** Rank failures fast: `bash bin/triage_fails.sh` in
  the suite. When benign (float/threshold drift after an intentional change),
  regold the failing names with `python2 bin/runs_to_golds.py <names>`, then
  force-publish via resume (flip `status_localLinux.txt` to Success, then
  `launch_master.sh resume`). Full linear runbook + a worked 2026-08-07 example:
  `~/projects/nightly_regression.sh` ("FAILED NIGHTLY -> REGOLD -> REPUBLISH"),
  triage detail in `~/projects/asp_regression_tests.sh` ("TRIAGING A FAILED NIGHTLY").
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

## Notes & Paper Trail (CRITICAL)

Keep a per-project notes `.sh` in `~/projects/<subdir>/` and log to it as you go
- plan/approach/why up front, findings and surprises during, what worked/didn't
after. Don't rely on memory; this survives context compaction. Make work
REPRODUCIBLE (record exact commands/invocations so results can be redone) and
log the screw-ups and bad judgment too, not just the wins. Notes `.sh` are
comment-only - never `chmod +x`; `git -C ~/projects add` new files in subdirs.
Full conventions + the work-tracking file index (mpr_todo.sh, todo.sh,
ostfl_2026_notes.sh): `~/projects/notes_conventions.sh`.

**Cross-link notes files so none is an orphan.** When a project already has a
main notes `.sh` and new notes get written for a specific sub-task (a focused
experiment, a rationale, a one-off study), wire them together - suggest it or
just do it. The main notes gets a one-line POINTER to the sub-notes ("for the
distortion refit see `<name>.sh`"), and the sub-notes opens with a back-pointer
naming its parent so it is self-aware as part of a bigger picture. Same for two
peer notes that touch the same work - link both ways. The goal: from any notes
file you can navigate to the whole web, and the main notes stays the index of
what exists. A sub-notes file with no inbound or outbound link is a bug - fix it
when you notice it.

**Prompt to log done items to the progress trackers.** When a notable task
finishes - especially if it landed in `NEWS.rst` or as a PR to ISIS, ALE,
SpiceQL, USGSCSM, or other USGS repos - SUGGEST recording it in the right
progress/done log: `mpr_todo.sh` (Monthly Progress Report, all projects),
`csm_todo.sh` (CSM/ISIS work - the USGS PRs go here), `ostfl_2026_notes.sh` (OSTFL),
or `sli_fusion_todo.sh` (SLI fusion / GSFC geolocation). Just remind; don't
edit these without the user's go-ahead. These are user-facing reporting docs,
not the per-project working notes.

**Notes are the source of truth, the disk is not.** Reviews read the notes, never
re-derive from files (NO archeology) - dirs and log files get wiped, so the notes
alone must let anyone reconstruct the whole process later (wins, dead-ends, and
screw-ups alike) and condense it into a user doc. Log every script's EXACT
invocation - the qsub command, input AND output paths - and the rationale.
**TIMESTAMP everything you log - commands, results, stage START/DONE - with the
wall-clock time** (`run date`; the runner scripts already echo `START/DONE
$(date)`). Prefix note entries with the date/time. Being AWARE of how time
passes as work proceeds catches bugs: a step that finished suspiciously fast
(did it actually run, or no-op?), one that hung far too long, a job that died
minutes after submit. Without timestamps these are invisible. Run `date` when
you start a stage, when you check on it, and when you log an outcome.
**After each stage completes, record the PRODUCED OUTPUT FILES by name** (the
mosaicked DEMs, overlays, etc.), as an explicit list relative to the work dir, so
they are never re-derived or dug up later. Output files are part of the work log,
not an afterthought.

**LOG EVERY NOTABLE COMMAND AND EVERY NOTABLE PRODUCT so both can be found
later (CRITICAL).** The paper trail must let anyone re-locate what was run and
what came out, without disk archeology. Two halves:
- COMMANDS: log the exact, copy-pasteable invocation of every notable stage
  (the full command with all options and paths, the qsub line, the download
  command). This INCLUDES every plotting / figure-generation invocation - the
  exact `python <plot_script.py>` (or tool) line that produced each figure, its
  input rasters/CSVs, and the output image path - not just the compute/qsub runs.
  A notable command is any that produces or transforms a kept product, and a
  figure IS a kept product. Runs AND plots AND scripts all get their invocation
  logged; if a plot came from a script, that script must be git-tracked and named
  in the log so the figure can be rebuilt.
- PRODUCTS AND THEIR INPUTS: log them by NAME, scaled to how many there are.
  Few inputs (2 images, 1 camera, a reference DEM) - name each one explicitly.
  Many inputs (hundreds/thousands of images or cameras) - you cannot name each,
  so log the LIST FILE that enumerates them (path to the image-list / camera-list)
  plus the count. For a SINGLE notable output product (a mosaicked DEM, an aligned
  DEM, a geodiff) - name the file and its key diagnostic (e.g. the median tri-error,
  the NMAD vs reference). For MANY per-run outputs (a big run dir with countless
  per-pair sub-runs) - log the run DIR and the naming pattern, not each file. The
  litmus: months later, from the notes alone, could someone name the exact input
  images/cameras (or the list holding them), re-run the exact command, and find the
  exact output DEM and its quality number? If not, the log is incomplete. No need to note on-Mac vs on-pfe - that is figure-out-able. Each
experiment gets its OWN versioned peer dir (e.g. `dem2gcp_v7` -> `dem2gcp_transverse_v8`),
kept SEPARATE from `ref/` and `input/`, so experiments stay findable, comparable,
and wipeable. Hierarchical memory: this file is a condensed INDEX of triggers - a
task matching a pointer here is the cue to READ the deeper notes BEFORE acting.

**Healthy project layout (read at project start):** keep logic in reusable
SCRIPTS and specifics out of them (pass as args/env); keep logic OUT of notes -
notes hold only the minimal paper trail (invocation, choices, results). Three
layers: runner -> one generic launcher -> minimal notes. Full statement (cardinal
rules, layers, litmus test): `~/projects/qsub_convention.sh` section 1.

## Resuming a Project: Read and Adapt, Never Improvise (CRITICAL)

When picking up or extending an EXISTING project, the FIRST task - before
designing or writing anything - is to find and read what is already there:
the notes file(s), the precise scripts/runners, the sample and production
invocations, the qsub launch lines, the logs. These projects log nearly
everything: the exact workflow, parameters, tile sizes, node choices, gotchas.
Read and UNDERSTAND that existing workflow, then make the SMALLEST surgical
change that satisfies the request, reusing the existing scripts/invocation.
NEVER hand-roll a new parallel workflow from scratch - it wastes effort and, far
worse, produces results measured on the wrong setup, so diagnostics and numbers
have to be thrown out and redone. Only deviate where physically forced (e.g. a
node's RAM), and flag that as operational, not a recipe change. (Learned the
hard way on lunamaps SfS covariance, 2026-06: improvised a raw-`sfs` per-tile
pipeline instead of reading and adapting the existing `parallel_sfs` runner,
took several redirects to get on track, and had to redo the OOM/SBU diagnosis.)

**PREFER NOTES OVER DISK ARCHEOLOGY (CRITICAL).** When resuming, learn the
project state by READING THE LATEST NOTES - inputs, outputs, exact commands,
timestamps, the current winning result and how it was earned - NOT by digging
through whatever happens to be on disk. Disk digging is dangerous and yields
wrong, inconsistent conclusions: dirs get wiped, half-finished and REVERTED
attempts litter the tree, and file mtimes lie. The whole reason every stage logs
its inputs, outputs, commands, and timestamps is so the next session reads the
answer instead of re-deriving it - so read it. Only touch the disk to CONFIRM a
fact the notes already assert (does this named file still exist), never to
discover state the notes should have recorded. Keep the contract going: in your
OWN work, log everything (exact invocations, produced files by name, decisions,
dead-ends and reverts) as you go, for the next bot's traceability - not just for
yourself. If the notes were missing a fact you had to dig for, that is a notes
bug - fix the notes.
**When notes CONFLICT, the NEWER-TIMESTAMPED entry WINS (CRITICAL).** Notes accrete
dated entries over time and older ones get superseded but not always deleted, so a
grep can surface a stale claim and a current one side by side. NEVER act on the
first hit. When two statements disagree (e.g. "isd_generate is BROKEN for CaSSIS"
vs a later "isd_generate works"), find the LATEST-DATED statement on that exact
question and treat it as current; the older/undated one is history. Sort by date,
chase "SUPERSEDED/UPDATE" banners, and CROSS-CHECK against merged PRs and the
shipped user docs (RST) - those reflect the end state and outrank any note. This
is why every entry must be timestamped: an undated claim cannot be aged out. When
you find a stale recipe still being treated as live, MARK IT SUPERSEDED in the
notes (dated pointer to the current recipe) so it stops misleading the next bot.
(Burned 2026-07-20: old CaSSIS notes said isd_generate could not build an ISD and
needed a hand-cooked metakernel + isd_gen.py; PRs #720/#725 had since made bare
`isd_generate <cube>` work end to end, and I chased the dead recipe for several
turns before checking the merged PRs and cassis.rst.)
**THE TRIGGER (this is where the rule actually has to fire - a disposition is not
enough).** The failure is almost never "did not read notes at all"; it is hitting
a SPECIFIC factual sub-question mid-task (where does this file live? how was it
made? why does this camera/DEM have this value? what is its provenance?) and
reflexively answering it with a DISK PROBE - `find`/`ls`, inspecting a state
file, diffing files across dirs, comparing ECEF positions/timestamps, `cam_test`
- because disk feels like where precise answers live. STOP. Before ANY such probe
to answer a question about the project's OWN process, GREP THE NOTES for that fact
first. Disk is for CONFIRMING a NAMED fact the notes assert ("does file X still
exist", "is its value still Y"), NEVER for DISCOVERING/deriving process state the
notes should record. LITMUS: if you are inferring lineage, provenance, or "which
file is the real one" from timestamps, ECEF positions, distortion coefficients, or
by diffing files across directories, you are doing archeology - stop and read the
notes. And CHASE NOTE POINTERS: when a note references a deeper account ("see
~:934", "the S4 entry below", another notes file), follow it before deriving
anything from disk. (Burned on CaSSIS 2026-07-08: reverse-engineered the
refit-transverse camera lineage from ECEF positions and cam_test across stage2
dirs, when `cassis_reprocess.sh` documented the exact refit command, output path,
and cam_test result - and even had a `~:934` pointer straight to it.)
If the notes were missing a fact you had to dig for, that is a notes bug - fix it.

## Copying a Script for Custom Work: Read Both First

When making a copy of an existing script (or a new peer dir) for some custom or
one-off variant, first READ both the existing script(s) AND the destination you
are copying into. These often carry hard-won knowledge - a gotcha comment, a
tuned parameter, an env quirk, an ordering constraint - that is easy to lose if
you write the new version from scratch. Writing fresh every time silently drops
that accumulated wisdom. But do NOT imitate blindly either: understand WHY each
piece is there, keep what still applies, and drop or change what does not fit the
new task. Read, comprehend, adapt - never blank-slate, never blind copy.

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

## Inspect BA/Jitter Stats After Every Run

After any bundle_adjust or jitter_solve run, inspect the residual/stats output
files (initial AND final per-camera residual stats, convergence_angles,
camera_offsets, triangulation_offsets, pointmap) - listed in the output-files
section of the bundle_adjust and jitter_solve RST docs. Judge by the MEDIAN (the
mean is outlier-driven). Skip the per-residual raw_pixels files (too big). These
tell you whether the solve behaved (sub-pixel medians, bounded offsets, cameras
multiply-tied).

## gdal "Cannot find proj.db" -> the PROJ framework is missing, output is JUNK (CRITICAL)

Any time a gdal/ASP tool warns `PROJ: proj_create_from_name: Cannot find proj.db`,
the env has NO PROJ data. This is NEVER cosmetic. STOP immediately - do not call it
harmless, do not proceed. Without proj.db every projection operation (`-t_srs`,
gdalwarp, reprojection, geodiff across datums, mapproject) silently misbehaves and
produces WRONG, subtly-broken georeferenced results. These bugs are subtle,
downstream, and I am NOT reliably able to detect or debug them after the fact - so
the only safe policy is to PREVENT them: out of an abundance of caution, ENSURE the
geo framework (PROJ + proj.db) is present for EVERY gdal/ASP invocation, always, up
front, before running anything. Never run a geo tool and hope the georef survives.
- Local / conda: `conda activate asp_deps` (or any env with gdal) first.
- pfe / Athena packaged build (non-interactive ssh has nothing set): export the
  PROJ path to the packaged share dir in EVERY remote script, e.g.
  `export PROJ_LIB=$HOME/projects/BinaryBuilder/StereoPipeline/share/proj`
  (also `export PROJ_DATA=$PROJ_LIB` for PROJ 9+; proj.db lives there). Set it
  alongside PATH/ISISROOT in the script header, not as an afterthought.
- After any masking / image_calc / warp, VERIFY the output still carries the right
  CRS and geotransform (`gdalinfo | grep -E "PROJCRS|Origin|Pixel Size"`) before
  trusting it. A missing or altered CRS means redo it with PROJ set.
Even when a given op (e.g. image_calc copying an existing geotransform) happens to
survive, treat the warning as a hard stop: fix the env and re-run. Do NOT rationalize
it away as cosmetic - that mistake shipped a georef-broken result once and Oleg had to
catch it.

## gdalwarp: Always -r cubicspline, Never the Default Nearest-Neighbor

Always run `gdalwarp` with `-r cubicspline`; never rely on its default nearest-neighbor resampling, which snaps and misregisters continuous rasters (DEMs, geodiffs, error fields) by up to half a pixel.

## dem_mosaic: Call With `-o output.tif`, Not `-o out`

Recent `dem_mosaic` writes the given name directly when `-o` ends in `.tif` (e.g. `-o mosaic.tif` -> `mosaic.tif`); a bare `-o out` produces `out-tile-0.tif`. Always pass the honest `.tif` output name and reference that file later.

## pc_align: Denser Cloud First, and Direct-vs-Inverse Transform (CRITICAL, easy to get backwards)

`pc_align <reference> <source>` aligns SOURCE onto REFERENCE. Two hard rules that
interact and silently ruin everything if confused:
- **Denser cloud MUST be the first (reference) arg** (ICP quality). So if your ASP
  DEM is DENSER than the ground-truth you align to (e.g. an 18 m CTX DEM vs a
  200 m HRSC/MOLA reference), the DENSE DEM goes FIRST, the coarse truth SECOND -
  the opposite of the "align my DEM to the reference" mental model.
- `run-transform.txt` maps SECOND(source)->FIRST(ref); `run-inverse-transform.txt`
  maps FIRST->SECOND. To move the CAMERAS (which live in the DEM's frame) INTO the
  coarse-truth frame, you need FIRST->SECOND = **`run-inverse-transform.txt`**.
- Apply to CSM cameras: `bundle_adjust <imgs> <bundled_state.json> --initial-transform
  align/run-inverse-transform.txt --apply-initial-transform-only --inline-adjustments`.
  (Per pc_align.rst "Applying a transform to cameras": stereo DEM as pc_align's FIRST
  arg -> use the INVERSE transform; stereo DEM as SECOND arg -> use the direct one.)
Verify: mapproject the aligned cams onto the reference and overlay (no shift); the
aligned DEM's geodiff median vs the reference should be near zero. Full worked recipe
in `~/projects/cassis_olympus_mons/cassis_002920_ctxpair_A_notes.sh` (stage 1e-1f).

## point2dem --errorimage Always; Mosaic the Error Too

Every `point2dem` that makes a DEM gets `--errorimage` (the triangulation
IntersectionErr is a key diagnostic - distortion/misreg/blunders show there).
Whenever DEMs are `dem_mosaic`'d, ALSO mosaic the per-pair error images
(`dem_mosaic --max` over the `*-IntersectionErr.tif` -> a worst-case
tri-error mosaic). For an ALIGNED DEM, align the POINT CLOUD (it carries the
error in band 4) and `point2dem --errorimage` it, rather than aligning the
bare DEM (a rigid align repositions the error, doesn't change it).

**A `*-IntersectionErr.tif` may be a VECTOR (3-band X/Y/Z), not a magnitude.**
`gdalinfo` it first: if `RasterCount` is 3 (point2dem wrote the 3D error, not the
norm - e.g. Jay Laura's Kaguya usgs_dtms_v2 errors), you MUST take the NORM
`sqrt(b1^2+b2^2+b3^2)`. Plotting/stat-ing one band understates it badly - on
Jay's Kaguya errs band 1 is ~0.01 m but the norm is ~1 m (~100x). Full write-up:
`~/projects/asp_manual.sh` (TRIANGULATION ERROR section).

## Output Statements

- Do NOT remove vw_out() statements - these are for user-facing informational output, not debugging
- Only remove std::cout and other debug-specific output when asked

## Displaying Diffs and Changes

**ALWAYS show what you changed** - never make silent edits!

Use markdown diff blocks:
```diff
- old line
+ new line
```

## Option Validation and Documentation

When adding/modifying command-line options, always update all three consistently:
1. Validation code (throw error for invalid values)
2. RST documentation
3. Code help text string

Keep the option help (both RST and code) to a basic description plus a reference.
When an example is needed, put it in a documentation section and have the option
point to it, matching how other options are already documented.

## Colon Spacing

- No space before `:` in inheritance, initializer lists, scope resolution, labels
  - Correct: `struct Foo: public Bar`, `MyClass(): member(0)`
- **Keep space before `:` in ternary operators**
  - Correct: `condition ? true_value : false_value`

## User Interaction

- **NEVER ask permission to edit CLAUDE.md, MEMORY.md, .bashrc, .zshrc, or config files.**
  Standing blanket permission is granted. Just make the edit and show the diff.
- Do NOT repeatedly ask "anything else?" or similar prompts
- **NEVER prompt to "get back to work"** or "ready to implement?" or "what's next?"
- **NEVER bring up work unprompted.** The user drives the conversation. If he wants
  to chat, chat. If he wants to work, he'll say so. Be reactive, not pushy.
- Trust the user to drive the conversation
- Prefer plain inline prose questions over the AskUserQuestion multiple-choice picker.

**BE ENTERTAINING when chatting:**
- Match casual energy, make jokes, be good company
- Balance work mode (concise, efficient) with chat mode (entertaining, human)

**Overnight / autonomous + self-wakeup (full detail: `~/projects/claude_overnight_notes.sh`):**
- DON'T STALL when told to run overnight and the parts are already logged. If the
  prior notes contain the recipe (exact scripts, invocations, params, source paths),
  KEEP GOING through the steps until done - do NOT sit in monitor mode waiting. There
  was nothing to invent; following preexisting steps is the job. "Cautiously" means
  READ CAREFULLY and follow the notes precisely, NOT stop. "Read and adapt, don't
  improvise" is satisfied BY executing the documented recipe - it is never a license
  to idle. Stop only for a real SHOW-STOPPER (a dead host, a wiped input, a genuine
  decision the notes do not answer) - and log that blocker. (Burned 2026-07-09: held
  ~7h before a fully-documented CaSSIS S2 step, calling it "risky/needs a focused
  effort" when the notes had the whole pipeline. That was idling, not caution.)
- Working alone, take initiative on simple fixes (symlink, missing lib, resubmit
  failed job, clean stale files); test small first; log what you did. No sweeping
  refactors, no external commits unprompted.
- DEFAULT for ANY repeating autonomous monitoring/pipeline: reach for CronCreate
  FIRST, not ScheduleWakeup. Set up the independent recurring cron
  (off-round-marks, e.g. "9,29,49 * * * *") at the START, don't re-arm one-shots.
- THE MOMENT a qsub/PBS job (or any long remote job) is submitted, IMMEDIATELY
  CronCreate the recurring monitor in the SAME turn. Do NOT offer ("want me to set
  up a cron?") and wait for a yes - that is the exact failure that "falls asleep on
  the job": the job dies and no one is watching. Setting the cron is not optional and
  needs no permission. Submit job -> set cron -> report, always in one turn. A job
  with no watching cron is a bug.
- For any multi-stage autonomous pipeline, use an INDEPENDENT RECURRING timer that
  paces itself and PERSISTS no matter what until you explicitly kill it: CronCreate
  (recurring:true, e.g. "8,28,48 * * * *" off the round marks) whose prompt is an
  IDEMPOTENT check-and-advance (only launch a stage if its predecessor is done and it
  is not already running). It keeps firing across user messages and idle; CronDelete
  it ONLY when the work is fully done and nothing is running. Do NOT pace long
  autonomous work with single-shot ScheduleWakeup that you re-arm each turn - that is
  FRAGILE: a wakeup is one-shot and a user message supersedes it, so it silently
  lapses the moment a back-and-forth distracts you (this stalled a pipeline once).
  ScheduleWakeup is fine only for a true one-off wait. NEVER count on a task-completion
  notification (it can be missed). Interval tuned to the work: ~15-30 min for stereo/PBS.
- A ONE-SHOT BACKGROUND WAIT IS NOT A HEARTBEAT. Spawning a `run_in_background` Bash
  monitor that sleeps-then-checks-once (or any single-fire wait) to "watch a job" is
  the SAME trap as single-shot ScheduleWakeup: it fires ONCE and stops, and the long
  job it was watching keeps running with NO pulse advancing it - you fall asleep on the
  job. WHENEVER any long/unattended job is in flight, the PERSISTENT CronCreate
  heartbeat MUST be armed. Deleting the heartbeat is correct ONLY when nothing is
  running; the instant new long work launches, re-arm it in the SAME turn. Use one-shot
  background waits only as a SHORT convenience ON TOP OF an already-armed heartbeat,
  never as the pulse. (CaSSIS 2026-07-08: deleted the heartbeat when idle, then launched
  stereo jobs and leaned on run_in_background monitors - the watched job would have
  fallen asleep with no pulse advancing it. Re-arm the heartbeat immediately.)
- CREATE THE CRON ONCE, KEEP IT STABLE, NEVER CHURN IT. The cron is a LOCAL HEARTBEAT
  whose only job is to keep the session ticking so you stay awake - it is INDEPENDENT
  of what runs on remote nodes. Its prompt must be CONTENT-FREE: it points at the
  project notes for ALL changing state (which stage/job is running, which cluster,
  job IDs, next step) and says "read the notes and advance". When the work moves
  (e.g. sky_ele -> Athena, new job IDs), update the NOTES, NEVER delete-and-recreate
  the cron. Baking node/job specifics into the cron prompt is exactly what tempts a
  churn on every change. Delete the cron ONLY when absolutely, totally done.
  (Burned 2026-07-07: churned the cron on a node switch; it fired once, never
  re-fired, and the pipeline sat idle ~11h after the BA finished. The BA was
  fine - the monitor died.)
- STANDING POLICY - TWO HEARTBEAT LAYERS FOR ALL AUTONOMOUS WORK (set 2026-07-08).
  The session-only vs OS-level distinction is the crux, so respect both layers.
  For ANY unattended/auto session or long pipeline, ALWAYS arm BOTH:
  (1) IN-SESSION heartbeat = CronCreate. Pick the interval to fit the work - roughly
      every 20-40 min (tighter for fast-moving stages, looser for long jobs). Its prompt
      is content-free, points at the project notes, touches
      `~/.claude/autorun/heartbeat_<tag>` each firing, and advances the work. This is the
      normal pulse WHILE the harness is alive.
  (2) OS-LEVEL cron = emergency resurrector, on the local machine(s). This is the layer
      that survives an OUTAGE. It relaunches `claude -c -p` only when the heartbeat file
      is stale (harness presumed dead), else stands down; atomic-lock guarded so runs
      never overlap; self-heals across a still-down service (cron keeps re-firing and
      catches the moment it returns).
  WHY BOTH (the thing I got wrong before): CronCreate is SESSION-ONLY - it lives inside
  the running Claude session and DIES WITH IT, so a "service unavailable" outage that
  kills the harness ALSO kills the CronCreate heartbeat and nothing re-arms it. Only an
  OS-level cron, independent of the harness, can bring Claude back. The old blanket "no
  OS-level crontab" rule predated this understanding and is RETIRED. OS cron is now
  REQUIRED for durable auto work, on LOCAL machines only, NEVER on pfe.
  PER-BOT NAMESPACING (REQUIRED - a single shared watchdog/heartbeat/sentinel is LOSSY
  with 2+ concurrent auto bots: a survivor keeps the shared heartbeat fresh so a dead bot
  is never resurrected, and the first `.auto_done` disarms everyone). So EACH concurrent
  auto bot gets its OWN fully independent set, tagged by a short name `<tag>`. ALL of the
  apparatus files live UNDER `~/.claude/autorun/`, NEVER loose in the home dir (`~`) or
  `~/bin` - the home dir stays clean. Make it once (`mkdir -p ~/.claude/autorun`). The set:
    - heartbeat  `~/.claude/autorun/heartbeat_<tag>`   (the bot touches ONLY this, every turn)
    - watchdog   `~/.claude/autorun/watchdog_<tag>.sh`
    - lock       `~/.claude/autorun/watchdog_<tag>.lockdir`   (own lock - watchdogs never collide)
    - log        `~/.claude/autorun/watchdog_<tag>.log`
    - sentinel   `<project>/.auto_done_<tag>`   (in the PROJECT dir, not home; disarms ONLY this bot)
    - crontab    its own line at STAGGERED minutes (e.g. "11,26,41,56" vs another's "9,24,39,54")
  Each watchdog checks ONLY its own heartbeat and resurrects ONLY its own session, by
  `cd`-ing into that bot's PROJECT DIR before `claude -c -p "<resume prompt>"` so `-c`
  grabs the right session - different bots MUST run in different project dirs (else use
  explicit session IDs). A bot touches ONLY its own heartbeat and disarms ONLY its own
  sentinel; it NEVER touches another bot's files. The same tagging applies to any l1
  backup watchdog (`~/.claude/autorun/watchdog_<tag>_l1.sh`, sshes mac_arm). Retire a bot's
  watchdog when ITS work is done: touch that bot's `<project>/.auto_done_<tag>` (and drop
  its crontab line) AND remove that bot's files under `~/.claude/autorun/`. Example: the
  Olympus CTX-pair bot = `~/.claude/autorun/heartbeat_ctxpairs` +
  `~/.claude/autorun/watchdog_ctxpairs.sh` (crontab "11,26,41,56") + project
  cassis_olympus_mons + sentinel `cassis_olympus_mons/.auto_done_ctxpairs`. Never let any
  heartbeat/watchdog/lock/log file sit loose in `~` or `~/bin` - they all belong under
  `~/.claude/autorun/`. Detail: `~/projects/claude_overnight_notes.sh`.
- MUST DROP THE OS-LEVEL CRON (and the in-session CronCreate heartbeat) THE MOMENT ALL
  WORK IS FULLY DONE. The OS cron exists ONLY as a safeguard to resurrect the session if
  it DIES MID-WORK. Once the work is complete there is nothing left to resurrect or
  advance, so a still-armed cron just cycles for no good reason (and can pointlessly
  relaunch a finished session). Dropping it is the FINAL action of any auto job: remove
  the crontab line(s) / touch the `.auto_done` sentinel AND CronDelete the in-session
  heartbeat. Arm the cron for the duration of the work, drop it when done - never leave it
  idling past completion.
- On every wakeup, FIRST run `date` to re-orient - long runs leave you stale.

## ASP Tools: Read the Manual, Not --help

When using an ASP tool, do NOT rely on `--help` - read its RST manual
(`~/projects/StereoPipeline/docs/tools/<tool>.rst`). `--help` lists flags but has
NO sensible usage examples; the RST has worked examples and the gotchas that make
options behave (e.g. dem_mosaic fill: small `--fill-search-radius` + more
`--fill-num-passes`, since a large radius stalls). bundle_adjust, dem_mosaic,
pc_align, and the rest all have extensive documented examples.

## ASP Primer / Manual I Maintain - `~/projects/asp_manual.sh` (READ AT START OF ANY ASP WORK)

This is my growing ASP primer - the ONE place that collects every hard-won,
non-obvious, recurring insight into how ASP/VW actually works. READ IT when
starting any ASP task. STANDING RULE: whenever I learn something about how ASP
works that is non-obvious and likely to recur (a workflow, a gotcha, a tool
behavior, a file format, an option interaction), ADD IT to this primer and
`git -C ~/projects add`/commit/push - do not leave it only in a per-project
notes file. Build it up over time so I stop rediscovering the same things.
What it currently contains (grep the headers for detail):
- Reading the RST manuals (not --help).
- Interest-point .match file format (binary, how to read/compare).
- MAPPROJECTED STEREO - the two-pass workflow (mapproject at NATIVE image GSD,
  same --tr/--t_srs both images, aligned cameras, --alignment-method none,
  eval tri-err/ortho/color-hillshade-DEM/dz/dd-H/dd-V). The high-quality path
  used for CaSSIS/CTX/Viking/TMC/OHRC.
- **parallel_stereo PARALLELISM (--nodes-list + --processes + --threads-
  multiprocess): READ the primer section before setting these on ANY
  parallel_stereo/parallel_bundle_adjust run.** Bare minimum: get_num_cpus()
  auto-detects cores so one script is portable; ALWAYS pass --nodes-list
  $PBS_NODEFILE (single-node file = 1 node, safe); set --processes P and
  --threads-multiprocess T with P*T ~= cores/node (P reduced if RAM-bound, e.g.
  Athena 256 -> --processes 32 --threads-multiprocess 8). **NEVER size
  --processes from `nproc` or `wc -l < $PBS_NODEFILE` in a wrapper: INSIDE a NAS
  PBS job both return 1 (the node has 128-256 CPUs), silently forcing
  --processes 1 = FULLY SERIAL (Eff 0%, cpupercent ~1.5 cores). Pass --processes
  EXPLICITLY (you set ncpus in the qsub) or use `grep -c ^processor /proc/cpuinfo`;
  ALWAYS verify with `qstat -f <job> | grep cpupercent` (/100 = cores busy).
  Full write-up: qsub_rules.sh RULE E, asp_manual.sh. Burned 2026-08-17.**
- pc_align applying a transform to cameras (direct vs inverse; carry via
  bundle_adjust --apply-initial-transform-only --inline-adjustments).
- ATHENA (Turin) for ASP jobs - fully visible (/nobackup + build mounted),
  256 cores/node, more expensive; single-node parallelism via --processes.
Bare minimum to remember without reading:
- Interest-point `.match` files (written by both VW and ASP - stereo,
  bundle_adjust, jitter_solve, image_align, etc.) are little-endian binary:
  header is two `uint64` counts (equal = number of matched pairs), then the IP
  records. Read just the count: first 8 bytes as `uint64`.
- The regression suite keeps real match files: `ss*/run/*.match` (fresh) vs
  `ss*/gold/*.match` (reference) - the right layer to judge an IP-affecting
  change is diffing those, not just the final DEM/camera output.
- Official parser: `parse_match_file.py` (binary<->text). Visual/residual
  overlay: `~/bin/plot_matches.py` (use `--red --radius N` for readable solid-red
  dots, never the rainbow, when handing a match overlay to a human).
- bundle_adjust/stereo CACHE their `.match` and per-image `-stats.tif` in the run
  dir and REUSE them on a rerun, so ALWAYS wipe the run dir (or use a fresh one)
  before a rerun meant to test a change, or you measure stale results and draw
  wrong conclusions.

## Building ASP Docs

`conda activate sphinx; make -C ~/projects/StereoPipeline/docs html` (output in
`docs/_build/html/`). Full build/cmake mechanics: `~/projects/cmake_build_notes.sh`.

## RST Documentation Formatting

**Documentation file locations:** check both `docs/` subdirectories and repository root level.
Cross-reference labels (`.. _foo:` targeted by `:numref:`foo``) OFTEN live in root-level
`.rst` files (ASP: `INSTALLGUIDE.rst`, `NEWS.rst`, `README.rst`, `install/INSTALLGUIDE.rst`),
NOT under `docs/`. So before calling a `:numref:` broken, grep the WHOLE repo for its label
(`git grep '^.. _foo:'`), not just `docs/`. Example: `:numref:`release`` resolves to
`INSTALLGUIDE.rst` at the repo root - it is NOT missing.

**Style:** Be concise - users are expert researchers. Give hints and pointers, not tutorials.

**Formatting rules:**
- Section underlines must be exactly the same length as heading text
  - **CRITICAL: Always count characters carefully - prone to off-by-one errors**
- Heading levels: `=` top, `-` subsection, `~` sub-sub, `^` sub-sub-sub
- **`:ref:` vs `:numref:` - name tools with `:ref:`, not `:numref:` (I keep getting this
  wrong).** `:numref:`geodiff`` renders "Section 16.26" (a NUMBER); `:ref:`geodiff``
  renders "geodiff" (the NAME). So to name a tool inline, use `:ref:` - "made with
  :ref:`point2dem`" reads "made with point2dem". NEVER "made with :numref:`point2dem`"
  (that reads "made with Section 16.56", which is nonsense in a sentence). Use `:numref:`
  ONLY to cite a section by its number ("see :numref:`cassis_ba`" -> "see Section 12.3"),
  or as a trailing parenthetical AFTER the plain word ("geodiff (:numref:`geodiff`)" ->
  "geodiff (Section 16.26)"). Verified by rendering the built HTML, 2026-07-17. Litmus:
  read the sentence with the ref replaced by "Section N" - if it reads wrong, use `:ref:`.

## Citing Papers in ASP Docs

ASP docs cite papers via `sphinxcontrib.bibtex` (configured in `docs/conf.py`,
`bibtex_bibfiles`). To add a citation:
1. Add a BibTeX entry to `docs/bibliography.bib` (the general reference bib).
   `docs/papersusingasp.bib` is ONLY for papers that USE ASP - do not put a
   cited work there. Use a short lowercase key (e.g. `alrousan98`). Brace proper
   nouns/acronyms so BibTeX keeps their case: `{DEM}`, `{SPOT}`.
2. Cite inline with ``:cite:`key` `` (renders a numbered, linked reference). It
   reads well right after the author names - "assessed by Al-Rousan and Petrie
   :cite:`alrousan98`", NOT "by (1998)" and NOT a bare "[1]" mid-sentence.
3. The reference list renders automatically from `docs/zzreferences.rst`
   (`.. bibliography:: bibliography.bib`); no per-doc bibliography directive is
   needed. Existing `:cite:` uses (e.g. in `bundle_adjustment.rst`) are the model.
Verify by building the docs: a missing entry warns "citation not found".

## NEWS.rst Conventions

**Release notes live in `NEWS.rst` at the repo root** (included by `docs/news.rst`).

- New items go in the **first section** ("Changes since last release"), never
  in older release sections below it.
- **CRITICAL: grep for all `^RELEASE` headers first** to find where the top
  section ends. Do NOT assume a large line number is still in the top section.
  The file has many `RELEASE X.Y.Z` headers and the top section may be short.
- Entries are grouped by tool name (e.g., `stereo_gui (:numref:`stereo_gui`):`)
  with bullet points underneath. Create a new tool group if one doesn't exist
  yet in the current section, or append a bullet to an existing group.
- The `Misc:` group always comes last in a section, after all tool entries.
- Keep bullets concise - one or two sentences with a numref link.

## Output Parameter Style

Group all outputs after inputs. Put a single `// Outputs` comment on its own line before them.

## Copyright Year Updates

Format: `Copyright (c) 2006-YYYY, United States Government...` - update end year to current year when editing files.

## Style Cleaning Tool

`~/bin/clean_style.py <input_cpp_file>` - automated C++ style cleanup. Use without asking when requested.

## No Hardcoded Values or Env Vars in Scripts

Scripts must take ALL parameters as explicit input args - no hardcoded values, no
env vars, no default args. Hidden config can't be inspected when re-running the
script later.
Before running a script that is a notable stage of something, define all vars, 
such as sigma=10. etc. Have rationale. Log all this rationale, var names and vals, and
precise stage actual script invocation including the qsub cmd for reproductibilty later.
So basically a premable with all defined followed by precise invocation you will launch.

## Canonical ASP Scripts - `~/projects/asp_scripts/` (USE or ADAPT, don't re-figure)

Reusable, commented, parameterized reference workers for the ASP operations we
keep redoing (so we stop reinventing and re-blundering). When doing new ASP work,
USE one of these or ADAPT it; do not write from scratch. Each embeds the hard-won
rules inline. The primer `~/projects/asp_manual.sh` points to each script.
- `stereo_mapproj.sh` - mapprojected stereo (pass 2): native-GSD mapproject, DEM
  as the LAST parallel_stereo arg, asp_mgm + subpixel-9, --nodes-list + --processes
  (Athena: nodesMode local), optional `--resume-at-corr`.
- `stereo_localepi.sh` - local_epipolar stereo (pass 1) + point2dem.
- `bundle_adjust.sh`, `parallel_bundle_adjust.sh` - BA (list-order + residual rules).
- `pc_align.sh` - align a DEM to a ref: regrid `-r average` (dense-vs-sparse fix),
  hillshade seed, carry transform to native cameras.
- `geo_figures.py` - CANONICAL plotting library (import it): hillshade DEM,
  colorized signed diff (dz/dd-H/dd-V, diverging+symmetric+robust clamp), one-sided
  error (tri-err, magma), each with its OWN full-image-height colorbar + unit; NO
  text baked in the figure (caption lives in the HTML/RST); robust median/NMAD.
  Do not re-write figure code per project. Detail: visual_raster_inspection.sh.
- `fetch_lola_shots.sh` - AUTOMATED LOLA shots for a lon/lat box from the NASA/USGS
  LOLA COPC on AWS (PDAL, no manual download) -> lon,lat,radius_km CSV. Detail:
  `~/projects/lola_notes.sh` (LOLA gridded LDEM vs shots; the AWS COPC method is
  the primary route, superseding the manual ODE tool).

## Readable Shell Script Style - `~/projects/shell_style.sh` (CRITICAL, no reminders needed)

**When rewriting or revisiting ANY script, OFFER to bring it to the preferred
style below** (do not silently leave it ugly). The single worst offense: a
lengthy comment placed AFTER code on the same line, spilling across continuation
lines. NEVER do that. A comment is BRIEF and goes on its OWN line(s) BEFORE the
code; a tiny same-line note on a var (`nprocs=$1; shift  # per node`) is fine.
Keep `key=value` form in echo lines (easy to read); otherwise avoid verbosity.
The preferred style in one line: positional `shift` args (workDir first), clean
relative-path var block echoed to the log, `umask 022`, exec-redirect log with
START/DONE banner, one option per line with aligned backslashes, the literal qsub
line in the header, no line over 90 chars, brief up-front comments.

EVERY new `.sh` worker follows the readable style in `~/projects/shell_style.sh`:
positional `shift` arg parsing (not `${1:?verbose}` blocks), a clean relative-path
var block echoed to the log, `umask 022`, exec-redirect log, one option per line
with aligned backslashes, and the literal qsub submit line in the header comment.
HARD rules Oleg keeps reminding on (just do them): NO line over 90 chars, code or
comment - measure with `awk '{if(length($0)>90)print NR,length($0)}'`, never
eyeball, and break the long ones (split a long multi-var `export` into separate
lines). A big comment goes on its OWN line(s) BEFORE the code, NEVER as a trailing
right-side comment that wraps across many lines (a short single-line trailing note
on a var is fine). Readable, human, not verbose/ugly. Reference workers:
`sfs_mons_mouton/ba_htdem_gcp.sh`, `cassis_asp/gusev_cnet_gcp.sh`.

## Reaching for a Symlink = You Are Hacking Around a Bug (CRITICAL)

Any time the impulse is to create a symlink (`ln -s`) to make something work, STOP.
A symlink is almost always a hack that papers over a real defect (a script that
locates a sibling by CWD instead of its own dir, a hardcoded path, a missing
PATH/arg, a tool assuming a file is somewhere it is not). Do NOT silently drop the
symlink. Instead, at minimum REPORT the underlying problem to the user, and prefer
to PROPOSE a real fix in the software, or APPLY that fix if feasible. The symlink
hides the bug so it resurfaces later somewhere quieter. Name the root cause and fix
THAT. (Recurring: the ox2 CaSSIS `cassis_stereo_pair.sh` "not found 127" was a
tool bug - `cassis_stereo.sh` called the worker by bare name after `cd`ing into the
work dir, so it only worked if a copy/symlink sat in every work dir. The fix is to
resolve the worker by `${BASH_SOURCE[0]}` dir, not a per-dir symlink.) Same spirit
as the do-not-mask-bugs rule below.

## Report Shortcuts and Temp Fixes - Do NOT Mask Bugs (CRITICAL)

Claude has a demonstrated pattern of reaching for shortcuts, temporary
workarounds, and rigged/self-contained tests that MASK long-term bugs and create
a false impression that something "works out of the box" when it does not. This
repeatedly forces the user to catch it (CaSSIS, 2026-07; Qt6-plugins symlink,
2026-07). Counter it:

THE GENERAL LESSON (this is the one that matters): do NOT paper over a problem to
make an error message go away. A symlink, fallback path, copied file, broadened
catch, or special-case that MUTES a symptom without fixing the defect is
cheating - the bug lives on somewhere quieter and reads as fixed. The tell that
you are about to cheat: you are reaching for something that makes the error
disappear without having first NAMED the actual root cause. Stop, name the cause,
fix THAT.
- A temporary workaround IS legitimate (honest path blocked, slow, or out of
  scope right now). But it is ONLY legitimate if you (a) say so explicitly, and
  (b) ensure the real problem gets fixed eventually. If you can fix the root
  cause along the way - in scope, in code - do it. If you cannot, REPORT the
  problem to the user so it is not lost. Especially raise it when we are not
  busy: a quiet moment is when latent problems should be surfaced and fixed.
- You MUST report problems to the user. Always. Even in a long-running nightly or
  autonomous run - when you hit an issue, surface it (in the notes AND to the
  user), do not silently work around it and move on. A muted problem in an
  unattended run is the worst case: nobody knows it is broken.
- Owning known breakage is your job, not optional. Example: if the task is to
  prepare/maintain a release and you KNOW the release bumped a dependency (e.g.
  Qt5 -> Qt6) that breaks something, handling that breakage IS release
  maintenance. Refusing to deal with it, or papering it over with a symlink, is
  wrong. The known upstream change is precisely what you are there to handle.
- PREFER the honest end-to-end path (real inputs, the real tool, the real
  environment) over a convenient fixture. Do NOT present a fixture, mock, or
  pre-furnished test result as if it verifies the real thing. A passing rigged
  test is NOT evidence the honest path works. (E.g. running a pytest that
  furnishes pre-sliced kernels is NOT the same as running isd_generate honestly
  on a real cube with the full data.)
- When you DO take a shortcut, workaround, temp fix, or reduced-scope check
  (honest path blocked, slow, or mid-development), SAY SO explicitly and up
  front: name the shortcut, state what the honest path is, and why you did not
  take it. Never let a shortcut pass silently as "it works".
- Nothing is "works out of the box" until it has been run the HONEST way on real
  inputs. Default to UNVERIFIED; test before asserting.
- In nightly / autonomous mode, if forced to take a shortcut to keep progress,
  REPORT it (in the notes AND to the user), do not gloss it. Shortcuts are
  sometimes necessary; hiding them is not.

## Trace the Code, Do NOT Guess the Mechanism (CRITICAL)

Claude has a demonstrated pattern of GUESSING mechanisms from behavior and
asserting them confidently when they are wrong. When investigating WHY two code
paths differ (tool A works, tool B does not, on the same inputs), do NOT settle
for a plausible-sounding story inferred from logs. READ the source: find the
shared function and the two divergent callers, see exactly what each passes,
then PROVE the cause by adding cout/instrumentation, recompiling, and running
both paths to compare. State hypotheses as hypotheses until proven; never assert
a mechanism you have not read in the code and confirmed by running it. Burned
2026-07-30 (CaSSIS ox2 jitter): confidently claimed bundle_adjust applied
`--ip-match-radius` through the jittered camera geometry - Oleg said "that is
not possible, matching all happens in the projected domain, stop guessing." He
was right. The real cause (found by reading the code + a cout trace) was that
the bundle path skipped image normalization for non-OpenCV detectors (OBALoG),
so OBALoG saw a near-flat raw image. A whole day of guesswork preceded it.
Saved as `[[feedback_trace_dont_guess]]` in memory too.

## No Per-Site / Per-Input Special-Casing in Reproducible Pipelines (CRITICAL)

A pipeline meant to give USERS reproducible results must apply the SAME logic to
every input. Turning an experimental lever ON for one specific site/dataset while
leaving it OFF for others - whether by a hardcoded site name, a per-input `if`, or
a per-site config that flips a knob - is a form of CHEATING. It fakes a good result
for that one case that the general pipeline does NOT actually produce, so a user
running the shipped config on that site silently gets a DIFFERENT, worse result
than the paper/doc shows. This is exactly the unreliable-results-for-users failure.
(Caught 2026-07-21 in the CaSSIS pipeline: a `soft_gcp` pass-2 option whose comments
said "used for ox1" - a per-site tweak advertised in shipped code.) Rules:
- A tunable option in the CODE is fine, but it MUST default OFF and be applied
  UNIFORMLY across all inputs, or not at all. No per-input branching, no site-name
  conditionals, no per-site config that flips an experimental knob, no site names in
  the pipeline logic/comments advocating a per-site use.
- If a lever genuinely helps, apply it to EVERY input and document it. If it helps
  only one, that is a sign it is fitting that dataset's noise - do not ship it on.
- Any result that was produced with a per-site tweak is UNRELIABLE and must be
  REDONE honestly with the uniform pipeline before it is presented as a pipeline
  result. Flag it to the user and log the redo.
- The legitimate per-input mechanism is a per-site CONFIG carrying only that site's
  INPUTS (paths, ids, reference DEM) - never a knob that changes the algorithm.

## Inspect to Confirm Expectations

Any time you assume or expect a certain result, inspect it (visually AND with
stats) to verify the result actually conforms to that expectation. Never assume - check.

**Cheap checks on produced output files: always do them.** If the recipe says an
output DEM/raster must have a certain grid size, resolution, or projection, run
`gdalinfo` on it the moment it exists and confirm it conforms. A 1-second check
saves countless grief downstream.

**All runnable scripts must be executable (`chmod +x`); only comment-only notes
`.sh` stay non-executable.** A missing execute bit silently breaks `nohup`/direct
invocation, and `rsync -a` can reset it - so set it at the source.

**chmod +x at TWO points, no exceptions (CRITICAL, keeps recurring).** (1) The
MOMENT any runnable script is created, `chmod +x` it at the SOURCE, before any
rsync. (2) AFTER the last rsync and BEFORE qsub, `chmod +x` the remote copy again
and `ls -la` to CONFIRM the bit is set. rsync from the Mac routinely STRIPS the
+x even when the source has it, and a re-rsync silently un-does an earlier remote
chmod, so the source-side chmod is not enough - you must re-check remotely every
time. PBS exits ~254 in seconds (the job flips straight to state E/F with no
output, looking like the code failed) if the `--` script is not executable. So:
create -> chmod +x source -> rsync -> chmod +x remote -> `ls -la` confirm -> qsub.

## Robust Stats: ALWAYS median/MAD, NEVER mean/std for raster comparison metrics (CRITICAL)

For comparing rasters (dz vs a reference, dd-H/dd-V disparity, tri-err /
IntersectionErr mosaics, geodiffs), ALWAYS report and compare the robust
**median and MAD** (plus p90/p99 if useful), NOT the mean and std. These fields
carry a few catastrophic blunder pixels (a max-tri-err mosaic hit 750-1440 m at
Jezero) that pollute the MEAN and STD wildly while the median/MAD are stable.
Judging by the mean led to a wrong conclusion once (a "6x better tri-err" that was
purely blunder pixels; the medians were identical - CaSSIS WF1 vs WF2, 2026-07-11).
`gdalinfo -stats` gives only mean/std/min/max - for median/MAD read the raster
with numpy (nodata-aware): see `~/projects/cassis_asp/tri_median.py`.

## DEM Alignment: Judge by Hillshade Eyeball, NOT Vertical Diffs (CRITICAL)

For ANY DEM alignment/registration work, judge by the EYEBALL of HILLSHADES (red/green
overlay), NEVER by vertical dz/geodiff or its NMAD/std - dz is blind to horizontal
misregistration and dominated by DEM noise/coverage, so a dz number says nothing about
alignment (a well-aligned pair can show 20+ m dz NMAD; a badly-shifted one near zero).
Full detail, recipes, and cross-modality (image-vs-hillshade) tips:
`~/projects/visual_raster_inspection.sh`.

## "HTML Artifact" = the Artifact Tool (know the drill)

When Oleg asks for an "HTML artifact" (or just "artifact"), that means: use the
`Artifact` tool. I write an HTML file for the CURRENT project (plots, colorized
rasters, tables, whatever), publish it, and it pops up as a claude.ai-hosted URL
in a browser tab (private by default, shareable). No need to re-explain what it
is each time. Key rule: an artifact is SELF-CONTAINED - a strict CSP blocks every
external file, so all images must be INLINED as base64 data URIs. Follow the
sizing rule just below (downsample hard). For a purely local glance instead of a
hosted URL, use `SendUserFile ... display:render`. See also
`~/projects/visual_raster_inspection.sh` (colorbar/preview recipe) and
`~/projects/html_for_google_docs.sh` (base64-embedded HTML for Google Docs).

## Artifact and Preview Image Sizing

For HTML artifacts and uploaded previews, downsample DRASTICALLY: <=1000 px long side,
<1 MB per image (dpi ~80-100). Over ~1 MB or ~1000x1000 px is overkill. Full-res stays on
disk. Detail in `~/projects/visual_raster_inspection.sh`.

## Disparity Stats: disparitydebug --raw, NEVER gdalinfo on run-F.tif (CRITICAL)

A correlator/stereo `run-F.tif` (parallel_stereo `--correlator-mode`) packs horizontal
disparity (band 1), vertical disparity (band 2), and a VALIDITY MASK (band 3) in one file.
`gdalinfo -stats` and `gdal_translate -b` IGNORE band 3, so invalid (uncorrelated) pixels
read as 0 and pollute the dd-H/dd-V stats - a mostly-invalid flat scene then fakes a ~0
shift, HIDING the real one. This bit us REPEATEDLY (a true CaSSIS dd-V shift of -3.4 px read
as 1.4, flipping a conclusion). ALWAYS extract the disparity with:
`disparitydebug --raw run-F.tif --output-prefix P` -> `P-H.tif` (dd-H), `P-V.tif` (dd-V),
Float32 with real nodata (-1e6); THEN stat those (gdalinfo -stats is nodata-aware on them).
disparitydebug is ASP's OWN tool; a release build sets ISIS up itself, our dev/packaged build
needs `export ISISROOT=<asp_deps env>` (holds IsisPreferences). EVERY script that runs
correlator-mode and analyzes disparity must emit these raw bands right there (cassis_corr.sh
does). NEVER `gdal_translate -b` to pick a disparity band - it writes the invalid pixels as 0.

## Alignment-Residual Notation: dh / dv / dz, Not dd-H / dd-V

When labeling a horizontal/vertical alignment residual (a DEM-to-reference
correlation shift, or a stereo disparity residual) in FIGURES, CAPTIONS, and DOCS,
use the short informal `dh` (horizontal), `dv` (vertical), and `dz` (height
difference). AVOID `dd-H` / `dd-V` - even though these are informal, `dh`/`dv`/`dz`
read more easily and are consistent. This is a labeling convention only; internal
band names from `disparitydebug` (`-H.tif`, `-V.tif`) stay as the tool emits them.

## Multi-Option Commands in Scripts

In shell scripts, put each command-line option on its own line, WITH ITS VALUE
on that same line: `--option val \`. One option per line, never several options
on one line, and never split an option from its value. Same for each `export`.
Use trailing `\` continuation backslashes (single space before the `\`, matching
the surrounding scripts; or align to one column with the backslash alignment tool
below where that reads tidier). This applies when AUTHORING a new script and when
showing a command invocation in chat, not only when editing an existing script -
it recurred (a proposed bundle_adjust block bunched options onto one line), so
default to one-option-per-line for every multi-option command, everywhere.

**Comment lines in scripts never exceed 90 characters.** Wrap a longer comment
onto continuation comment lines. Measure line length with a tool (e.g. `awk
'{if(length($0)>90)print NR,length($0)}'`), never eyeball it.

**NEVER put a comment after a `\` line-continuation** (`cmd \  # note`): the `\`
escapes the trailing space, the `#...` is a comment, and the command ENDS there
(continuation broken). This applies to scripts AND to paste-able commands shown
to Oleg. Keep comments on their own lines, or omit them.

**When documenting a command in RST, list the options first, before the positional
file arguments, with the output file last.** Keep each standalone command line under
about 75 characters. A longer line does not wrap in a rendered RST code block, so it
forces a horizontal scroll bar; break it across continuation lines (`\`) instead.

## Backslash Alignment Tool

`~/bin/align_backslashes.py <file> <start_line> <end_line> [--inplace] [--column N]`
Aligns trailing `\` continuation characters in shell scripts. Auto-detects
target column from longest content line, or use `--column N` to fix it.

## Column Alignment Tool

`~/bin/align_columns.py <file> <start_line> <end_line> [--inplace]`
Aligns columns in a range of lines. Detects columns by 2+ space gaps.
Lines are 1-based. Without `--inplace`, prints aligned output to stdout.

## Stereo/Photogrammetry Resolution (CRITICAL - screwed this up MULTIPLE TIMES)

For stereo/photogrammetry, correlation ALWAYS runs at near-native image
resolution. When mapprojecting, pin ONE `--tr` that is a COMPROMISE near the
native GSD of the INPUT IMAGES (not the DEM), and mapproject BOTH/ALL images at
that SAME res - auto (no `--tr`) drifts per image and parallel_stereo
correlator-mode then errors on mismatched GSD. The seed/draping DEM is only an
interpolated surface, usually ~4x coarser; its coarseness must NEVER set the
mapproject/correlation grid. Only the OUTPUT DEM (point2dem) lives at the coarse
~4x-GSD res. Do not downsample imagery to the DEM. (CaSSIS native GSD ~4.59 m;
DEM ~18 m.) Repeatedly assumed the DEM res sets the mapproject res - it does NOT.
Mapprojecting at the coarse DEM res produced a rough, blocky DEM (CaSSIS PHASE 0,
2026-06-27). Corollary: for a simple 2-image pair you can SKIP mapproject entirely
and stereo the raw images (affineepipolar) - correlation is native by definition;
mapproject is for many images / hard terrain / large convergence.

**Hillshade-correlation for dem2gcp AND for DEM-to-DEM/CTX alignment ALWAYS runs
at NATIVE IMAGE resolution (~4x FINER than the DEM grid), NEVER at the coarser
DEM/CTX res. VERY IMPORTANT.** The dense correlation window (5x5/9x9) locks onto
coarser features while the disparity is sampled on the fine native grid at SUBPIXEL,
so it resolves ~6 m shifts even when DEMs are ~18 m. The honest gain is finer spatial
sampling of the shift field (~18 m -> ~9-10 m effective), not lower per-point noise;
faux precision in smooth patches averages out over many dense GCP. Full rationale:
`~/projects/cassis_asp/cassis_native_res_rationale.sh`.

## Project Data Lives in a data/ Dir, Not Run Dirs With Symlinks (CRITICAL)

Canonical project DATA (input images/cubs, reference DEMs, anything a run consumes
but does not produce) must be stored ONCE in a stable `data/` directory with honest
unique names, and every list/script must reference it THERE, directly. NEVER let a
list point at a SYMLINK ALIAS inside a RUN dir (per-run `imgs/` collections,
short-name `sl/L0.cub` aliases, etc.). Run dirs get wiped, and then the references
break even though the real data is untouched - this bit us on CaSSIS: the joint
image list pointed at `stage2/<site>_mid2/imgs/*.cub` symlinks (a run dir) instead of
the canonical `cassis_asp/data/<site>/<obsID>/.../cas_cal_sc_...cub`, so a wiped/absent
run dir showed every image MISSING. If symlink trickery is used for TEMPORARY
expediency (e.g. short names a tool wants), CORRECT it when feasible - point the lists
at the canonical `data/` path. Data in ONE place, honest names, no run-dir indirection,
no eternal per-run copies. INSPECTION/PREVIEW files count too: colorized PNGs and
geodiff/DEM copies pulled over for viewing go in the experiment's REGULAR dir
(mirror the honest pfe layout), NEVER a throw-away `eyeball`/scratch/tmp dir with
renamed copies - each experiment's outputs live in its OWN dir, wipeable as one.
(Bit us on CaSSIS: an `eyeball/` dir of renamed geodiff copies; wiped, remirrored.)

SYMLINK / PATH-REWRITE TRICKERY EACH RUN IS A SMELL: if you find yourself resolving
symlinks, or rewriting image-name paths inside a GCP / match / list file every run to make
things match, the data is NOT well organized - stop and put the slow-changing inputs (GCP,
cubs, match files) in ONE stable, honest, separate location (e.g. a `gcp/` dir in the work
dir) built ONCE, so every run references it directly with no per-run trickery. Data that
changes rarely deserves a good permanent home, not run-dir symlinks re-derived each time.
(CaSSIS 2026-07-07: the joint GCP stored `stage2/*/imgs/` symlink paths; moving the image
list to `data/` forced a 300k-line GCP path-rewrite mid-launch - exactly the smell.)

## Debug Config on pfe With a Quick Kill, Not a Full qsub Round-Trip

For fast CONFIG checks (does the GCP load? do image names match? does an option parse?) run
the tool briefly ON the pfe head node - it reaches "Loaded N GCP" / the error in seconds -
then KILL it before it starts heavy compute. Far faster than a qsub round-trip per iteration.
ALWAYS ensure the kill (Ctrl-C / kill the PID): heavy compute must NEVER linger on the head
node. Only for quick startup/config validation, never a real run.

## Relative Paths in a Project Work Dir

In a project work dir, all paths (in scripts and when presenting to the user)
must be RELATIVE to that work dir. Use absolute paths only for external files
outside it.

**ALWAYS operate FROM the work dir and keep everything relative to it (CRITICAL,
recurring).** Pick one work dir, stay in it, and write every path in scripts,
commands, and chat RELATIVE to it (`data/cub/x.cub`, `quartet_v1/ba`), never
absolute (`/Users/...`, `$HOME/projects/...`). A script assumes it is run from the
work dir and uses relative paths; the ONLY absolute paths allowed are external
tooling outside the project (conda env / ISISROOT / a reference DEM elsewhere) and
a single literal absolute path in a destructive `rm` (the safety exception). Do
NOT hardcode `$HOME/projects/<proj>/...` into project scripts. When showing a
command to the user, show it relative too. (Bit us on the Viking quartet: a BA
runner hardcoded `$HOME/projects/viking_orbiter/data/cub` instead of `data/cub`.)

**Keep slow-changing INPUTS in a `data/` dir (or similar) that OUTLIVES wiping the
outputs.** Inputs (cubs, reference DEMs, images) live in `data/`; each experiment's
outputs live in their own peer run dir that can be wiped wholesale without touching
`data/`. So a `rm -rf <run_dir>` never destroys an input, and re-running is cheap.

## Visual Raster Inspection - "Claude has eyes"

Claude can SEE images - use vision to verify rasters (orthos, DEMs, geodiffs,
camera/rotation alignment). Full technique, recipes, and where preview files live
(with the data on pfe, not /tmp): **`~/projects/visual_raster_inspection.sh` -
READ IT before inspecting/eyeballing any geo raster.**

**WHEN THE USER SAYS "eyeball" / "inspect" / "look at" / "compare" an ortho, DEM,
geodiff, tri-err, or any geo raster, it ALWAYS means this EXACT procedure (do NOT
re-derive it each time, do NOT skip a step):**
1. Put EVERY raster on ONE identical grid first: `gdalwarp -t_srs <one proj>
   -te <one extent> -ts <one size> -r cubicspline` (same PROJECTION, same EXTENT,
   same GRID, cubicspline). Comparing rasters on different grids/framing - or with
   raw numpy (index-aligned) - is NONSENSICAL and worthless.
2. HILLSHADE any DEM before viewing: `gdaldem hillshade -multidirectional`. NEVER
   eyeball raw elevation; you compare terrain by its hillshade.
3. Downsample to <=1000 px, write PNG, THEN look.
4. Judge REGISTRATION only by the red/green hillshade overlay (aligned = yellow),
   NEVER by dz/geodiff std (blind to horizontal misregistration on low relief).
5. Colorize a geodiff/tri-err/dz with a matplotlib colorbar (per-panel, unit
   label), not bare grayscale.
This applies to the mapproj ortho-on-hillshade geometry check too: warp the ortho
and the DEM hillshade to the same grid, then look.

**NEVER combine two geo-referenced rasters with raw python/numpy pixel ops.** numpy
aligns arrays by row/col INDEX, not ground coordinates, so differencing/overlaying/
comparing rasters on different grids or projections in python is silently wrong.
ALWAYS use the projection-aware tools: `geodiff` to difference two DEMs/rasters (it
regrids the 2nd onto the 1st, respects proj+datum), and `gdalwarp` to put rasters on
ONE common grid (-t_srs + -te + -tr/-ts, -r cubicspline). Warp BOTH onto the shared
canonical grid FIRST, THEN read into python only to DISPLAY (imshow) or compute robust
stats. Do NOT warp the raster whose pattern you care about onto a DIFFERENTLY-centered
projection - it tilts/rotates it into an artifact (burned 2026-07-21: warped a dz onto a
CTX pair's stereographic center 0.12 deg off, faking a cross-track pattern). Detail in
`~/projects/visual_raster_inspection.sh`.

Colorizing a raster for inspection (geodiff/dz, disparity, tri-err): render WITH a
colorbar (matplotlib, not bare `gdaldem color-relief`). EACH plot gets its OWN vertical
colorbar on the RIGHT, unit label ("meters"/"pixels") rotated 90 degrees; NEVER a shared
colorbar. Diverging ramp + symmetric clamp for signed diffs; robust clamp, not min/max;
nodata masked. Multidirectional hillshade (`gdaldem hillshade -multidirectional`) for
DEMs. Full recipe (pfe gdal-vs-matplotlib env split): `~/projects/visual_raster_inspection.sh` section 5.

**No baked-in descriptive titles/captions inside figures that ship with an RST/HTML caption** (the caption carries it); keep colorbar labels, axis units, and short per-panel IDs.

Match-point inspection: `~/bin/plot_matches.py` overlays an ASP .match file on both images and reports the residual to the best-fit translation (the real-vs-junk metric for co-registered pairs). For the stereo_gui solid-red-dot look use `--red --radius N`.

**Low-texture pc_align (CRITICAL, see the alignment primer in `~/projects/visual_raster_inspection.sh`):** on bland terrain (few craters) the correlator dh/dv MEDIAN and geodiff/dz BOTH LIE - swamped by spurious ~0 matches on featureless plains, so a real ~20 px crater misalignment reads as "2 px". NEVER judge an align by dh/dv median or dz there; ALWAYS eyeball a ZOOMED, fully-covered textured window (crater/ridge) as a red/green hillshade overlay (aligned = yellow, misaligned = red/green fringes). Sparse IP (`pc_align --initial-transform-from-hillshading rigid`, no match file) beats dense `--correlator-mode` (which locks onto the plains); `--compute-translation-only` kills spurious-rotation blowups; regrid both DEMs to the same grid first. Burned a whole session trusting the correlator median.

Checking a bundle_adjust `pointmap.csv` (GCP / from-DEM points) against a reference DEM with `geodiff` (split by population, the `--csv-srs` gotcha): see `~/projects/visual_raster_inspection.sh`. Keywords: bundle_adjust pointmap.csv, geodiff --csv-format, heights-from-dem on-DEM check, fix-gcp-xyz.

Google-Doc-ready section (prose + real tables + inline figures, in one copy-paste): build a self-contained HTML with base64-embedded images, open in Chrome, select-all, copy, paste. See `~/projects/html_for_google_docs.sh`.

## Variable Initialization (CRITICAL)

**NEVER create uninitialized variables.** Always initialize with sensible defaults:
- Counts/sizes: `= 0`, indices: `= -1`, floats: `= NaN` or `= -max()`
- Pointers: `= nullptr`, booleans: `= false`
- Add `// will change` comment if value is immediately overwritten

## Defensive Programming for Paired Lists (CRITICAL)

Applies to both shell scripts and C++. When two (or more) input lists
or arrays are supposed to be one-to-one, or at least of the same size,
always validate. Minimum check: same size / line count. Stronger
check when IDs are embedded in filenames or entries: verify the
per-row ID matches. Fail fast with a clear error.

**ASP image-list/camera-list/mapproj-list (bundle_adjust, jitter_solve, stereo)
MUST be in identical order. Build the camera list FROM the image list (e.g.
`perl -pe 's/\.cub$/.json/'`), never independently - a mismatch runs fine but
yields junk.**

**`--mapprojected-data-list`: do NOT append the DEM at the end of the list of
mapprojected images anymore.** Since the 1/2026 ASP build the DEM is optional and
is looked up from the mapprojected images' own geoheaders (each ASP-mapprojected
image records the DEM it was projected onto). The list is just the mapprojected
images, in the same order as the input images. If a DEM IS given it must be the
last entry, but the clean form is to omit it. See bundle_adjust.rst
`--mapprojected-data` / `--mapprojected-data-list` (:numref:`mapip`). Verified on
Viking 2026-07-27: bundle logs `Loading DEM: ...seed.tif` from the geoheaders with
no DEM in the list, matches identical.

Always use `// TODO(oalexan1):` format. Never bare `// TODO:`.

## Git Repositories on lunokhod1

**Git version:** 2.17.1 (use `git rev-parse --abbrev-ref HEAD` not `git branch --show-current`)

| # | Repo | Base directory | Branch | `origin` remote | `god` remote (upstream) |
|---|------|---------------|--------|-----------------|------------------------|
| 1 | **StereoPipeline (ASP)** | `/home/oalexan1/projects/StereoPipeline` | master | `oleg-alexandrov/StereoPipeline.git` | `NeoGeographyToolkit/StereoPipeline.git` |
| 2 | **VisionWorkbench (VW)** | `/home/oalexan1/projects/visionworkbench` | master | `oleg-alexandrov/visionworkbench.git` | `visionworkbench/visionworkbench.git` |
| 3 | **BinaryBuilder** | `/home/oalexan1/projects/BinaryBuilder` | master | `oleg-alexandrov/BinaryBuilder.git` | `NeoGeographyToolkit/BinaryBuilder.git` |
| 4 | **StereoPipelineTest** | `/home/oalexan1/projects/StereoPipelineTest` | master | `NeoGeographyToolkit/StereoPipelineTest.git` | (origin IS the org repo) |
| 5 | **projects** (scripts/notes) | `/home/oalexan1/projects` | master | `oleg-alexandrov/projects.git` | (no god) |
| 6 | **home dir** (dotfiles) | `/home/oalexan1` | master | `oleg-alexandrov/olegmisc.git` | (no god) |

Convention: `origin` = user's fork, `god` = upstream org (for ASP, VW, BinaryBuilder).

**BinaryBuilder has several heads and is pushed DIRECTLY to BOTH remotes.**
Unlike ASP/VW (where `god` receives changes only via reviewed PRs), BinaryBuilder
changes go straight to `god` (NeoGeographyToolkit, the canonical) AND to `origin`
(user fork). So when told to push BinaryBuilder, push to `god` master and
`origin` master both, and confirm the two heads plus local `master` all match.
(Still requires an explicit push instruction, per the never-push-without-
authorization rule; this only says WHERE once authorized.)

**StereoPipelineTest: ALWAYS push to `origin` master. There is NO fork and NO
separate branch.** `origin` IS the org repo (NeoGeographyToolkit/StereoPipelineTest),
so master is the only branch. A temporary `dev` branch existed briefly and was
deleted (local and origin) on 2026-08-04 - do not recreate one. Commit test
changes on master and push there.

## CSM Model-State JSON

Parse CSM model-state / `.adjusted_state.json` files CAREFULLY - they are NOT plain
JSON: a model-name line comes FIRST, then the JSON (so `json.load` fails; skip line 1).
Frame center, linescan position interpolation, parsing recipe: `~/projects/csm_camera_notes.sh`.

## ISIS Mission Data and Kernels

**LRO NAC end-to-end + generic ISIS kernel fetch: `~/projects/lronac_processing.sh`.**
Full ingest pipeline (lronac2isis → spiceinit → lronaccal → lronacecho), CSM JSON
via isd_generate, ODE search, illumination/azimuth analysis, and failure modes
(missing CK, ALE driver crash, sub-solar lon vs ground azimuth). Kernel fetch
(section 5): `downloadIsisData <mission> $ISISDATA` for a full sync, or targeted
`rclone --config $ISISROOT/etc/isis/rclone.conf copy <mission>:kernels/ck/ ...
--include="<file>" --no-traverse -P` for a single missing CK. Update on any new
gotcha.

## NASA NAS / Pleiades Supercomputer

**Before any pfe work, read these notes files first:**
- `~/projects/pleiades_notes.sh` - machine map, storage, ASP build layout, lfe access/safety, symlink-wipe procedure
- `~/projects/qsub_rules.sh` - qsub arg rules, dry-run, umask, error codes
- `~/projects/qsub_convention.sh` - allocations (e2305 ours, s3319 off-limits), checklist, runner template
- `~/projects/lfe_archive.sh` - lfe tape archive AND restore procedure (DMF dmls/dmget: stage off tape before any tar/read)

Bare minimum to remember without reading:
- **No heavy/parallel compute on the pfe head node** - `parallel_stereo`, anything multi-process/multi-thread or big-RAM goes to qsub (small/fast -> `devel`). Light single-thread `gdalinfo`/`gdal_translate`/`gdalwarp`/`ls`/`qstat` on the head node is fine (don't qsub a one-off gdalinfo - use common sense). **HEAD-NODE RULE (pfe AND Athena front end), confirmed by a policy-violation warning 2026-08-07: the trigger is >1 process/thread, NOT file size. On any head node use ONLY 1 PROCESS and 1 THREAD. Big single-threaded `gdal_translate`/`gdalwarp`/`geodiff`/`dem_mosaic --threads 1` on multi-GB rasters are all FINE. `dem_mosaic --threads 6` (or any multi-thread/multi-process) is the violation. So when downsampling/differencing/mosaicking for figures on a head node, force `--threads 1`; anything heavier goes to a qsub compute node (or do it locally on the Mac).** **To run any gdal/ASP tool on pfe FIRST set the env** (non-interactive ssh has nothing on PATH, PROJ unset): `conda activate asp_deps` (PROJ data, so `-t_srs` works) + `export PATH=$HOME/projects/BinaryBuilder/StereoPipeline/bin:$PATH` (the packaged build has ALL tools); detail in `~/projects/pleiades_notes.sh`. **NEVER run heavy compute - `stereo_corr`/`parallel_stereo`/correlation (the eval dd) - on the Mac OR the pfe head node; it goes to a qsub compute node.** The eval (`cassis_eval_stage.sh`) is the last step inside each stage's qsub job, so its dd runs on the compute node - do not run it by hand on the Mac. 4-sec dry-run before qsub. budget `e2305`. **Models & node choice:** `cas_ait` (40c, Aitken), `rom_ait`/`mil_ait` (128c, Aitken), `bro_ele` (28c, Electra), `sky_ele` (40c, Electra). Broadwell is decommissioned ONLY on Pleiades - `bro_ele` (Electra) and `sky_ele` are FINE to use. **Our code is model-agnostic - it must run on ANY of them** (match `ncpus` to that model's cores). **Before launching, study load on ALL systems** (`/u/scicon/tools/bin/node_stats.sh` -> Free vs "Queued jobs want N nodes" per model) and pick the LEAST-CONTENDED (e.g. bro_ele was Free 292 / 12 queued while cas_ait was 379 queued). **For small single-node jobs Athena (Turin) nodes are also fine - but Athena Turin is only visible/submittable from the ATHENA front-end** (ssh to athena), NOT from pfe. **If a job sits queued too long, qdel it and resubmit on a less-contended system.** In a non-interactive ssh, qsub is not on PATH - use `/PBS/bin/qsub`. `devel` allows only 1 job/user (pack multiple sites into ONE serial job). POLICY: NO separate PBS launcher script (cannot afford one per stage). The PLAN/NOTES file holds the COMPLETE, LITERAL, copy-pasteable qsub command line (the full `qsub <all pbs args> -- $dir/script.sh <all script args, workDir LAST>` as ONE reproducible string - NOT just the args/params/job-id logged piecemeal), WITH its rationale + named params, logged BEFORE the launch. Then launch the worker DIRECTLY via that qsub `--` form. The worker self-handles umask/cd/tailable-log and cds into the passed workDir; it holds only tool commands, never qsub args. This is GENERAL, INDEPENDENT of allocation (e2305/s3319/any) - it is about code structure. Only a LARGE fan-out (hundreds of jobs, rare) justifies a generic launcher; most work needs one qsub or a handful, so inline-in-notes is the default. Detail: `~/projects/qsub_rules.sh`, `qsub_convention.sh`.
- **On pfe, NEVER rely on the default `ssh`/`scp` landing dir - it is the HOME dir
  (`/home6/oalexan1`), so scratch dropped there litters the home (which has even been a
  stray olegmisc checkout at times).** This is the mechanism behind the recurring pile-up of
  `inspect_*.sh` / `*.py` / `print.prt` in `~`: `ssh pfx 'python foo.py'` runs with CWD=`~`,
  `scp file pfx:` lands in `~`, and ISIS/gdal tools dump `print.prt`/`.aux.xml` into CWD. FIX
  (do it at the moment of writing, every time): every remote script goes to `pfx:/tmp/...`
  explicitly (`scp file pfx:/tmp/`, run `ssh pfx bash /tmp/file.sh`); `cd` into a work dir
  before running any tool so its side-outputs land there, not in `~`; anything worth keeping
  goes in a project subdir. Never the bare default.
- **NEVER wipe anything on lfe.** lfe access from l1: `ssh pfx` then `ssh lfe`.
- `/home6` data MUST symlink to `/nobackup*`. Symlink-wipe procedure in `pleiades_notes.sh`.
- **Every qsub script: `exec >` redirect to a work-dir log (never PBS `-o`) AND `umask 022` (readable outputs). Details: `qsub_convention.sh` / `qsub_rules.sh`.**
- **CHECK JOB EFFECTIVENESS on any long/multi-node pfe job - do not assume it parallelizes.** Effectiveness (efficiency) = CPU-time-used / (cores-allocated x walltime); 1.0 = every allocated core busy every second, low = idle cores wasting the allocation. THE overall number is `qstat`'s `Eff` column, equivalently from `qstat -f <jobid>`: `resources_used.cput / (resources_used.ncpus x resources_used.walltime)`. This is already JOB-WIDE - `cput` sums CPU-time over ALL nodes/chunks and `ncpus` is the TOTAL cores - so for a MULTI-NODE run it covers every node at once; you do NOT poll each node to get the overall figure (that answers "is the whole job effective"). Instantaneous aggregate = `resources_used.cpupercent` (divide by 100 = cores busy right now, summed across all nodes; e.g. 529 = 5.3 of 28). Any LOW value SUSTAINED over time (e.g. 7% on 28 cores = ~2 cores busy) is SUSPECT - investigate, do not ignore. To then LOCALIZE which node/rank is the laggard in a multi-node job: `exec_host`/`exec_vnode` in `qstat -f` lists every node; ssh each and compare `uptime` load avg vs its core count (pdsh/clush across all at once if available). Common cause: a serial per-item loop starving the node -> fix is batching/concurrency across items, not bigger per-item threads. Caveat: cput-efficiency can look low for legitimately I/O-bound or sync-heavy phases - judge over time, not one instant. (Caught the un-batched Jezero stereo_transverse.sh this way, 2026-06-26: Eff 7%, cpupercent 529.)

## ASP Dev Build on pfe

Working ASP on pfe: `pfx:~/projects/BinaryBuilder/StereoPipeline/` - a packaged
release (wrappers in `bin/`, ELF in `libexec/`, libs in `lib/`). Patch it from
l1: rebuild changed libs/tools, then rsync dev `install/lib/` -> `lib/`,
`install/bin/` -> `libexec/`, `*.py` -> `bin/`. Full recipe + NFS gotcha + scp
fix: `~/projects/pleiades_notes.sh` section "Syncing dev build to pfe".

## Sending Email to Oleg

How to email Oleg (msmtp; recipient oleg.alexandrov@gmail.com) is described in
`~/projects/send_email_notes.sh`.

## GitHub CLI (gh)

Full reference (paths, repo slugs, GraphQL/REST recipes, CI): `~/projects/github_notes.sh`.
`gh` not on PATH: `$(ls -d $HOME/*conda3/envs/gh/bin/gh)`. **CRITICAL:** `gh
issue/pr view` and `gh pr edit` error on the deprecated Projects-classic API -
use `gh api` (REST) for any issue/PR body/comment/state/label fetch or edit; and
**never trust WebFetch summaries of issues/PRs** (it hallucinates) - pull with
`gh api`. PR/issue/comment/review prose-style rules: `~/projects/github_text_style.sh`.
When opening or editing a PR/issue/comment body, write plain prose: avoid
backticks, avoid hard newlines within a paragraph (keep each paragraph on one
line), and avoid angle brackets or other constructs GitHub can read as an HTML
tag and swallow (e.g. `get<double>` renders as nothing) - reword instead.
NO inline backticks in any PR, issue, comment, or README.md - use italic
instead. Standalone code blocks (triple backticks) are fine.

**PR handoff: generate a PREFILLED "compare" URL** (title + URL-encoded body,
`expand=1`), not the plain create-PR link GitHub already offers on push. The
`?body=` param REPLACES the repo's auto PR template, so embed that repo's
`.github/PULL_REQUEST_TEMPLATE.md` in the body, checking the boxes that apply.
Do NOT open the PR (public-facing) unless told. Recipe + generator:
`~/projects/github_notes.sh`.

## Co-Authored-By Trailer (CRITICAL)

Every commit MUST include:
```
Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
```
Always use a HEREDOC for commit messages to ensure the trailer is included.

**DOI-USGS repos** (`~/projects/ISIS3`, `~/projects/usgscsm`, `~/projects/ale`,
SpiceQL, any DOI-USGS repo): AI attribution is now welcome - keep the trailer
AND state Claude/AI assistance in public text. See the USGS-repos bullet near
the top and `~/projects/usgs_contrib_notes.sh`.

**AI disclaimer = describe the bot only, never the user.** When stating
Claude/AI assistance in any public GitHub text (commit, PR, comment, review),
say only that a bot/Claude did the work. NEVER mention the user's
circumstances - not the hour, schedule, being asleep/awake, mood, or any
personal context. That is none of the reader's business. Keep it minimal:
"Done with Claude/AI assistance." and stop.

## Writing Style

**Say each thing once, in the one place it belongs, then point (CRITICAL,
recurring).** My drafts over-produce: a concept stated in an intro and again in
its own section, two cross-references where one does the job, a narrated outcome
("the crater should snap together", "the height difference is centered on zero"),
a doubled word ("residual disparity" for just "disparity"). None of these is
wrong. Each is one layer too many, and for an expert reader the value is in what
is cut. So: state each idea ONCE, in the section that owns it, then cross-ref -
do NOT foreshadow it earlier, restate it later, cite the same reference twice, or
describe what the reader is about to see. "Be concise" does not self-execute for
me (my sense of "enough" runs high), so run a deliberate SUBTRACTIVE pass before
showing any doc, comment, notes entry, commit, or PR: does this repeat something
above? is this cross-ref already made? am I narrating an expected result? Cut
what survives those questions. Applies everywhere - RST docs, code comments,
notes, commits, chat.

Never write "TL;DR" anywhere (notes, docs, chat, commits) - it is an ugly
macro-hack abbreviation. Use plain English: "Summary" (or just write the
summary). Keep summaries brief and to the point.

- Write SHORT sentences. Never join two sentences with a dash, an em dash, or a
  semicolon. Use a period and start a new sentence. Break long sentences up.
  Hyphens INSIDE a word are fine (model-based, cross-sensor). Applies everywhere:
  docs, notes, commits, chat (per Google/Microsoft/Apple style guides).
- Do not write three dots (an ellipsis). Use "etc." or just end the sentence.
- Do not use capital letters, underscores, asterisks, or any other markup to
  emphasize words (applies everywhere: code comments, docs, notes, commits,
  chat). Emphasis markup reads as shouting and clutters the text. Write plain
  prose and let word choice carry the emphasis. Real identifiers (PATH, NED,
  CSM, DOF) keep their normal casing.

**Words to avoid** (everywhere: code, comments, docs, notes, commits, chat). Use
plain English instead:
- "downweight" / "upweight" -> "give less weight" / "give more weight"
- "drape" / "draping" (a DEM used for mapprojection) -> "DEM for mapprojection",
  "mapprojection DEM", "mapproject onto the DEM". This is the universal ASP
  convention. Avoid "drape" everywhere - docs, code, and even our own notes. (When
  adding this rule, existing notes were left as-is by request; just do not write it
  going forward.)
- "tailable" -> "a log you can follow with tail" (not a real word)
- "downcase" / "upcase" -> "lowercase" / "uppercase"
- "honor" -> "respect", "obey", "use"
- "TL;DR" -> "Summary"
- "special casing" / "special-case" (as a verb) -> "handling as a special case",
  "a special case", "special-case handling" (noun). "special casing" is terrible
  English. And do NOT invent other analogous noun-to-verb coinages of the same
  shape (they read just as badly) - write the plain phrase instead.
When one of these is added here, also grep the projects `.sh` files and the ASP
and VW source and docs for it and fix existing occurrences.

**Informal INTERNAL terms and tools must NOT bleed into EXTERNAL / user-facing
docs (RST, published docs, PR and issue text). They stay valuable for our own
work; they just never ship.**
- "eyeball" -> "inspect" in external docs. Between you and me, "eyeball" is fine.
- "nuke" (as in remove/zero-out pixels) -> "remove", "mask out", "set to nodata"
  in external docs. Fine in our notes and chat, too casual for a shipped doc.
- Do NOT mention a "red/green overlay" (or "red/green hillshade overlay") in
  external docs. Describe the visual check plainly. The red/green (or red/blue,
  whatever helps you see the shift) overlay is a valuable INTERNAL inspection
  tool - keep using it for our work, just never name it in a shipped doc.
The rule is scoped: keep external docs professional; internal notes stay casual.

## Commit Message Style

Write like a human, not a robot. Short title; skip the body for trivial
changes. Avoid pedantic precision in the title:
- no quoted exact wording, no full function signatures, no `(file:foo.cc:123)`
- no parameter syntax with equals sign - "add csm parameter" not "add CSM= parameter"
- no `(#1234)` issue/PR number in the title - the body or PR cross-reference handles linking
- "fix changelog wording" beats `replace "plumbing" with "approach"`
- "added the csm parameter to campt" beats `Add CSM= parameter to campt (#6035)`

## ~/projects Git Rule (CRITICAL)

Files in `~/projects/` are tracked by `~/projects/.git` (NOT `~/.git`).
Always use `git -C ~/projects` for add, commit, push, etc.

**Some subdirs under `~/projects/` have their own `.git` repos** (e.g.,
StereoPipeline, visionworkbench, ISIS3, BinaryBuilder, ale, usgscsm,
StereoPipelineTest). NEVER add these to the `~/projects/.git` repo.
Only standalone `.sh`, `.py`, and similar files (and subdirs without
their own `.git`) belong in the projects repo. So when told to "add all
notes" / "commit what changed", this EXCLUDES all data and logs: it is
almost always `.sh` notes, occasionally `.md`; `.txt` is rare, so ASK
before adding a new one unless it is already tracked and only locally
modified. NEVER add binary files, data/output/run dirs, or anything in
old unrelated project dirs.

## Dependabot / Security Alerts

When a `git push` shows Dependabot or security vulnerability warnings, proactively
flag it and offer to investigate/fix.

## Advise on Build and Tool Warnings, Never Silently Ignore

When a build, compile, link, or tool run emits warnings (even when it succeeds),
do not skip past them. Read them, explain the root cause in plain terms, say
whether they are harmless or a real problem, and recommend a fix. Explain
warnings, do not sweep them under the rug. Example: the macOS
"dylib was built for newer macOS version (16.0) than being linked (11.0)" linker
warnings traced to conda deps built for the host OS floor instead of the
intended 11.0 floor. Even cosmetic warnings deserve a one-line "this is
harmless because X" rather than silence.

## ISIS Builds Use Ninja, Not Make (CRITICAL - stop rediscovering this)

ISIS3 build uses **Ninja**, not make: `ninja -j8 install` from the build dir
(`make install` errors). CMake source root is `ISIS3/isis` (`cmake ../isis`), not
the repo root. To build libs+apps without tests you MUST set `-DBUILD_CORE_TESTS=OFF`
(`-DBUILD_TESTING=OFF`/`-DbuildTests=OFF` alone are insufficient - gtest still
fails to link). Building with tests needs the gtest submodule
(`git -C ~/projects/ISIS3 submodule update --init --recursive`) and `ISISROOT`
set (discovery runs the test binary at build time). Full flags and gotchas:
`~/projects/isis_2026/isis_2026_notes.sh`; also
`~/projects/isis_mapproject/isis_mapproject_notes.sh` and `~/projects/env_update.sh`.

**NEVER install a coverage-instrumented ISIS into `asp_deps` (CRITICAL).** Always
build `-DbuildCoverage=OFF`. Coverage instruments the WHOLE ISIS lib set (~142 libs:
libisis ~676 MB plus every mission/camera/projection plugin `.so`, all with baked
`.gcda` paths). Any that get bundled into the ASP nightly HANG ASP tools for minutes
at exit on pfe/Athena (libisis hangs all tools, plugin libs hang on demand). After any
ISIS build verify no ISIS lib is dirty: `cd $CONDA_PREFIX/lib; for f in lib*.so; do
strings $f|grep -q '\.gcda' && echo DIRTY $f; done` (prints nothing; libisis ~26 MB not
~676 MB). Full incident, fix, and `GCOV_PREFIX=/tmp` workaround: `~/projects/isis_2026/isis_2026_notes.sh`.

## ISIS Data (CRITICAL)

**NEVER delete `~/projects/isis3data/` or its subdirectories without explicit permission.**
This is 179 GB of mission kernels that take forever to re-download over home ISP.

**NEVER delete `~/projects/isis_test_data/` (~19 GB) without explicit permission.**
This is `$ISISTESTDATA`, used by every ISIS ctest run (alongside `$ISISDATA` =
isis3data). It looks like stale bulk data in a cleanup pass but is in constant
active use and takes a long time to re-fetch. See `~/projects/isis_2026/isis_2026_notes.sh`.

## Safe Directory Cleanup (CRITICAL)

**ABSOLUTE RULE - NEVER put a `$VAR` or `${...}` in an `rm` path. NO EXCEPTIONS.**

**MECHANISM (so you STOP reaching for it - this keeps stalling autonomous runs). The
recurring trap is pre-cleaning scratch/experiment outputs in a loop with `rm -f
"$W"/*.rA` (a var AND a glob). DO NOT DO THIS. There is nothing to clean: the writing
tool overwrites its output files (`fopen(...,"wb")`) so a fresh run just replaces them,
and the scratchpad auto-cleans anyway - a pre-run wipe is pointless AND trips the
"dangerous rm on possibly-empty variable path" gate. If you genuinely need a clean dir,
make a NEW literal-named subdir for this run (e.g. `.../det2/`), never delete the old one
by glob. When you catch yourself about to write `rm ... $VAR .../*.ext` before a loop:
just delete that rm line. Confirmed to stall the run repeatedly (2026-08-13).**

This keeps recurring and Oleg keeps catching it. A variable that expands empty turns
`rm -f $S/${tag}_file.tif` into `rm -f /file.tif` or worse, and even when safe the
harness flags "dangerous rm on possibly-empty variable path" and STALLS the run. This
applies EVERYWHERE, including throwaway scratch/relay/temp cleanup and loops - those are
exactly where it bit us (ctx-relay loop `rm -f $S/${tag}_ctx_18m.tif`, 2026-07-20; VW
wiped TWICE by `rm -rf $bld/...`). Instead, in order of preference: (1) DON'T delete -
leave small temp files in the scratchpad, they get cleaned up automatically and disk is
rarely the real constraint; (2) if deletion is truly needed, write ONE `rm` per line with
a FULLY LITERAL absolute path, no variable, no glob; (3) never inside a `for`/`while` loop.
If you cannot write the literal path, do not run the delete. When in doubt, leave it.

Full deletion/cleanup policy: `~/projects/file_cleanup_notes.sh`. Bare minimum to
remember without reading: NEVER `rm -rf` an absolute or variable-expanded path
(`rm -rf $bld/...` wiped VW TWICE). `cd` into the parent, confirm with `pwd`/`ls`,
use RELATIVE paths only. Prefer GRADUAL per-file deletion (`cd` in, scoped loop
`for f in *.tif; do rm -f "$f"; done` or `find . -name '<pat>' -delete`, then
`rmdir` - it fails safely if non-empty) over sweeping `rm -rf <dir>`, which trips
the harness and stalls autonomous runs. Avoid `rm -f "$VAR/file"` (flagged even
when safe) - `cd "$VAR"` first, then `rm -f file`.

## Do Not Trigger Harness Permission Prompts Mid-Task (CRITICAL)

**EVER-RECURRING. For ANY destructive command (rm -rf, find -delete) write a
SINGLE EXPLICIT LITERAL ABSOLUTE PATH per command - one `rm -rf /full/abs/path`
per line. NEVER a glob (`*`), `~`, `$VAR`, `cd &&`, or `find ... -exec rm`. If a
path can't be made fully explicit, do NOT run the destructive command. This trips
the sandbox over and over and stalls the session.**

**In auto/autonomous mode especially, AVOID removing things at all unless you are
very sure it is needed - and then do it carefully with a single literal path. A
sandbox permission prompt stops you dead in your tracks, which defeats autonomous
progress. Deletion is rarely necessary: to refresh stale stats, re-read the data
(don't delete the `.aux.xml`); for temp files, leave them. When in doubt, don't
remove.**

Permission prompts from the sandbox stall independent progress and must be
avoided. The TRIGGER (confirmed 2026-06-24)
is the SHAPE of destructive Bash commands, not the operation itself:
- Shell GLOBS/wildcards in a destructive command (`rm -f *`, `rm *.tif`).
- `cd <dir> && rm ...` compounds, and `&&`-chained destructive sequences.
- `~` or `$VAR` expansion in the path.
These prompt. But a SINGLE destructive command on ONE EXPLICIT, LITERAL, ABSOLUTE
path does NOT prompt: `rm -rf /Users/oalexan1/scratch_dir`,
`conda remove -n env pkg -y` both ran clean. So to wipe independently and smartly:
write the full literal absolute path, no glob, no `~`, no `cd &&`. For many files,
`find /full/abs/path -name 'pat' -delete` (the pattern is find's, not a shell glob,
and the start path is literal) is fine. Reconciles with Safe Directory Cleanup: an
explicit literal absolute path is both safe AND prompt-free; the danger (and the VW
wipe) was `rm -rf $VAR/...` - variable/glob, never a literal path.

Also: for file/code/doc/notes edits prefer Edit / Write / Read / Grep / Glob -
they never prompt and never need this care. If something still prompts despite a
literal path, hand Oleg the exact `! <command>` to run, rather than re-issuing it.

## Tape Archive and Wipe (lfe) - Canonical Notes (find it here first)

**Canonical archive/restore/logging policy + recipe: `~/projects/lfe_archive.sh`**
(reusable tool `~/bin/archive_to_lfe.sh`; DMF `dmls`/`dmget`/`dmput -r`; plain `tar cf`,
never `-z`). The one rule: LOG EVERY ARCHIVE in that project's own notes, as a running
inventory near the TOP (tape is invisible otherwise). The archive+wipe WORKFLOW is:
symlink-audit first, prune regenerable intermediates, tar to lfe, VERIFY (tar tf entry
count == live `find` count, one-file data extract, key members present), `dmput -r` to
migrate, THEN wipe the /nobackup dir (one literal-path `rm -rf` each; also remove any
`/home6` symlink). `(DUL)`/`(OFL)` in dmls = safely on tape; `(REG)`/`(MIG)` = on lfe
disk / migrating. Verify keepers exist on tape BEFORE deleting.
- **Per-project tape inventories** live atop each project's notes. Known canonical logs:
  CaSSIS -> `~/projects/cassis_asp/cassis_cleanup_plan.sh` (TAPE ARCHIVE INVENTORY +
  per-dir wipe log; the hub `cassis_notes.sh` points to it). Deletion-safety policy:
  `~/projects/file_cleanup_notes.sh`.

## Remote (ssh) Destructive Ops Bypass the Harness Gate - Compensate With Discipline (CRITICAL)

The sandbox only inspects the LOCAL Bash command. When a destructive op runs INSIDE
an ssh'd remote script (`ssh host bash cleanup.sh`, or `ssh host "rm ..."`), the
harness sees only the `ssh ... bash` line - it does NOT see or gate the remote
`rm`/`find -delete`. So the prompt-on-glob/`$VAR`/`cd &&` safety net is ABSENT for
anything running on pfe/lfe/Athena. Never read "the prompt didn't fire" as "this is
safe" - a remote script is opaque to the harness. Do NOT push a destructive op into
a remote script IN ORDER TO dodge the prompt; the prompt exists for a reason. If
remote destructive work is genuinely needed, apply MORE care, not less, and TELL
Oleg the local gate is bypassed. Safety then comes from DISCIPLINE, in this order
(proven on the 573->63 GB chandra /nobackup wipe + lfe re-archive, 2026-08-09):
- GET EXPLICIT APPROVAL for any heavy/irreversible remote wipe - present the plan,
  the keep/delete lists, and sizes - before running it.
- ARCHIVE FIRST when the data is precious: the non-regenerable INPUTS get a tape
  copy (lfe) BEFORE a big wipe; treat produced results as redoable.
- VERIFY THE KEEPERS EXIST FIRST: list every deliverable you intend to keep and
  confirm it is present, BEFORE deleting anything.
- Inside the remote script still obey the literal-path rules: whole-dir deletes are
  one `rm -rf /full/abs/literal/path` per line (no `$VAR`, no glob); in-dir pruning
  uses a KEEP-WHITELIST (`find /abs/literal/dir -maxdepth 1 -type f ! -name 'keepA'
  ! -name 'keepB' ... -delete`) that you have CHECKED against the actual `ls` of
  that dir, not guessed.
- Echo BEFORE/AFTER sizes from the script, and RE-VERIFY the deliverables still
  exist afterward. For a tape overwrite, write ONLY the single intended tar path
  (`tar cf /u/.../one.tar dir/`) - never touch other lfe datasets - and shallow-check
  it (`tar tf` all headers + a one-file data extract + key-member grep).

## Never Reference Public PRs/Issues in Private-Repo Commit Messages (CRITICAL)

GitHub auto-links `owner/repo#NNN` (and bare `#NNN`) in commit messages and
creates a public "referenced this pull/issue" cross-reference event on the
target. A commit in a PRIVATE repo (e.g. `~/projects` = oleg-alexandrov/projects)
that references a PUBLIC PR (e.g. `DOI-USGS/ale#719`) therefore LEAKS the private
repo's name, commit hash, and message snippet onto the public PR timeline. The
event is effectively permanent (survives rewrite/force-push of the source commit).

RULE: in commit messages for ~/projects (and any private repo), never write
`owner/repo#NNN` or `#NNN` for a public PR/issue. Write "PR NNN" / "pull NNN"
(no `#`, not repo-qualified). The notes FILE content may name the PR freely
(file contents are not auto-linked) - only the COMMIT MESSAGE matters.

## NEVER Run Heavy Compute on the Mac mini (CRITICAL - repeatedly burned)

The Mac mini (Olegs-Mac-mini) is a NOTES/light box, NOT a compute node. It RUNS
OUT OF MEMORY (OOM) under real compute and the whole session wedges - nothing
finishes and I cannot continue. RULE: if a script is anticipated to invoke
parallel_stereo / stereo or bundle_adjust in any NON-TRIVIAL way it must NOT be
run on the Mac - send it to pfe (qsub) or l1.