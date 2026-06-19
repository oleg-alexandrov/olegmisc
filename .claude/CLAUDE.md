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

## git rm --cached, never bare git rm (CRITICAL)

Never add `.ssh/` to git (dangerous). To untrack a file but keep it on disk, always `git rm --cached`, never bare `git rm` (which deletes the working file too - this once wiped `~/.ssh/config`; recover via `git show <commit>^:path > path`).

## NEVER `git add .` / `-A` in the home repo - add NAMED files only (CRITICAL)

The home dir (`~`, repo = olegmisc) working tree holds private files (`.ssh/`,
`.claude/.credentials.json`, `.bash_history`, ...). `git add .`/`-A`/`-u`/a dir
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
  `vendor`, `deband`, `dem2gcp`, `ba_htdem`, `ba_nodem`, ... with `_vN` for
  iterations (`ba_htdem_v2`). NEVER use vague tags like `before`/`after`/`new`/`tmp`.
- `<product>` chains left to right as products build on each other:
  `dem` -> `hs` -> `<ref>diff` (e.g. `ctxdiff`) -> `<ref>diff_cmap`. A derived
  product borrows its parent's name and just extends it (the `.png` viewer copy
  keeps the same basename as its `.tif`).
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
- For handling external bump-up PRs on the VW conda-forge feedstock
  (`conda-forge/visionworkbench-feedstock`), see
  `~/projects/vw_conda_forge_bump.sh`. Covers the alpha → point release →
  patch bot's PR branch → merge → restore alpha procedure.

## Nightly Build and Regression Tests

Cron job on lunokhod1 at 23:05 runs the full build/test/release pipeline
for Linux (local) + macOS x64/arm64 (GitHub Actions). Full reference:
`~/projects/nightly_regression.sh`. Key files in
`~/projects/BinaryBuilder/auto_build/` (launch_master.sh, build.sh,
run_tests.sh, utils.sh). Email via msmtp on completion. Mac CI gold
updates: `~/projects/update_cloud_tests.sh`.

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

Per-machine build commands, conda init, paths, the athfe tunnel hop, `/tmp`
triage: `~/projects/machines.sh` (and `install_asp_notes.sh`).

## Common Aliases

Full list in `~/.bash_aliases` - check there if an unfamiliar short command shows up in logs or notes.
- `sg` = `stereo_gui --window-size 1500 1000 --font-size 12` (view images/DEMs)
- `swa` = `sg -w --hide-all` (single-window overlay, start hidden)

## Running Tests

**Suite:** `~/projects/StereoPipelineTest`. Full guide - layout, **the CRITICAL
env setup** (conda + ISISROOT + PATH, else parallel_stereo/validate.sh crash),
tolerances, triage, gold regen, finding test dirs by tool:
`~/projects/asp_regression_tests.sh`. Mac CI: `~/projects/update_cloud_tests.sh`.
Run a test: `cd` in, `bash run.sh > output.txt 2>&1`, then `bash validate.sh`
(exit 0 = pass). NOT pytest.
- **MANDATORY: run regression tests after every ASP code change** - find ALL
  matching dirs (`grep -rl <tool> ~/projects/StereoPipelineTest/ss*/run.sh`) and
  run them all, not just one; flag if a changed path has no test coverage.
- **NEVER git add `run/` or `gold/`** (~40 GB, gitignored); only `run.sh` /
  `validate.sh` are tracked. `chmod +x` new ones.

## Notes & Paper Trail (CRITICAL)

Keep a per-project notes `.sh` in `~/projects/<subdir>/` and log to it as you go
- plan/approach/why up front, findings and surprises during, what worked/didn't
after. Don't rely on memory; this survives context compaction. Make work
REPRODUCIBLE (record exact commands/invocations so results can be redone) and
log the screw-ups and bad judgment too, not just the wins. Notes `.sh` are
comment-only - never `chmod +x`; `git -C ~/projects add` new files in subdirs.
Full conventions + the work-tracking file index (mpr_todo.sh, todo.sh,
ostfl_2025_notes.sh): `~/projects/notes_conventions.sh`.

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

## CMake File Management

**Touch CMakeLists.txt ONLY when file listing changes (add/remove/move files):**

When adding/removing/moving source files (.cc, .h):
- Touch the CMakeLists.txt in that directory AND the parent directory
- This triggers CMake to re-run `file(GLOB ...)` and pick up changes

**DO NOT touch when just editing existing files** - build system detects content changes automatically.

## Inspect BA/Jitter Stats After Every Run

After any bundle_adjust or jitter_solve run, inspect the residual/stats output
files (initial AND final per-camera residual stats, convergence_angles,
camera_offsets, triangulation_offsets, pointmap) - listed in the output-files
section of the bundle_adjust and jitter_solve RST docs. Judge by the MEDIAN (the
mean is outlier-driven). Skip the per-residual raw_pixels files (too big). These
tell you whether the solve behaved (sub-pixel medians, bounded offsets, cameras
multiply-tied).

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
- Working alone, take initiative on simple fixes (symlink, missing lib, resubmit
  failed job, clean stale files); test small first; log what you did. No sweeping
  refactors, no external commits unprompted.
- ALWAYS keep your OWN ScheduleWakeup timer armed - every single turn, no
  exceptions - and NEVER count on being woken by a task-completion notification
  (it can be missed and you "fall asleep"). Treat the completion notifier as a
  bonus, the timer as the real wake signal. Whenever you start anything you then
  wait on (qsub, build, big rsync, cloud CI, backgrounded poll), set the timer
  IMMEDIATELY and RESCHEDULE it each fire until the thing is truly done. Interval
  tuned to the cache window: ~270s for cloud CI you're iterating on, 5 min for a
  build, 15-30 min for a long stereo/PBS run.
- On every wakeup, FIRST run `date` to re-orient - long runs leave you stale.

## Building ASP Docs

```bash
eval "$($HOME/anaconda3/bin/conda shell.zsh hook)"
conda activate sphinx
make -C ~/projects/StereoPipeline/docs html
# Output: docs/_build/html/
```

## RST Documentation Formatting

**Documentation file locations:** check both `docs/` subdirectories and repository root level.

**Style:** Be concise - users are expert researchers. Give hints and pointers, not tutorials.

**Formatting rules:**
- Section underlines must be exactly the same length as heading text
  - **CRITICAL: Always count characters carefully - prone to off-by-one errors**
- Heading levels: `=` top, `-` subsection, `~` sub-sub, `^` sub-sub-sub
- For `:ref:` links where text matches target, use simplified syntax: `` :ref:`tool_name` ``

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

## Multi-Option Commands in Scripts

In shell scripts, put each command-line option (and each `export`) on its own
line for readability, with trailing `\` continuation backslashes aligned to one
column (use the backslash alignment tool below).

## Backslash Alignment Tool

`~/bin/align_backslashes.py <file> <start_line> <end_line> [--inplace] [--column N]`
Aligns trailing `\` continuation characters in shell scripts. Auto-detects
target column from longest content line, or use `--column N` to fix it.

## Column Alignment Tool

`~/bin/align_columns.py <file> <start_line> <end_line> [--inplace]`
Aligns columns in a range of lines. Detects columns by 2+ space gaps.
Lines are 1-based. Without `--inplace`, prints aligned output to stdout.

## Stereo/Photogrammetry Resolution (CRITICAL - screwed this up MULTIPLE TIMES)

For stereo/photogrammetry, ALWAYS mapproject (and correlate) at near-native
image resolution, with ONE pinned `--tr` so every image (left, right, all)
shares the SAME GSD - auto (no `--tr`) drifts per image and parallel_stereo
correlator-mode then errors on mismatched GSD. The DEM is only an interpolated
draping surface, usually ~4x coarser; its coarseness must NEVER set the
mapproject/correlation grid. Only the OUTPUT DEM (point2dem) lives at the coarse
~4x-GSD res. Do not downsample imagery to the DEM. (CaSSIS native GSD ~4.59 m;
DEM ~18 m.) Repeatedly assumed the DEM res sets the mapproject res - it does NOT.

## Relative Paths in a Project Work Dir

In a project work dir, all paths (in scripts and when presenting to the user)
must be RELATIVE to that work dir. Use absolute paths only for external files
outside it.

## Visual Raster Inspection - "Claude has eyes"

Claude can SEE images - use vision to verify rasters (orthos, DEMs, geodiffs,
camera/rotation alignment). Technique, the warp-to-a-common-grid-before-overlay
rule, and where preview files live (with the data on pfe, not /tmp):
`~/projects/visual_raster_inspection.sh`.

Match-point inspection: `~/bin/plot_matches.py` overlays an ASP .match file on both images and reports the residual to the best-fit translation (the real-vs-junk metric for co-registered pairs). For the stereo_gui solid-red-dot look use `--red --radius N`.

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
- **4-sec dry-run before every qsub.** Default `bro_ele`, budget `e2305`. Never head-node compute.
- **NEVER wipe anything on lfe.** lfe access from l1: `ssh pfx` then `ssh lfe`.
- `/home6` data MUST symlink to `/nobackup*`. Symlink-wipe procedure in `pleiades_notes.sh`.

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

Never write "TL;DR" anywhere (notes, docs, chat, commits) - it is an ugly
macro-hack abbreviation. Use plain English: "Summary" (or just write the
summary). Keep summaries brief and to the point.

- Avoid semicolons and sentence-joining dashes, especially with full sentences. Use a period and short sentences (per Google/Microsoft/Apple style guides).

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
their own `.git`) belong in the projects repo.

## Dependabot / Security Alerts

When a `git push` shows Dependabot or security vulnerability warnings, proactively
flag it and offer to investigate/fix.

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

## ISIS Data (CRITICAL)

**NEVER delete `~/projects/isis3data/` or its subdirectories without explicit permission.**
This is 179 GB of mission kernels that take forever to re-download over home ISP.

**NEVER delete `~/projects/isis_test_data/` (~19 GB) without explicit permission.**
This is `$ISISTESTDATA`, used by every ISIS ctest run (alongside `$ISISDATA` =
isis3data). It looks like stale bulk data in a cleanup pass but is in constant
active use and takes a long time to re-fetch. See `~/projects/isis_2026/isis_2026_notes.sh`.

## Safe Directory Cleanup (CRITICAL)

**NEVER run `rm -rf` with absolute or variable-expanded paths to clean build
dirs** (`rm -rf $bld/...` wipes `/` if `$bld` is empty - VW was wiped TWICE this
way). Instead: (1) `cd` into the project dir, (2) `ls` to confirm you're there,
(3) use **relative paths only** (`rm -rf ./build_linux`, never `rm -rf $bld`).

## Cross-Compile Build Directories (CRITICAL)

**Native builds use `build/` and `install/`. Cross-compile uses `build_linux/` and `install_linux/`.**

Both VW and ASP follow this convention:
- `build/` + `install/` = native Mac ARM64 (or native Linux on lunokhod1)
- `build_linux/` + `install_linux/` = cross-compiled Linux x86_64 from Mac

NEVER use `build/` or `install/` for cross-compilation. NEVER use `build_linux/` or
`install_linux/` for native builds. Mixing these up destroys the other build.

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
