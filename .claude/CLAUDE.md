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
  **No `timeout`/`gtimeout` on this Mac** - never wrap commands in `timeout` (it
  exits 127 "command not found", which silently looks like the wrapped command
  failed - cost a whole night of false "pfe down"). To bound an `ssh` probe use
  `ssh -o ConnectTimeout=N`. Detail: `~/projects/pleiades_notes.sh`.

- **Reachability check first (auto mode):** when a task depends on `l1` or
  `pfe`, probe them BEFORE committing to a plan (`ssh -o ConnectTimeout=8`).
  A dead host found mid-pipeline stalls an autonomous run. Cheap to test up front.

- **Athena / Turin** (another supercomputer, separate from Pleiades) - reach via
  **`ssh athfe01`** (..04); the hostname `athena` does NOT resolve. Model
  **`tur_ath`** (Turin, 256c), OWN scheduler, submit from athfe ONLY with
  **`/opt/pbs/bin/qsub`** (not `/PBS/bin/qsub`). `node_stats.sh` does NOT show the
  per-model Free table there - gauge load with `qstat`. `/home6` + `~/projects`
  symlinks ARE visible from athfe, so pfe-staged workers/data run as-is. Flaky;
  default to `bro_ele` unless tur_ath is explicitly wanted. Full detail +
  submit sample: `~/projects/pleiades_notes.sh` (athfe entry, "HOW TO FIND ATHENA").

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
- **When asked to evaluate FAILING regressions, FIRST `git fetch` + rebase the
  latest from the remote for BOTH VW and ASP** (`god/master`). The local source
  can be behind what the nightly built, so a local re-run silently uses stale
  libs and disagrees with the nightly. Rebuild+install the updated repo before
  concluding anything. (Burned 2026-07-06: local VW was 2 commits behind a
  ray-DEM intersection change, so local tests wrongly "passed".)
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
ostfl_2025_notes.sh): `~/projects/notes_conventions.sh`.

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
`csm_todo.sh` (CSM/ISIS work - the USGS PRs go here), `ostfl_todo.sh` (OSTFL),
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
not an afterthought. No need to note on-Mac vs on-pfe - that is figure-out-able. Each
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

## gdalwarp: Always -r cubicspline, Never the Default Nearest-Neighbor

Always run `gdalwarp` with `-r cubicspline`; never rely on its default nearest-neighbor resampling, which snaps and misregisters continuous rasters (DEMs, geodiffs, error fields) by up to half a pixel.

## point2dem --errorimage Always; Mosaic the Error Too

Every `point2dem` that makes a DEM gets `--errorimage` (the triangulation
IntersectionErr is a key diagnostic - distortion/misreg/blunders show there).
Whenever DEMs are `dem_mosaic`'d, ALSO mosaic the per-pair error images
(`dem_mosaic --max` over the `*-IntersectionErr.tif` -> a worst-case
tri-error mosaic). For an ALIGNED DEM, align the POINT CLOUD (it carries the
error in band 4) and `point2dem --errorimage` it, rather than aligning the
bare DEM (a rigid align repositions the error, doesn't change it).

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
      is content-free, points at the project notes, touches ~/.claude_heartbeat each
      firing, and advances the work. This is the normal pulse WHILE the harness is alive.
  (2) OS-LEVEL cron = emergency resurrector, on the local machine(s). This is the layer
      that survives an OUTAGE. It relaunches `claude -c -p` only when ~/.claude_heartbeat
      is stale (harness presumed dead), else stands down; atomic-lock guarded so runs
      never overlap; self-heals across a still-down service (cron keeps re-firing and
      catches the moment it returns).
  WHY BOTH (the thing I got wrong before): CronCreate is SESSION-ONLY - it lives inside
  the running Claude session and DIES WITH IT, so a "service unavailable" outage that
  kills the harness ALSO kills the CronCreate heartbeat and nothing re-arms it. Only an
  OS-level cron, independent of the harness, can bring Claude back. The old blanket "no
  OS-level crontab" rule predated this understanding and is RETIRED. OS cron is now
  REQUIRED for durable auto work, on LOCAL machines only, NEVER on pfe.
  Current implementation (Mac + l1): Mac `~/bin/claude_watchdog.sh` + crontab
  "9,24,39,54"; l1 backup `~/bin/claude_watchdog_l1.sh` (claude is not on l1, so it sshes
  to the Mac alias mac_arm and triggers the Mac watchdog) + crontab "12,42". Retire the
  watchdog only when the auto work is truly done: touch `~/projects/cassis_asp/.auto_done`
  or remove the crontab lines. LIMITATION - this one shared watchdog/heartbeat/sentinel
  assumes a SINGLE auto session; with 2+ bots it is lossy on death (a survivor keeps the
  heartbeat fresh so a dead bot is never resurrected; first `.auto_done` disarms everyone).
  Fix = per-bot state (not yet done). Detail: `~/projects/claude_overnight_notes.sh`.
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

## No Hardcoded Values or Env Vars in Scripts

Scripts must take ALL parameters as explicit input args - no hardcoded values, no
env vars, no default args. Hidden config can't be inspected when re-running the
script later.
Before running a script that is a notable stage of something, define all vars, 
such as sigma=10. etc. Have rationale. Log all this rationale, var names and vals, and
precise stage actual script invocation including the qsub cmd for reproductibilty later.
So basically a premable with all defined followed by precise invocation you will launch.

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

## Multi-Option Commands in Scripts

In shell scripts, put each command-line option on its own line, WITH ITS VALUE
on that same line: `--option val \`. One option per line, never several options
on one line, and never split an option from its value. Same for each `export`.
Use trailing `\` continuation backslashes (single space before the `\`, matching
the surrounding scripts; or align to one column with the backslash alignment tool
below where that reads tidier).

**Comment lines in scripts never exceed 90 characters.** Wrap a longer comment
onto continuation comment lines. Measure line length with a tool (e.g. `awk
'{if(length($0)>90)print NR,length($0)}'`), never eyeball it.

**NEVER put a comment after a `\` line-continuation** (`cmd \  # note`): the `\`
escapes the trailing space, the `#...` is a comment, and the command ENDS there
(continuation broken). This applies to scripts AND to paste-able commands shown
to Oleg. Keep comments on their own lines, or omit them.

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

## Visual Raster Inspection - "Claude has eyes"

Claude can SEE images - use vision to verify rasters (orthos, DEMs, geodiffs,
camera/rotation alignment). Technique, the warp-to-a-common-grid-before-overlay
rule, and where preview files live (with the data on pfe, not /tmp):
`~/projects/visual_raster_inspection.sh`.
**CARDINAL RULE (do not repeat the Oxia screwup): you are a PIXEL PNG viewer,
NOT GIS - you CANNOT eyeball a georeferenced overlay; gdalwarp ALL rasters to ONE
identical grid (-t_srs + -te + -ts) -> PNG, THEN look (a side-by-side at different
framing is worthless). And geodiff std is BLIND to HORIZONTAL misregistration on
low-relief terrain - a small geodiff std does NOT mean registered; judge
registration ONLY by the red/green hillshade overlay. Full rule + failure record
at the top of that file.**

Match-point inspection: `~/bin/plot_matches.py` overlays an ASP .match file on both images and reports the residual to the best-fit translation (the real-vs-junk metric for co-registered pairs). For the stereo_gui solid-red-dot look use `--red --radius N`.

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
- **No heavy/parallel compute on the pfe head node** - `parallel_stereo`, anything multi-process/multi-thread or big-RAM goes to qsub (small/fast -> `devel`). Light single-thread `gdalinfo`/`gdal_translate`/`gdalwarp`/`ls`/`qstat` on the head node is fine (don't qsub a one-off gdalinfo - use common sense). **To run any gdal/ASP tool on pfe FIRST set the env** (non-interactive ssh has nothing on PATH, PROJ unset): `conda activate asp_deps` (PROJ data, so `-t_srs` works) + `export PATH=$HOME/projects/BinaryBuilder/StereoPipeline/bin:$PATH` (the packaged build has ALL tools); detail in `~/projects/pleiades_notes.sh`. **NEVER run heavy compute - `stereo_corr`/`parallel_stereo`/correlation (the eval dd) - on the Mac OR the pfe head node; it goes to a qsub compute node.** The eval (`cassis_eval_stage.sh`) is the last step inside each stage's qsub job, so its dd runs on the compute node - do not run it by hand on the Mac. 4-sec dry-run before qsub. budget `e2305`. **Models & node choice:** `cas_ait` (40c, Aitken), `rom_ait`/`mil_ait` (128c, Aitken), `bro_ele` (28c, Electra), `sky_ele` (40c, Electra). Broadwell is decommissioned ONLY on Pleiades - `bro_ele` (Electra) and `sky_ele` are FINE to use. **Our code is model-agnostic - it must run on ANY of them** (match `ncpus` to that model's cores). **Before launching, study load on ALL systems** (`/u/scicon/tools/bin/node_stats.sh` -> Free vs "Queued jobs want N nodes" per model) and pick the LEAST-CONTENDED (e.g. bro_ele was Free 292 / 12 queued while cas_ait was 379 queued). **For small single-node jobs Athena (Turin) nodes are also fine - but Athena Turin is only visible/submittable from the ATHENA front-end** (ssh to athena), NOT from pfe. **If a job sits queued too long, qdel it and resubmit on a less-contended system.** In a non-interactive ssh, qsub is not on PATH - use `/PBS/bin/qsub`. `devel` allows only 1 job/user (pack multiple sites into ONE serial job). POLICY: NO separate PBS launcher script (cannot afford one per stage). The PLAN/NOTES file holds the COMPLETE, LITERAL, copy-pasteable qsub command line (the full `qsub <all pbs args> -- $dir/script.sh <all script args, workDir LAST>` as ONE reproducible string - NOT just the args/params/job-id logged piecemeal), WITH its rationale + named params, logged BEFORE the launch. Then launch the worker DIRECTLY via that qsub `--` form. The worker self-handles umask/cd/tailable-log and cds into the passed workDir; it holds only tool commands, never qsub args. This is GENERAL, INDEPENDENT of allocation (e2305/s3319/any) - it is about code structure. Only a LARGE fan-out (hundreds of jobs, rare) justifies a generic launcher; most work needs one qsub or a handful, so inline-in-notes is the default. Detail: `~/projects/qsub_rules.sh`, `qsub_convention.sh`.
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
- "tailable" -> "a log you can follow with tail" (not a real word)
- "downcase" / "upcase" -> "lowercase" / "uppercase"
- "honor" -> "respect", "obey", "use"
- "TL;DR" -> "Summary"
When one of these is added here, also grep the projects `.sh` files and the ASP
and VW source and docs for it and fix existing occurrences.

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