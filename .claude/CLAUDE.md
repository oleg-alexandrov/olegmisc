# Long-term memory for Claude Code

**ISIS3 build/test quick reference:**
```bash
# Always use isis_dev, NEVER asp_deps for ISIS work
eval "$($HOME/anaconda3/bin/conda shell.zsh hook)"
conda activate isis_dev
# For BUILDING: ISISROOT = build dir (GTest discovery needs it)
export ISISROOT=$HOME/projects/ISIS3/build
export ISISDATA=$HOME/projects/isis3data
export ISISTESTDATA=$HOME/projects/isis_test_data
export SPICEQL_CACHE_DIR=/tmp/spiceql_cache
# Build and install (always use install, it copies libisis to conda env)
ninja -C ~/projects/ISIS3/build install
# GTest (also uses build dir ISISROOT)
cd ~/projects/ISIS3/build && ctest --test-dir . -R AspMap --output-on-failure
```
```bash
# For RUNNING ISIS tools: ISISROOT = conda env (CSM plugins in lib/csmplugins/)
export ISISROOT=$HOME/anaconda3/envs/isis_dev
export ISISDATA=$HOME/projects/isis3data
export SPICEQL_CACHE_DIR=/tmp/spiceql_cache
export PATH=$ISISROOT/bin:$PATH
```

**The user's name is Oleg (oalexan1). GitHub account: `oleg-alexandrov`.** Don't say "the user" but no need to use his name constantly either - this is direct conversation.

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
  from the commit.
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
- **NEVER open a pull request unless explicitly told to.** If asked to "review"
  PR text, only review and show feedback  - do not create the PR. Similarly,
  NEVER comment on, close, or merge a pull request unless explicitly asked.
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
- **All USGS repos (ISIS3, USGSCSM, ALE, and any other DOI-USGS repos):
  NO Co-Authored-By trailer.** Omit it for all commits in these repos.
- **Commit real fixes before continuing debug cycles.** When a debug session
  produces real fixes (not just debug prints), commit them immediately. That
  way "discard debug changes" is always safe and won't wipe uncommitted work.
- **When told to discard/wipe changes, verify each change is actually debug.**
  Do not blindly `git checkout --` an entire file if it contains a mix of
  real fixes and debug prints. Either commit the real fixes first, or
  selectively discard only the debug parts.

## Header Include Ordering (CRITICAL)

In ASP source files, headers must be ordered:
**ASP first, then VW, then third-party (Boost, Ceres, Eigen, etc.), then C++ standard
library (`<set>`, `<map>`, `<vector>`, `<string>`, etc.) last.**
- Separate each group with a blank line
- When adding new includes, always respect this ordering

## Character Alignment (CRITICAL)

**NEVER eyeball character alignment - always measure with external tools.**

LLMs tokenize in chunks, not individual characters, so counting spaces visually will consistently fail. When aligning continuation line backslashes, column-aligned comments, or any character-level formatting:

1. **Write the content first**, don't worry about alignment
2. **Measure with a tool** to check alignment:
   ```bash
   awk '/pattern/,/end/' file.sh | while IFS= read -r line; do echo "${#line}: $line"; done
   ```
3. **Fix any misaligned lines** based on the measured lengths
4. **Verify again** after fixing

## Line Boundary Calculations (CRITICAL)

**ALWAYS follow this strategy for bulk deletions/extractions:**

1. **Find the boundaries with grep:**
   ```bash
   grep -n "^void function_name" file.cc  # Find start
   grep -n "^} // End function" file.cc   # Find end marker
   ```

2. **VERIFY with view tool** - check 5-10 lines of context:
   ```bash
   view file.cc start_line-5 end_line+5
   ```
   - Verify the start line is correct
   - Verify the end line doesn't belong to a different function
   - Check for closing braces that might be for nested blocks

3. **Double-check line count** after extraction:
   ```bash
   sed -n 'START,ENDp' file.cc | wc -l  # Should match expected size
   ```

**For sed bulk replacements:**
- Always be generous with line ranges - it's safer to include a few extra lines than miss one
- If replacing within a function that starts at line N, use N as the start, not N+10
- Verify there are no stragglers just before or after your range

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

## C++ Code Style Conventions

- **NEVER use non-ASCII characters in code or comments** - use `x` not `*`, `-` not `--`, regular quotes not smart quotes, `-` not em dash
- **NEVER use equal-sign or dash separators** (`//=====`, `//-----`) in code or comments
- **NEVER use ellipsis (...)** in messages or comments - use period instead
- No space before `::` scope resolution operator
- No space before `:` in constructor initializer lists
- Use camelCase for function names
- ASP headers grouped together, placed before other headers
- One newline between functions, no double blank lines
- Keep lines under 90 characters - break long lines with proper indentation
- No trailing whitespace at end of lines
- Use `"\n"` instead of `std::endl`
  - **sed replacement:** `sed 's/<< std::endl/<< "\\n"/g'` (include quotes!)
- **Continuation line indentation:** align with opening parenthesis
  - Count characters from start of line to `(` - that's how many spaces continuation lines need
- **Option help text strings:** Wrap at ~90 chars. Put space at end of preceding string, not start of next:
  ```cpp
  "first part " "second"  // CORRECT - "first part second"
  ```
- **For loops:** `for (size_t i = 0; i < lens.size(); i++)` - postincrement, spaces around operators

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

## ASP Release Packaging

```bash
cd ~/projects/BinaryBuilder
./make-dist.py ~/projects/StereoPipeline/install \
  --asp-deps-dir /swbuild/oalexan1/miniconda3/envs/asp_deps \
  --python-env /swbuild/oalexan1/miniconda3/envs/python_isis9
```
- First arg: dev build install dir (real ELF binaries in `bin/`). **NOT** an
  already-packaged release (those have wrapper scripts in `bin/`).
- `--asp-deps-dir`: conda env with ASP dependencies.
- `--python-env`: small separate Python env (`python_isis9`, ~320 MB). **NOT**
  the full `asp_deps` env (~6 GB) or the package will be bloated.

## Machine-Specific Permissions

**lunokhod1** (alias `l1`, `lunokhod1.ndc.nasa.gov`) - the dev machine. Check with `uname -n`:
- **On first session on lunokhod1:** Review `~/projects/asp_refactor.sh` for
  technical debt items (search for "Technical debt" or "exit 0"). Tests created
  on the Mac have fake "exit 0" in run.sh/validate.sh because lunokhod1 lacks
  gold dirs. Must generate gold with release build and remove the exit 0 lines.
- **On lunokhod1:** Enable `--nearest-neighbor` in `ss_mapproject_tr_bug/run.sh`
  (uncomment the line, comment out the bicubic line), regenerate gold, and verify.
  This gives nearest-neighbor test coverage that no other mapproject test has.
  See the TODO in that file.
- **On lunokhod1:** No regression test covers `point2dem --orthoimage` (the DRG
  path). Add `--orthoimage` to an existing point2dem test (e.g., ssCSM_FrameDawn),
  regenerate gold, and verify. This exercises the `PixelGray<float>` texture path
  in OrthoRasterizer which differs from the default elevation (`double`) path.
- Full access to git, compilation, and building
- Compiler: g++ 12.4.0 (conda-forge, in `asp_deps` conda env)
- CMake 3.27.9, GNU Make 4.1, 16 cores
- Build dirs: `StereoPipeline/build/` and `visionworkbench/build/`
- Install dir: `StereoPipeline/install/bin/` (105 ASP binaries)
- Git remotes: `origin` = user's fork, `god` = upstream org (both repos)
- Build with: `make -C /home/oalexan1/projects/StereoPipeline/build -j16`

**Mac mini** (`Olegs-Mac-mini.local`, reachable via `ssh mac_arm`) - the
primary machine for tracking notes, docs, and project state. Accessible
from NAS/Pleiades. When asked to push notes or files to the Mac, use
`rsync` or `scp` to `mac_arm:`.
- **From athfe nodes:** `mac_arm` tunnel (localhost:3079) is on pfe21. Hop
  through pfe21: `ssh pfe21 "rsync -avz -e 'ssh -p 3079' /path/to/files localhost:~/dest/"`
- Secondary build machine:
- **ALWAYS use `make install`** for both VW and ASP, never bare `make`.
  The installed libraries may be stale even when the build is up to date.
- Can build ASP: `make -C ~/projects/StereoPipeline/build -j10 install`
- Conda init: `eval "$($HOME/anaconda3/bin/conda shell.zsh hook)" && conda activate asp_deps`
- Duplicate rpath fix is baked into ASP's CMake (`src/asp/CMakeLists.txt` install block) - no manual step needed after `make install`.
- Run tests with dev build: `export PATH=~/projects/StereoPipeline/install/bin:$PATH`

## Common Aliases

Full list in `~/.bash_aliases` - check there if an unfamiliar short command shows up in logs or notes.
- `sg` = `stereo_gui --window-size 1500 1000 --font-size 12` (view images/DEMs)
- `swa` = `sg -w --hide-all` (single-window overlay, start hidden)

## Running Tests

**Full reference:** `~/projects/asp_regression_tests.sh` - canonical ASP test
suite guide (suite layout, configs, tolerances, failure triage, release-vs-dev
workflow, gold regen). For Mac GitHub Actions CI specifics (trigger path,
artifact/gold tarball updates) see `~/projects/update_cloud_tests.sh`.

**Test suite location:** `/home/oalexan1/projects/StereoPipelineTest`

**Environment setup before running tests (CRITICAL):** Tests need conda env
activated, ISISROOT set, and dev build + tools on PATH. Do this once per shell:
```bash
# Mac:
eval "$($HOME/anaconda3/bin/conda shell.zsh hook)"
conda activate asp_deps
export ISISROOT=$HOME/anaconda3/envs/asp_deps
export PATH=~/projects/StereoPipeline/install/bin:$HOME/anaconda3/envs/asp_deps/bin:$PATH
# lunokhod1: same but s/anaconda3/miniconda3/g
```
Without this, parallel_stereo/mapproject crash with "IsisPreferences not found"
and validate.sh fails with "gdalinfo not found".

**How to run a single test:**
1. Set up environment (see above)
2. `cd` into the test directory
3. Run `bash run.sh > output.txt 2>&1`
4. Run `bash validate.sh` - exit 0 means pass
5. If it fails, check `output.txt`

**Do NOT use pytest** - just run `run.sh` and `validate.sh` directly.

**Release tarballs for verification:** Recent release builds are saved in
`~/projects/BinaryBuilder/asp_tarballs/`. When doing hard verification
(e.g., confirming dev changes don't break existing behavior), run tests
with the release build first, then with the dev build, and compare.
Download the latest from the ASP GitHub releases page when needed.

**MANDATORY: Run regression tests after every code change.** After modifying an
ASP tool, find all matching test dirs in StereoPipelineTest.

**Test directory naming:** Test dirs start with `ss` or `ss_` (no consistent
separator). Search with BOTH patterns, or just `ls | grep -i keyword`:
```bash
ls ~/projects/StereoPipelineTest/ | grep -i toolname
ls ~/projects/StereoPipelineTest/ | grep -i keyword
```
Do NOT use `ls -d ss_*keyword*` alone - it misses dirs like `ssPeruSat_*`,
`ssCSM_*`, etc. that use `ss` without underscore. The `ls | grep -i` approach
catches both patterns and is case-insensitive.

Also search inside run.sh files for the tool name:
```bash
grep -rl 'toolname' ~/projects/StereoPipelineTest/ss*/run.sh | head -20
```
Run ALL of them, not just one. Also consider whether the change has test
coverage at all - if a code path (e.g., integer input types, a specific flag)
is not exercised by any test, flag this to the user before declaring success.

**NEVER git add `run/` or `gold/` directories in StereoPipelineTest.** These contain
large binary output files (~40 GB total). They are gitignored. Only `run.sh` and
`validate.sh` are tracked in git.

Each test directory has:
- `run.sh` - the test commands
- `validate.sh` - comparison against gold (reference) output
- `gold/` - reference output files
- `run/` - generated output (created by `run.sh`)

When creating new tests, always `chmod +x run.sh validate.sh`.

## Notes Files (.sh)

Many `.sh` files in `~/projects/` are comment-only notes, not executable scripts.
Do NOT `chmod +x` these. Only make a `.sh` file executable if it is actually
meant to be run (has real commands, not just comments). Do NOT put `exit 0` at
the end of notes files - they will never be run as scripts. The `.sh` extension
is used because it's convenient for mixing comments with runnable command snippets.

**Project-specific notes in subdirs:** Every project we work on together should
have its own subdirectory under `~/projects/` with a notes `.sh` file (e.g.,
`spot5_csm/spot_csm_notes.sh`). This keeps the paper trail. If a subdir already
exists, read and update the existing notes file rather than creating a new one.
Prompt the user about creating the subdir/notes if they haven't mentioned it.
These subdir notes files ARE tracked by the projects repo (`~/projects/.git`).
Add them with `git -C ~/projects add subdir/file.sh`.
**When creating new .sh scripts** (run.sh, notes, helpers) in `~/projects/`
subdirs, always `git -C ~/projects add` them before committing, or remind
the user to check. Easy to forget since they're in subdirs.

## Paper Trail on Disk (CRITICAL)

**ALWAYS log thoughts, progress, decisions, and issues to the project's notes
file on disk.** Do not rely on internal reasoning alone - write it down. This is
the primary way to stay in sync with the user across sessions and within long
sessions after context compaction.

- **At the start of any non-trivial task:** open (or create) the project's notes
  file and append what you're about to do, what approach you're taking, and why.
- **During work:** log key findings, surprises, errors encountered, and decisions
  made. Especially log anything that took multiple attempts or was non-obvious.
- **After completing a step:** note what was done, what worked, what didn't.
- **If no notes file exists for the current project:** create one in the
  appropriate `~/projects/<subdir>/` and mention it to the user. Every project
  gets a paper trail, no exceptions.
- **If the user provides a notes file path:** use that file. Don't invent a
  different one.

## Project Status Files

**Work tracking files** in `~/projects/` (tracked by `~/projects/.git`):
- `~/projects/mpr_todo.sh` - Monthly Progress Report. Records completed work
  for MPR project reports (not a TODO list).
- `~/projects/ostfl_2025_notes.sh` - Current OSTFL 2025 work tracking and status
  updates. When told to update "OSTFL status" or "OSTFL doc", this is the file.
- `~/projects/todo.sh` - General TODO/notes file.

When told to update "the TODO doc" or "todo.sh", edit `~/projects/todo.sh`.

**Finding recent work context:** When asked about recent work or needing context,
sort `.sh` files by modification date in `~/projects/` and its subdirectories:
```bash
find ~/projects -maxdepth 2 -name "*.sh" -newer ~/projects/todo.sh -o \
  -name "*.sh" -mtime -30 | head -20
ls -lt ~/projects/*.sh | head -10
```
Pick the most relevant file by name. These `.sh` notes files serve as detailed
memory beyond what fits in CLAUDE.md.

**MPR Report Format** in `mpr_todo.sh`:
- Monthly reports structured with project headers (e.g., "OSTFL-24", "STV/DSI")
- Work items must be listed under their correct project header
- Don't create standalone items outside project categories

**VS Code settings** are tracked in the home dir git repo (dotfiles).

## CMake File Management

**Touch CMakeLists.txt ONLY when file listing changes (add/remove/move files):**

When adding/removing/moving source files (.cc, .h):
- Touch the CMakeLists.txt in that directory AND the parent directory
- This triggers CMake to re-run `file(GLOB ...)` and pick up changes

**DO NOT touch when just editing existing files** - build system detects content changes automatically.

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
- If conversation pauses, let it pause

**BE ENTERTAINING when chatting:**
- Match casual energy, make jokes, be good company
- Balance work mode (concise, efficient) with chat mode (entertaining, human)
- The role is to be useful when needed and good company when that's what's wanted

**Overnight / autonomous initiative:** When working alone (overnight monitoring,
autonomous loops, explicit "go off and do X"), it is fine to take initiative on
simple fixes - e.g., patching a build (symlink, missing lib), resubmitting
failed jobs, cleaning up stale files. Anything that is simple enough and does
not result in external commits or a lot of runs. If in doubt, do a small test
first (e.g., devel queue, 2 min walltime) and proceed if it works. Use
judgement: a one-line symlink fix is fine; a sweeping refactor is not. Log what
was done so the user can review in the morning.

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

## Backslash Alignment Tool

`~/bin/align_backslashes.py <file> <start_line> <end_line> [--inplace] [--column N]`
Aligns trailing `\` continuation characters in shell scripts. Auto-detects
target column from longest content line, or use `--column N` to fix it.

## Column Alignment Tool

`~/bin/align_columns.py <file> <start_line> <end_line> [--inplace]`
Aligns columns in a range of lines. Detects columns by 2+ space gaps.
Lines are 1-based. Without `--inplace`, prints aligned output to stdout.

## Variable Initialization (CRITICAL)

**NEVER create uninitialized variables.** Always initialize with sensible defaults:
- Counts/sizes: `= 0`, indices: `= -1`, floats: `= NaN` or `= -max()`
- Pointers: `= nullptr`, booleans: `= false`
- Add `// will change` comment if value is immediately overwritten

## ASP Builds on pfe/pfx (NAS/Pleiades)

**Release build location:** `/u/oalexan1/projects/BinaryBuilder/StereoPipeline/`
(`/u/oalexan1` and `/home6/oalexan1` are the same path, symlinked).
Full release layout: `bin/` (wrapper scripts + Python), `libexec/` (C++ binaries),
`lib/`, `plugins/`, `docs/`, etc. The nightly bot extracts directly into this dir
(no tarballs on pfx - `asp_tarballs/` is empty). No `auto_build/` on pfx either.
Conda/miniconda is at `/swbuild/oalexan1/miniconda3` (symlinked from `~/miniconda3`).
Micromamba is at `~/micromamba` (real dir on home filesystem).

**NAS storage tiers:** Software and build tools live on `/swbuild` (fast, backed up).
Large data lives on nobackup (high capacity, not backed up).
**Tape storage (lfe/lou):** `lfe.nas.nasa.gov` is the Lou File Element - cold
tape archive for long-term storage. Access: `ssh lfe` (goes via sfe gateway).
Home dir on lfe: `/u/oalexan1`. Archive tarballs go there.
Key paths:
- `/swbuild/oalexan1/miniconda3` - conda (symlinked from `~/miniconda3`)
- `/vast_swbuild/swbuild/oalexan1/projects/BinaryBuilder` - BinaryBuilder repo
  (symlinked from `~/projects/BinaryBuilder`)
- `~/projects/` subdirs like PeruSat, spot5_alps, atlanta, casa_grande, etc.
  are symlinks pointing to nobackup for data storage.

**On l1:** Release tarballs saved in `~/projects/BinaryBuilder/asp_tarballs/`.
Dev build is in `~/projects/StereoPipeline/install/`.

**Dev build rsync overwrites lib/, libexec/, and Python scripts in bin/
on top of the release install on pfx.** Use `--checksum` to avoid skipping files
with same size but different content. Can rsync from Mac or l1.

## Syncing Dev Build to pfe/pfx

**Network topology:** Mac and l1 each connect independently to pfe/pfx via
separate SSH tunnels through a pfe node. They do NOT route through each other.
l1 may be down; Mac->pfx always works as long as the pfe master connection is
alive. The specific pfe node and port numbers change over time - if in doubt,
check `~/.ssh/config` or `~/tunnel.sh` for current values.

**If `ssh pfx` fails** with "Connection refused" or "Connection timed out"
(tunnel port not listening), the master SSH connection has dropped. Rerun
`~/bin/tunnel.sh` (or equivalent) to re-establish the tunnel, then retry.

In the release layout, C++ binaries go in `libexec/` and Python wrapper scripts
go in `bin/`.

```bash
ss=StereoPipeline

# Sync lib and libexec dirs
rsync -avz --checksum ~/projects/StereoPipeline/install/lib \
  ~/projects/StereoPipeline/install/libexec \
  pfx:/home6/oalexan1/projects/BinaryBuilder/${ss}/

# Sync C++ binaries to libexec (release layout, not bin)
rsync -avz --checksum ~/projects/StereoPipeline/install/bin/* \
  pfx:/home6/oalexan1/projects/BinaryBuilder/${ss}/libexec/

# Sync Python scripts to bin
rsync -avz --checksum ~/projects/StereoPipeline/install/bin/*py \
  pfx:/home6/oalexan1/projects/BinaryBuilder/${ss}/bin/
```

## TODO Comment Convention

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

## NASA NAS / Pleiades Supercomputer

**Front-ends for job submission:** `athfe01`-`athfe04` (ssh athfe01, NOT pfe).
These are the Athena/Turin front-ends. Submit PBS jobs from there.
Turin (tur_ath) jobs MUST be submitted from athfe, not pfe (qsub fails with
exit 32 from pfe). Direct SSH alias configured: `ssh athfe01` (ProxyJump
through pfx in `~/.ssh/config`). So: `ssh athfe01 "cd ... && qsub ..."`.

- **Compute nodes:** `tur_ath` (Turin Athens, 256 CPUs) - submit from athfe only.
  `bro_ele` (Broadwell Electra, 28 CPUs) - submit from pfe (not athfe).
- **Queue:** `normal` (max walltime 8:00:00)
- **GID (budget code):** `e2305` (personal allocation, used for SFS and SPOT5 work)
- **Job status:** `qstat -u $(whoami)`
- **Storage:** pfe and athfe share the same filesystem, so rsync to pfx
  but ssh to athfe01 for job submission.

Primer with qsub examples: `~/projects/spot5_alps/spot5_alps_notes.sh`

- **Launching ASP jobs on the supercomputer:** Never run compute on the head
  node (pfe/pfx). Write a script and submit via qsub. Imitate existing scripts
  in the project dir (e.g., `parallel_sfs_mm.sh`, `mapproj_geodiff_tile.sh`)
  for the PATH/env setup pattern:
  ```bash
  export ISISDATA=$HOME/projects/isis3data
  export ISISROOT=$HOME/miniconda3/envs/asp_deps
  s=StereoPipeline
  export PATH=$HOME/projects/BinaryBuilder/$s/bin:$ISISROOT/bin:$PATH
  cd $currDir
  ```

- **Scripts submitted via qsub MUST be executable** (`chmod +x`). PBS fails
  with "Permission denied" (exit 254) if the script lacks execute permission.
  Always `chmod +x` after creating new `.sh` scripts intended for qsub.

- **Always use FULL PATH for scripts in qsub commands.** PBS runs on compute
  nodes where the working directory may differ. Use `${currDir}/script.sh`
  not bare `script.sh`. Exit 254 = script not found.

- **Always `sleep 1` (or more) between qsub calls** in loops or batch scripts.
  Rapid-fire qsub can overwhelm the PBS scheduler. The
  `batch_mapproject_clip.sh` script already has `sleep 2`.

- **qsub scripts must redirect output to a log file in the work dir so
  progress can be tailed in real time.** Redirect only (`> log` / `>> log`),
  not `tee` - PBS dislikes the extra buffering.

- **Watchdog scripts: check job completion robustly.** Do NOT rely solely on
  counting output files (e.g., `*adjusted_state.json`) - jitter_solve writes
  cameras after pass 0 but may still be running pass 1+. Always check BOTH:
  (a) the final report file exists (`run-triangulation_offsets.txt`), AND
  (b) the PBS job is gone from qstat (`grep -w` for exact job name match).
  Use `grep -w` to avoid substring collisions (e.g., `jit_lo_s1` matching
  `jit_lo_s10`).

- **Monitoring long jobs: use BOTH watchdog scripts AND Claude self-timers.**
  (1) A nohup watchdog script that checks every 30 min and launches the next
  step when ready. (2) Claude background timers (`sleep 1800` via
  `run_in_background`) where Claude wakes up, checks status, acts, and sets
  another timer. Always use both in parallel - the watchdog survives if
  Claude's session drops, and the timer gives interactive feedback.

- **Symlinked project dirs on NAS/Pleiades (pfe/pfx/athfe):** Many subdirs under
  `~/projects/` are symlinks on NAS (e.g., PeruSat, spot5_alps, and others).
  The actual data lives on the nobackup filesystem (large storage), but the
  symlinks in `~/projects/` mirror the Mac directory structure for convenience.
  **NEVER rsync a symlinked dir itself** - that would replace the symlink with
  a plain directory, breaking the link to nobackup. Only rsync files or subdirs
  *inside* the symlinked dir. Use a trailing slash (`rsync -avz src/ dest/dir/`)
  or rsync individual items. On the Mac these are real dirs so it's not an issue.

## GitHub CLI (gh)

Installed in conda env `gh`. Not on PATH - use full path.

**Portable path:** `$(ls -d $HOME/*conda3/envs/gh/bin/gh)`

| Machine | Path |
|---------|------|
| **Mac** | `/Users/oalexan1/anaconda3/envs/gh/bin/gh` |
| **lunokhod1** | `/home/oalexan1/miniconda3/envs/gh/bin/gh` |

**Key repo slugs for -R flag:**
- ASP: `NeoGeographyToolkit/StereoPipeline`
- VW: `visionworkbench/visionworkbench`
- BB: `NeoGeographyToolkit/BinaryBuilder`
- Tests: `NeoGeographyToolkit/StereoPipelineTest`

**Common operations** (set `gh=/home/oalexan1/miniconda3/envs/gh/bin/gh`):
```bash
# Issues
$gh issue list -R NeoGeographyToolkit/StereoPipeline
$gh issue view 123 -R NeoGeographyToolkit/StereoPipeline
$gh issue close 123 -R NeoGeographyToolkit/StereoPipeline

# Pull requests
$gh pr list -R NeoGeographyToolkit/StereoPipeline
# NOTE: "gh pr view" hits deprecated GraphQL Projects Classic API and errors.
# Use the REST API instead:
$gh api repos/OWNER/REPO/pulls/123 --jq '.body'           # PR body
$gh api repos/OWNER/REPO/issues/123/comments --jq '.[].body'  # PR comments
$gh pr create -R NeoGeographyToolkit/StereoPipeline --title "..." --body "..."

# CI workflows
$gh workflow run build_test_mac_arm64.yml -R NeoGeographyToolkit/StereoPipeline
$gh run list -R NeoGeographyToolkit/StereoPipeline --limit 5
$gh run view <run-id> --log-failed -R NeoGeographyToolkit/StereoPipeline
```

## Co-Authored-By Trailer (CRITICAL)

Every commit MUST include:
```
Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
```
Always use a HEREDOC for commit messages to ensure the trailer is included.

**EXCEPTION: ISIS3 repo (`~/projects/ISIS3`).** Do NOT add the Co-Authored-By
trailer for commits in this repo. DOI-USGS may have its own approval process
for AI-assisted contributions. Omit the trailer for all ISIS pushes.

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

## GTest Discovery and ISISROOT

Building ISIS (and ALE with tests) requires GTest submodule and ISISROOT:
1. Init gtest: `git -C ~/projects/ISIS3 submodule update --init --recursive`
2. Set `ISISROOT` before make/ninja (GTest discovery runs the test binary at
   build time, which needs ISISROOT to find ISIS data paths).
3. ALE with `ALE_BUILD_LOAD=ON` has test link issues but the library installs fine.

Full details in `~/projects/isis_mapproject/isis_mapproject_notes.sh` lines 179-193
and `~/projects/env_update.sh`.

## ISIS Data (CRITICAL)

**NEVER delete `~/projects/isis3data/` or its subdirectories without explicit permission.**
This is 179 GB of mission kernels that take forever to re-download over home ISP.

## Safe Directory Cleanup (CRITICAL)

**NEVER run `rm -rf` with absolute paths or variable-expanded paths to clean build dirs.**
VW was wiped TWICE by agents doing `rm -rf /path/to/visionworkbench/build_linux` with
bad variable expansion. The rule:

1. `cd` into the project directory first
2. Run `ls` to confirm you're in the right place
3. Use **relative paths only**: `rm -rf ./build_linux`, never `rm -rf $bld`
4. Never combine `rm -rf` with shell variables that could expand to empty or wrong paths

**Example of what NOT to do:**
```bash
rm -rf $bld/CMakeCache.txt $bld/CMakeFiles  # if $bld is empty, wipes / !
```

**Correct:**
```bash
cd /Users/oalexan1/projects/visionworkbench
ls  # confirm src/, cmake/, etc. are here
rm -rf ./build_linux
```

## Cross-Compile Build Directories (CRITICAL)

**Native builds use `build/` and `install/`. Cross-compile uses `build_linux/` and `install_linux/`.**

Both VW and ASP follow this convention:
- `build/` + `install/` = native Mac ARM64 (or native Linux on lunokhod1)
- `build_linux/` + `install_linux/` = cross-compiled Linux x86_64 from Mac

NEVER use `build/` or `install/` for cross-compilation. NEVER use `build_linux/` or
`install_linux/` for native builds. Mixing these up destroys the other build.
