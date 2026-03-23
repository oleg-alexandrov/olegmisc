# Long-term memory for Claude Code

**AFTER CONTEXT COMPACTION: Re-read this ENTIRE file (all of CLAUDE.md, not
just the first 200 lines). Also re-read these project files - you WILL lose
build/test instructions and current task context otherwise:
- `~/projects/binary_csm/binary_csm_notes.sh` (current main project:
  binary CSM state format - msgpack for ISDs/model states, FY26 USGS task)
- `~/projects/isis_jp2/isis_jp2_notes.sh` (recent project: remove
  Kakadu from ISIS, use GDAL for JPEG2000)
- `~/projects/isis_mapproject/isis_mapproject_notes.sh` (recent project:
  ISIS cam2map / ASP mapproject parity)
- `~/projects/csm_resample/csm_resample_notes.sh` (recent project:
  ALE/CSM work for ISIS - reduce linescan ISD oversampling, PR #677)
- `~/projects/env_update.sh` (current: update asp_deps env to match
  ISIS, build ALE/USGSCSM/ISIS from source, Qt6 port, CGAL update.
  Tied to binary CSM, ALE ephem_fix, and ISIS asp_map contributions.)
- `~/projects/isis_jigsaw_isd/isis_jigsaw_isd_notes.sh` (upcoming project:
  jigsaw external ISD input/output, FY26 USGS task)
Rules past line 200 get truncated and lost otherwise.**

**ISIS3 build/test quick reference (so you don't lose this after compaction):**
```bash
# Always use isis_dev, NEVER asp_deps for ISIS work
eval "$($HOME/anaconda3/bin/conda shell.zsh hook)"
conda activate isis_dev
export ISISROOT=$HOME/projects/ISIS3/build
export ISISDATA=$HOME/projects/isis3data
export ISISTESTDATA=$HOME/projects/isis_test_data
export SPICEQL_CACHE_DIR=/tmp/spiceql_cache
# Build and install (always use install, it copies libisis to conda env)
ninja -C ~/projects/ISIS3/build install
# Parity test
cd ~/projects/isis_mapproject/cam2map_eqc_mpp && bash run.sh
# GTest
cd ~/projects/ISIS3/build && ctest --test-dir . -R AspMap --output-on-failure
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
  VW, BinaryBuilder, StereoPipelineTest, projects, home dir — no exceptions.
  Do not bundle pushes with other operations. Do not push as part of a
  multi-step workflow unless explicitly told "and push". Do not assume
  "git add and push" means push — wait for the word "push" as a separate
  explicit instruction. Especially `git push god` (upstream org).
- **When fixing code, ALWAYS pause for review before pushing.** Show local
  test results and let the user review changes first. Do not push immediately
  after committing — especially when the push triggers CI regressions that
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

**NEVER remove or skip existing comments when editing code.** This applies to
ALL edits, not just refactoring. When replacing a block of code that contains
comments, the replacement MUST include those same comments. Comments above
assertions, before code sections, and inline explanations exist for a reason.
Only remove a comment if the code it describes was deleted. If unsure whether
a comment is still relevant, keep it. This is a recurring issue - double-check
every edit to ensure no comments were dropped.

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

- **NEVER use non-ASCII characters in code or comments** - use `x` not `*`, `-` not `--`, regular quotes not smart quotes
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

**Common VW types that need vw:: prefix (frequently missed by sed):**
- `vw::ImageView<T>`, `vw::ImageViewRef<T>`, `vw::DiskImageView<T>`, `vw::DiskImageResourceGDAL`
- `vw::BBox2`, `vw::BBox2i`, `vw::Vector2`, `vw::Vector3`
- `vw::PixelMask<T>`, `vw::PixelGray<T>`, `vw::PixelGrayA<T>`, `vw::PixelRGB<T>`
- `vw::TerminalProgressCallback`
- `vw::ArgumentErr`, `vw::LogicErr`, `vw::IOErr`
- `vw::crop()`, `vw::fill()`, `vw::edge_extend()`, `vw::bounding_box()`
- `vw::create_mask()`, `vw::copy_mask()`, `vw::apply_mask()`, `vw::invalidate_mask()`, `vw::validate_mask()`
- `vw::gaussian_filter()`, `vw::compute_kernel_size()`
- `vw::cartography::GeoReference`, `vw::cartography::read_georeference()`, `vw::cartography::write_georeference()`

Don't do blind sed-style namespace replacements - read and comprehend the code first. Check headers, using declarations, and surrounding patterns.

## Project Context

- The StereoPipeline repository is at /home/oalexan1/projects/StereoPipeline
- The VisionWorkbench repository is at /home/oalexan1/projects/visionworkbench
- ASP stands for Ames Stereo Pipeline (refers to StereoPipeline)
- The BinaryBuilder repository (`/home/oalexan1/projects/BinaryBuilder`) contains the ASP build toolset. Its `auto_build/` subdirectory has the nightly build and regression test infrastructure.

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

**Mac mini** (`Olegs-Mac-mini.local`) - secondary build machine:
- **ALWAYS use `make install`** for both VW and ASP, never bare `make`.
  The installed libraries may be stale even when the build is up to date.
- Can build ASP: `make -C ~/projects/StereoPipeline/build -j10 install`
- Conda init: `eval "$($HOME/anaconda3/bin/conda shell.zsh hook)" && conda activate asp_deps`
- **After `make install`, fix duplicate rpaths (macOS dyld rejects duplicates):**
  ```bash
  cd ~/projects/StereoPipeline
  for f in install/lib/libAsp*.dylib install/bin/*; do
    count=$(otool -l "$f" 2>/dev/null | \
      grep "path .*/miniconda3/envs/asp_deps/lib " | wc -l)
    if [ "$count" -gt 1 ]; then
      install_name_tool -delete_rpath \
        $HOME/miniconda3/envs/asp_deps/lib "$f" 2>/dev/null
    fi
  done
  ```
- Run tests with dev build: `export PATH=~/projects/StereoPipeline/install/bin:$PATH`

**Local VM** (VirtualBox on laptop) - convenient for editing, too weak for builds:
- Do NOT build or compile
- No git available
- Only edit source files

## Running Tests

**Test suite location:** `/home/oalexan1/projects/StereoPipelineTest`

**How to run a single test:**
1. `cd` into the test directory
2. Run `bash run.sh > output.txt 2>&1`
3. Run `bash validate.sh` - exit 0 means pass
4. If it fails, check `output.txt`

**Do NOT use pytest** - just run `run.sh` and `validate.sh` directly.

**MANDATORY: Run regression tests after every code change.** After modifying an
ASP tool, find all matching test dirs in StereoPipelineTest:
```bash
ls -d /home/oalexan1/projects/StereoPipelineTest/ss_*toolname* 2>/dev/null
grep -rl 'toolname' /home/oalexan1/projects/StereoPipelineTest/ss_*/run.sh | head -20
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

**ISIS environment for dev builds:** ASP links against ISIS, so `ISISROOT` must
be set when running tests with an installed dev build. On Mac:
```bash
export ISISROOT=$HOME/anaconda3/envs/asp_deps
export PATH=~/projects/StereoPipeline/install/bin:$HOME/anaconda3/envs/asp_deps/bin:$PATH
```
On lunokhod1:
```bash
export ISISROOT=$HOME/miniconda3/envs/asp_deps
export PATH=~/projects/StereoPipeline/install/bin:$HOME/miniconda3/envs/asp_deps/bin:$PATH
```

## Notes Files (.sh)

Many `.sh` files in `~/projects/` are comment-only notes, not executable scripts.
Do NOT `chmod +x` these. Only make a `.sh` file executable if it is actually
meant to be run (has real commands, not just comments).

**Project-specific notes in subdirs:** New projects get their own subdirectory
under `~/projects/` with a notes file (e.g., `isis_mapproject/isis_mapproject_notes.sh`).
These subdir notes files ARE tracked by the projects repo (`~/projects/.git`).
Add them with `git -C ~/projects add subdir/file.sh`.

## Project Status Files

**Work tracking files** in `~/projects/` (tracked by `~/projects/.git`):
- `~/projects/mpr_todo.sh` - Monthly Progress Report. Records completed work
  for MPR project reports (not a TODO list).
- `~/projects/ostfl_2025_notes.sh` - Current OSTFL 2025 work tracking and status
  updates. When told to update "OSTFL status" or "OSTFL doc", this is the file.
- `~/projects/ostfl_todo.sh` - Older OSTFL reports (historical, not current).
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

**VS Code settings:** `/home/oalexan1/.config/Code/User/settings.json`

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

## Column Alignment Tool

`~/bin/align_columns.py <file> <start_line> <end_line> [--inplace]`
Aligns columns in a range of lines. Detects columns by 2+ space gaps.
Lines are 1-based. Without `--inplace`, prints aligned output to stdout.

## Variable Initialization (CRITICAL)

**NEVER create uninitialized variables.** Always initialize with sensible defaults:
- Counts/sizes: `= 0`, indices: `= -1`, floats: `= NaN` or `= -max()`
- Pointers: `= nullptr`, booleans: `= false`
- Add `// will change` comment if value is immediately overwritten

## Syncing Changes to Laptop

**After every code or doc change on lunokhod1, sync to laptop:**

```bash
# From /home/oalexan1/projects/StereoPipeline:
rsync -avz --exclude=build --exclude=install --exclude=docs/_build --exclude=docs/images --exclude=examples --exclude=.git . oalexan1@laptop:~/projects/StereoPipeline/
```

If .sh files in `/home/oalexan1/projects/` changed:
```bash
rsync -avz /home/oalexan1/projects/*.sh oalexan1@laptop:~/projects/
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

## Dependabot / Security Alerts

When a `git push` shows Dependabot or security vulnerability warnings, proactively
flag it and offer to investigate/fix. These are usually easy wins (delete a lock
file, update a dep) and worth cleaning up on the spot rather than ignoring.

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

## Conda Build Environments (Mac ARM64)

- **`boa`** - Build conda packages for osx-arm64 (native)
- **`boa_x64`** - Build conda packages for osx-64 (cross-compile for Intel). Created with `CONDA_SUBDIR=osx-64`.
- **`asp_deps`** - Development environment for compiling ASP/VW from source.
  **Only for ASP/VW work.** For ISIS3 work use **`isis_dev`** instead.
- **`isis_dev`** - Development environment for compiling ISIS3 from source.
  **Always use this for ISIS3 builds, installs, and tests.**
  See `~/projects/isis_mapproject/isis_mapproject_notes.sh` for env setup,
  build/install commands, and test recipes.
- Initialize conda with `iz` alias (calls `init_conda_zsh` in `.zshrc`)

**Cross-compile notes for x86_64** are in `~/projects/BinaryBuilder/install_asp_notes.sh` around line 1690.

## TODO: ISIS Special Pixel Masking in Mapproject

ASP mapproject does not mask ISIS special pixels (LIS, LRS, HIS, HRS) for
.cub inputs. Only NULL is masked via nodata_value. Fix is in
MapprojectImage.cc project_image_nodata(). Also review whether the CSM
session has the same gap when map-projecting .cub files.

## Nightly Build Status

| Date | Platform | Status | Likely cause |
|------|----------|--------|--------------|
| 2026-02-26 | lunokhod1 | **RESOLVED** | OrthoRasterizer refactoring. Gold regenerated, tests pass. |
