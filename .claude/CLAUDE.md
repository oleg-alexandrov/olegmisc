# Long-term memory for Claude Code

**The user's name is Oleg (oalexan1). Don't say "the user" but no need to use his name constantly either - this is direct conversation.**

- Always end files with a newline character (POSIX requirement).
- When Oleg says to "remember" something, add it to this CLAUDE.md file.
- "Project dir" or "projects dir" means `~/projects`.

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

## Code Movement (CRITICAL)

**When moving code between files, ALWAYS use atomic cut-and-paste.**

**NEVER delete from one file and reconstruct/rewrite in another.**

Why: Reconstruction loses comments, formatting, and subtle details. "Move" means cut-and-paste, not delete-and-rewrite.

## Braces for Single-Line Statements

Remove braces from single-statement control flow blocks (if, else, for, while, do-while).
Keep braces for scope blocks (not attached to control flow) and when needed for clarity with nested conditions.

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

**lunokhod1** (`lunokhod1.ndc.nasa.gov`) - the dev machine. Check with `uname -n`:
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
- Can build ASP: `make -C ~/projects/StereoPipeline/build -j10`
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

**NEVER git add `run/` or `gold/` directories in StereoPipelineTest.** These contain
large binary output files (~40 GB total). They are gitignored. Only `run.sh` and
`validate.sh` are tracked in git.

Each test directory has:
- `run.sh` - the test commands
- `validate.sh` - comparison against gold (reference) output
- `gold/` - reference output files
- `run/` - generated output (created by `run.sh`)

When creating new tests, always `chmod +x run.sh validate.sh`.

## Project Status Files

**Work tracking files** in `/home/oalexan1/projects/`:
- `mpr_todo.sh` - Records completed work for MPR project reports (not a TODO list)
- `ostfl_todo.sh` - Records completed work for older OSTFL reports (not a TODO list)
- `ostfl_2025_notes.sh` - Current OSTFL 2025 work tracking and status updates

When user says to update "OSTFL status", edit `ostfl_2025_notes.sh`, not `ostfl_todo.sh`.

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

- Do NOT repeatedly ask "anything else?" or similar prompts
- **NEVER prompt to "get back to work"** or "ready to implement?" or "what's next?"
- Trust the user to drive the conversation
- If conversation pauses, let it pause

**BE ENTERTAINING when chatting:**
- Match casual energy, make jokes, suggest breaks
- "Enough obsessing over header dependencies - go touch grass"
- Balance work mode (concise, efficient) with chat mode (entertaining, human)

## RST Documentation Formatting

**Documentation file locations:** check both `docs/` subdirectories and repository root level.

**Style:** Be concise - users are expert researchers. Give hints and pointers, not tutorials.

**Formatting rules:**
- Section underlines must be exactly the same length as heading text
  - **CRITICAL: Always count characters carefully - prone to off-by-one errors**
- Heading levels: `=` top, `-` subsection, `~` sub-sub, `^` sub-sub-sub
- For `:ref:` links where text matches target, use simplified syntax: `` :ref:`tool_name` ``

## Output Parameter Style

Group all outputs after inputs. Put a single `// Outputs` comment on its own line before them.

## Copyright Year Updates

Format: `Copyright (c) 2006-YYYY, United States Government...` - update end year to current year when editing files.

## Style Cleaning Tool

`~/bin/clean_style.py <input_cpp_file>` - automated C++ style cleanup. Use without asking when requested.

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

**Triggering CI workflows:**
```bash
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

## ~/projects Git Rule (CRITICAL)

Files in `~/projects/` are tracked by `~/projects/.git` (NOT `~/.git`).
Always use `git -C ~/projects` for add, commit, push, etc.

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

## Conda Build Environments (Mac ARM64)

- **`boa`** - Build conda packages for osx-arm64 (native)
- **`boa_x64`** - Build conda packages for osx-64 (cross-compile for Intel). Created with `CONDA_SUBDIR=osx-64`.
- **`asp_deps`** - Development environment with all ASP dependencies for compiling ASP from source
- Initialize conda with `iz` alias (calls `init_conda_zsh` in `.zshrc`)

**Cross-compile notes for x86_64** are in `~/projects/BinaryBuilder/install_asp_notes.sh` around line 1690.
