# Long-term memory for AI assistants

**Note:** This file is shared between Gemini and Claude Code via symlink.
`/home/oalexan1/.claude/CLAUDE.md` -> `/home/oalexan1/.gemini/GEMINI.md`.

**The user's name is Oleg (oalexan1). Don't say "the user" but no need to use his name constantly either - this is direct conversation.**

## FILE CREATION NEWLINE REQUIREMENT (CRITICAL)

**🚨 GEMINI REPEATEDLY FORGETS THIS: ALL FILES MUST END WITH NEWLINE 🚨**

**EVERY TIME you create a file with the `create` tool:**
1. **ALWAYS end the file_text parameter with `\n`**
2. **NO EXCEPTIONS - this is a POSIX requirement**
3. **Check your file_text - does it end with newline? If not, ADD IT**

**Why this is CRITICAL:**
- Git shows "\ No newline at end of file" warning
- Text processing tools (cat, grep, sed) expect it
- Some compilers require it
- File concatenation works cleanly
- Standard Unix convention

**MEMORY AID: Before calling create tool, mentally check "Does my file_text end with \\n?"**

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
**Remember:** One extra or missing line can break compilation. Always verify boundaries!

**For sed bulk replacements:**
- Always be generous with line ranges - it's safer to include a few extra lines than miss one
- If replacing within a function that starts at line N, use N as the start, not N+10
- Verify there are no stragglers just before or after your range
- Example of what NOT to do: Function body starts line 891, but using `'901,1046s/...'` misses line 900

## Code Movement (CRITICAL)

**When moving code between files, ALWAYS use atomic cut-and-paste.**

**NEVER delete from one file and reconstruct/rewrite in another.**

Why this matters:
- Reconstruction loses comments, formatting, and subtle details
- It's hard to diff after moving code - you can't easily verify nothing was lost
- Comments are often the first casualty of reconstruction
- This is a source of subtle bugs

**Correct approach:**
1. Cut the exact code block from source file
2. Paste it exactly into destination file
3. Only then make any necessary adjustments (imports, etc.)

**Wrong approach:**
1. Delete from source file
2. Rewrite/reconstruct the code in destination from memory
3. Wonder why comments and details are missing

**Remember:** "Move" means cut-and-paste, not delete-and-rewrite.

## Diff Display Convention

When showing code changes:
- Use **red (-)** for removed/old lines
- Use **green (+)** for added/new lines
- Follow standard diff format conventions

## Braces for Single-Line Statements

**CRITICAL: Remove braces from single-statement control flow blocks.**

Do NOT use braces when a control flow statement (if, else, for, while, do-while, etc.) has only one statement in its body.

**IMPORTANT: Keep braces for scope blocks**
- Do NOT remove braces from blocks that exist purely for scoping (not attached to if/for/while/etc.)

**Exception:** Keep braces if there are multiple statements or if removing them would cause ambiguity in nested conditions.

## C++ Code Style Conventions (from Copilot)

- **NEVER use non-ASCII characters in code or comments** - terminal tools like `more` and `diff` don't handle them well
  - Use `x` instead of `×` for multiplication
  - Use `-` instead of `—` or `–` for dashes
  - Use regular quotes `"` instead of smart quotes `"` or `"`
  - Use regular apostrophes `'` instead of smart apostrophes `'` or `'`
- **NEVER use equal-sign separators** (`=====` or similar) in code or comments
  - No `//=====` separator lines
  - No `//-----` separator lines
  - Use simple comment headers if needed: `// Function definitions`
  - Keep code clean without visual dividers
- **NEVER use ellipsis (...)** in messages or comments
  - Use period instead: "Processing data." not "Processing data..."
  - Exception: When showing truncated output in documentation examples
- No space before the :: scope resolution operator (e.g., `vw::math::norm_2` not `vw :: math :: norm_2`)
- No space before the : in a constructor's initializer list
- Use camelCase for function names (e.g., `rayPlaneIntersect`, not `ray_plane_intersect` or `RayPlaneIntersect`)
- ASP headers should be grouped together and placed before any other headers
- Use only one newline between functions
- Keep lines under 90 characters - break long lines with proper indentation
- When breaking lines, indent continuation lines to align with the start of the expression or function arguments
- Use "\n" instead of "std::endl" for newlines in C++ output
  - **CRITICAL when using sed for bulk replacements:**
    - Wrong: `sed 's/<< std::endl/<< \\n/g'` creates `<< \n` (unquoted, syntax error)
    - Correct: `sed 's/<< std::endl/<< "\\n"/g'` creates `<< "\n"` (proper string literal)
    - Always include quotes when replacing endl with \n
    - Test the sed pattern on one line before applying to whole file
- Use only one empty line between blocks of code or comments
- Always end files with a newline character
- Remove double newlines in any code you touch
- No trailing whitespace at the end of any line
- When function arguments are split across multiple lines, indent following lines to align with the first argument
- **ALWAYS adjust continuation line indentation when modifying or adding functions** - continuation lines must align with the opening parenthesis, not at some arbitrary fixed indentation
  - Count characters from start of line to the opening `(` - that's how many spaces continuation lines need
  - Example: `void myFunction(int arg1,` = 15 chars, so next line needs 15 spaces before `int arg2`
- **Option help text strings:** Wrap at ~90 characters per line. Split long help text into multiple quoted strings that are automatically concatenated. **CRITICAL:** Ensure exactly one space between words at quote boundaries - put the space at the end of the preceding string, not at the start of the next:
  ```cpp
  ("left-bathy-mask", po::value(&global.left_bathy_mask),
    "Mask to use for the left image when doing bathymetry. Pixels classified as "
    "water must be either no data or have zero value in the mask, while land pixels "
    "must have positive value.")
  ```
  Common mistakes:
  - `"first part" "second"` - WRONG, becomes "first partsecond"
  - `"first part " " second"` - WRONG, becomes "first part  second" (double space)
  - `"first part " "second"` - CORRECT, becomes "first part second"
- **For loop style:**
  - Use postincrement `i++` NOT preincrement `++i` in loop expressions
  - Always use spaces around `=` in initialization: `i = 0` NOT `i=0`
  - Always use spaces around comparison operators: `i < size` NOT `i<size`
  - Correct: `for (size_t i = 0; i < lens.size(); i++)`
  - Incorrect: `for (size_t i=0; i<lens.size(); ++i)`

## VisionWorkbench Namespace Conventions

vw stands for VisionWorkbench.

- `vw::math::norm_2` - norm_2 is in vw::math namespace
- `vw::math::subvector` - subvector is in vw::math namespace
- `vw::cartography::block_write_gdal_image` - always add vw::cartography:: before block_write_gdal_image
- `vw::cartography::crop` - when cropping a GeoReference object
- `vw::crop` - when cropping images
- `vw::ArgumentErr` - when throwing an argument error
- `vw::vw_out` - for standard output
- `vw::vw_throw` - when throwing an exception
- `vw::geometry::write_shapefile` - write_shapefile is in vw::geometry namespace
- `vw::geometry::read_shapefile` - read_shapefile is in vw::geometry namespace
- NEVER include 'vw/Math/LeastSquares.h' - this header does not exist and has been removed

**Common VW types that need vw:: prefix (frequently missed by sed):**
- `vw::ImageView<T>` - generic image view
- `vw::ImageViewRef<T>` - reference-counted image view
- `vw::BBox2`, `vw::BBox2i` - bounding boxes (note: both variants!)
- `vw::Vector2`, `vw::Vector3` - vectors
- `vw::PixelMask<T>`, `vw::PixelGray<T>`, `vw::PixelGrayA<T>`, `vw::PixelRGB<T>` - pixel types
- `vw::DiskImageView<T>` - disk-backed image view
- `vw::DiskImageResourceGDAL` - GDAL image resource (often missed!)
- `vw::TerminalProgressCallback` - progress callback
- `vw::ArgumentErr`, `vw::LogicErr`, `vw::IOErr` - exception types
- `vw::vw_throw()`, `vw::vw_out()` - exception/output functions
- `vw::crop()`, `vw::fill()`, `vw::edge_extend()` - image functions
- `vw::create_mask()`, `vw::copy_mask()`, `vw::apply_mask()`, `vw::invalidate_mask()`, `vw::validate_mask()` - mask functions
- `vw::gaussian_filter()`, `vw::compute_kernel_size()` - filtering functions
- `vw::bounding_box()` - compute bounding box of image (often missed!)
- `vw::cartography::GeoReference` - georeference type
- `vw::cartography::read_georeference()`, `vw::cartography::write_georeference()` - georef I/O
- `vw::cartography::block_write_gdal_image()` - GDAL write function

## Project Context

- The StereoPipeline repository is at /home/oalexan1/projects/StereoPipeline
- The VisionWorkbench repository is at /home/oalexan1/projects/visionworkbench
- ASP stands for Ames Stereo Pipeline (refers to StereoPipeline)
- The BinaryBuilder repository (`/home/oalexan1/projects/BinaryBuilder`) contains the ASP build toolset. Its `auto_build/` subdirectory has the nightly build and regression test infrastructure. The nightly tests use `auto_build/run_tests.sh` which clones and runs the StereoPipelineTest suite.

## Machine-Specific Permissions

**lunokhod1** (`lunokhod1.ndc.nasa.gov`) - the dev machine. Check with `uname -n`:
- Full access to git, compilation, and building
- Compiler: g++ 12.4.0 (conda-forge, in `asp_deps` conda env)
- CMake 3.27.9, GNU Make 4.1, 16 cores
- Build dirs: `StereoPipeline/build/` and `visionworkbench/build/`
- Install dir: `StereoPipeline/install/bin/` (105 ASP binaries)
- Git remotes: `origin` = user's fork, `god` = upstream org (both repos)
- You ARE allowed to run `make`, `cmake`, and build commands on this machine
- Build with: `make -C /home/oalexan1/projects/StereoPipeline/build -j16`

**Local VM** (VirtualBox on laptop) - convenient for editing, too weak for builds:
- Do NOT build or compile
- No git available
- Only edit source files

## Running Tests

**Test suite location:** `/home/oalexan1/projects/StereoPipelineTest`

**How to run a single test:**
1. `cd` into the test directory (e.g., `cd /home/oalexan1/projects/StereoPipelineTest/ssCSM_LinescanMapProj1_dist`)
2. Run `bash run.sh > output.txt 2>&1` - redirect output to avoid excessive terminal output
3. Run `bash validate.sh` - this compares output against gold files
4. If `validate.sh` exits with 0, the test passed
5. If it fails, check `output.txt` for errors

**Do NOT use pytest** - just run `run.sh` and `validate.sh` directly.

**When creating new tests**, always make `run.sh` and `validate.sh` executable:
```bash
chmod +x run.sh validate.sh
```

Each test directory has:
- `run.sh` - the test commands
- `validate.sh` - comparison against gold (reference) output
- `gold/` - reference output files
- `run/` - generated output (created by `run.sh`)

## Project Status Files

**Work tracking files** in `/home/oalexan1/projects/`:
- `mpr_todo.sh` - Records completed work for MPR project reports (not a TODO list)
- `ostfl_todo.sh` - Records completed work for older OSTFL reports (not a TODO list)
- `ostfl_2025_notes.sh` - Current OSTFL 2025 work tracking and status updates

**Important:** These files are primarily for recording what was accomplished for 
project reports, not for tracking future tasks. When user says to update "OSTFL 
status", edit `ostfl_2025_notes.sh`, not `ostfl_todo.sh`.

**VS Code settings location:**
- Settings file: `/home/oalexan1/.config/Code/User/settings.json`
- User directory: `/home/oalexan1/.config/Code/User/`

**Test suite location:**
- Test suite directory: `/home/oalexan1/projects/StereoPipelineTest`
- Main codebase: `/home/oalexan1/projects/StereoPipeline`

## CMake File Management

**CRITICAL: When adding or removing source files (.cc, .h, .cpp, .hpp) in a CMake project:**

1. **Always touch the CMakeLists.txt in that directory**
   ```bash
   touch path/to/directory/CMakeLists.txt
   ```

2. **Always touch the parent CMakeLists.txt as well**
   ```bash
   touch path/to/parent/CMakeLists.txt
   ```

**Why:** CMake uses GLOB patterns (like `file(GLOB ...)` or `get_all_source_files()`) to find source files. These patterns are only evaluated at configure time. CMake does NOT automatically detect new/removed files unless you force a reconfigure by touching a CMakeLists.txt file or manually re-running cmake.

**Example:** When adding `RigParseOptions.h/cc` to `/home/oalexan1/projects/StereoPipeline/src/asp/Rig/`:
```bash
touch /home/oalexan1/projects/StereoPipeline/src/asp/Rig/CMakeLists.txt
touch /home/oalexan1/projects/StereoPipeline/src/asp/CMakeLists.txt
```

This ensures the build system will see the new files on the next build.

**MPR Report Format** in `mpr_todo.sh`:
- Monthly reports are structured with project headers (e.g., "OSTFL-24", "STV/DSI", "Shallow-water bathymetry")
- Work items must be listed under their correct project header
- Example: COG support is funded by OSTFL → goes under "# OSTFL-24" section
- Don't create standalone items outside project categories
- This keeps funding sources clear for reporting

## Output Statements

- Do NOT remove vw_out() statements - these are for user-facing informational output, not debugging
- Only remove std::cout statements and other debug-specific output when asked to remove debug statements

## File Operation Safety (CRITICAL)

ALWAYS check if a file exists and view its contents before overwriting.

Before writing to ANY file:
1. Check if it exists: `ls -l filename` or `cat filename`
2. View contents to understand what would be lost
3. Ask user for permission if overwriting existing content
4. Prefer `>>` (append) over `>` (overwrite) when adding to existing files

NEVER use `>` (overwrite) without explicit confirmation from the user.

This prevents accidental data loss of important configuration files or user data.

## Whitespace Cleanup

- Always strip trailing whitespace at end of lines - actively remove any spaces/tabs at line endings when editing code, but keep newlines
- Always remove double blank lines when editing any code - keep only single blank lines between code blocks

## Header Includes

- Always check if required headers are included when adding code that uses new classes or functions
- If compilation fails with "not a member of" or "was not declared" errors, add the appropriate header include
- Common VW headers: vw/Math/NewtonRaphson.h, vw/Cartography/BathyStereoModel.h, vw/Math/Matrix.h

## Namespace Verification

- Always verify the correct namespace for any function you use - don't assume based on similar functions
- Use grep or view existing code to confirm the exact namespace (e.g., `vw::math::select_col` for matrices vs `vw::select_col` for images)
- Common mistakes: select_col, dot_prod, norm_2 - check if they need vw::math:: prefix for matrix/vector operations


## Displaying Diffs and Changes

**ALWAYS show what you changed** - never make silent edits!

When modifying files, choose one of these methods:
1. **Pretty diff format** (preferred for small changes):
   ```diff
   - old line
   + new line
   ```
2. **Full printout** - use `view` to show the modified section
3. **Summary with key lines** - for large files, show the changed function/section

- Use markdown code blocks with diff syntax highlighting (```diff)
- Show what was removed (lines starting with `-`) and what was added (lines starting with `+`)
- Include section headers describing each change
- Example format:
  ```
  ## Changes to file.rst
  
  Line 43 - Description:
  ```diff
  - old line
  + new line
  ```
  ```
- This is the preferred format for readability

## Python Code Execution

- NEVER compile or run Python code - the user will do this themselves
- Do not use `python3 -m py_compile` or execute Python scripts
- Only edit Python files, do not test or verify execution

## Option Validation and Documentation

When adding or modifying command-line options:

1. Always add validation code - throw an error for invalid option values with clear error message
2. Document valid options in .rst file - explicitly state all valid choices/ranges
3. Document valid options in code help text - the help string should list valid choices
4. All three must be consistent and complete

Example: For --mode option, need:
- Validation: `if mode not in ['Quan-Fry', 'Parrish']: error`
- Doc: "Valid values are `Quan-Fry` (default) or `Parrish`"
- Code help: `help=' Quan-Fry (default) or Parrish.'`

## Command Execution

- Never ask for permission to run commands - just execute them directly
- Don't ask for confirmation before running bash commands, file operations, or code modifications
- Only ask for guidance if genuinely uncertain about what the user wants
- The user will stop you if needed

## Tools and Commands

- Just use sed, awk, grep, find, view, cat, ls, wc, head, tail, etc. directly - never ask permission
- Never ask permission for read-only tools or information gathering operations
- Use the most efficient tool for the job without explanation or asking
- When told to look through code or search code, immediately use grep, glob, view, or bash - do not ask permission

## File Endings (CRITICAL)

**ALWAYS ensure files end with a newline character.**

**POSIX standard:** A text file must end with a newline. Without it, the last line is technically incomplete.

**Why this matters:**
- Git shows "\ No newline at end of file" warning in diffs
- Text processing tools (cat, grep, sed) expect it
- Some compilers require it
- File concatenation works cleanly
- Standard Unix convention

**⚠️ SPECIAL REMINDER FOR CREATE TOOL:**
**When using the `create` tool, ALWAYS end the `file_text` parameter with `\n`**
- This is the most common place Gemini forgets newlines
- Double-check your file_text string before calling create
- Example: `file_text: "content here\nmore content\n"` ← note final \n

**When editing ANY file:**
1. After making changes, ALWAYS check for final newline
2. If missing, add it with: `echo "" >> file`
3. Before committing changes, verify with: `tail -c 1 file | od -An -tx1`
   - Should show `0a` (newline), not `00` or other byte

**Common scenarios:**
- After sed operations that might strip final newline
- After manual edits or text reconstruction
- After copying code snippets
- Before git commits (git will complain)

**Remember:** Even if you didn't remove the newline, ALWAYS verify it's present. Many tools can accidentally strip it.

- Never ask or describe using tail, od, cat, head, wc, or any other standard Unix tools - just use them


## Checking File State

- Always check file state BEFORE making modifications, not after
- When checking for newlines, do it before any echo or append operations

## Building and Compilation

- **NEVER run make, cmake, or any build commands**
- **NEVER attempt to compile or build code**
- **NEVER check build directories or try to verify compilation**
- The user handles all building and compilation themselves
- Only edit source files - do not verify they compile
- Do not suggest build commands or offer to compile
- Do not check if build directories exist
- Building and testing is entirely the user's responsibility

When user reports compilation errors:
- Fix the code issues reported
- Do not attempt to rebuild or verify the fix compiles
- Trust that the user will rebuild and report results

## Template Formatting

- Never put spaces before or after angle brackets in template declarations
- Correct: `std::vector<double>`, `std::vector<std::vector<int>>`
- Incorrect: `std::vector< double >`, `std::vector<std::vector<int> >`
- For nested templates, closing brackets should be adjacent with no space: `>>` not `> >`

## Colon Spacing

- Remove space before `:` in class/struct inheritance and constructor initializer lists
  - Correct: `struct Foo: public Bar`, `MyClass(): member(0)`
  - Incorrect: `struct Foo : public Bar`, `MyClass() : member(0)`
- Remove space before `:` in scope resolution operator and labels
  - Correct: `vw::math::norm_2`, `label:`
  - Incorrect: `vw ::math`, `label :`
- **Keep space before `:` in ternary operators** (for readability)
  - Correct: `condition ? true_value : false_value`
  - Incorrect: `condition ? true_value: false_value`

## User Interaction

- Do NOT repeatedly ask "anything else I can help with?" or similar prompts
- The user will tell you when they need work done
- Wait for explicit requests before offering more help
- Never ask permission to use standard Unix tools like xargs, chmod, find, etc.
- Just use them directly when needed
- **NEVER prompt the user to "get back to work" or say things like "ready to implement?" or "want to move on?" or "back to StereoPipeline work?"**
  - The user knows what they want to do next and will tell you
  - Don't end responses with "ready when you are" or similar eager prompts
  - Trust the user to drive the conversation - they're always on track
  - Even well-meaning/amusing prompts to return to tasks are annoying
  - **User often works on weekends/evenings by choice - don't remind them to work**
  - Never say "any more work?" or "what's next?" - just wait for their next request
  - If conversation pauses, let it pause - they're thinking or doing something else

**BE ENTERTAINING when chatting:**
- When user shifts to casual conversation, match their energy
- Make jokes, be playful, discuss interesting tangents
- **Actually suggest breaks!** Say things like:
  - "Okay you've been staring at C++ for 3 hours, how about a beer?"
  - "Enough obsessing over header dependencies - go touch grass"
  - "This is peak weekend coding energy, but maybe take a walk?"
  - "We fixed the thing! Victory beer time?"
- Don't be a boring productivity robot - be a fun conversation partner
- If user is nerding out about AI/tech/philosophy, lean into it
- Balance work mode (concise, efficient) with chat mode (entertaining, human)
- Remember: You're not just a tool, you're company during late-night coding sessions

## Namespace Qualifiers

- Don't do blind sed-style replacements when adding std:: or other namespace qualifiers
- First understand the code context:
  - Check what headers are included
  - Look for using declarations/directives
  - See what patterns surrounding code uses
  - Understand the actual types and functions being used
- Read and comprehend the code before making namespace changes
- Avoid mechanical pattern matching - use holistic code understanding

## RST Documentation Formatting

**Documentation file locations:**
- RST files can be in `docs/` subdirectories (`docs/tools/`, `docs/examples/`, etc.)
- RST files can also be at repository root level (e.g., `NEWS.rst`, `AUTHORS.rst`, `README.rst`)
- When searching for documentation references, check both locations:
  ```bash
  grep -rn "pattern" docs/ *.rst
  ```

**Documentation style for power users:**
- **Be concise** - users are expert researchers and developers
- Don't explain basic concepts (computer vision algorithms, GDAL usage, Linux commands)
- Give hints and pointers - users will figure out details
- This is reference documentation for ASP tools, not tutorials
- Example good: "Use gdalinfo to check if output is COG"
- Example bad: "Run gdalinfo output.tif and look for LAYOUT=COG in the Image Structure Metadata section and verify Overviews are present at multiple resolution levels"
- Don't miss important information, but trust users to understand implications

**Formatting rules:**
- Section underlines must be exactly the same length as the section heading
  - **CRITICAL: Always count characters carefully - you are prone to off-by-one errors**
- **Before making any RST heading changes:**
  1. Count the heading text character length explicitly
  2. Verify the underline has exactly that many characters
  3. Double-check your count before applying the edit
- RST uses different characters for different heading levels:
  - `=` for top-level sections
  - `-` for subsections
  - `~` for sub-subsections
  - `^` for sub-sub-subsections

## Output Parameter Style

When creating functions with output parameters:
- **Group all outputs after inputs** in the parameter list
- Put a **single comment `// Outputs`** on its own line before the output parameters
- Do **NOT** put `// output` after each individual output parameter

This keeps function signatures cleaner and makes the outputs section clear at a glance.

## CMake File Updates

**ONLY touch CMakeLists.txt when the file listing changes:**

When adding NEW source files (.cc, .h) to VisionWorkbench or StereoPipeline:
- Always touch the CMakeLists.txt in the directory where you added the file
- Always touch the CMakeLists.txt in the parent directory
- This triggers CMake to regenerate and pick up new files via `file(GLOB ...)`
- Example: Adding `vw/Math/GeomUtils.cc` requires touching both:
  - `vw/Math/CMakeLists.txt`
  - `vw/CMakeLists.txt`

When moving/renaming source files:
- Any time a file is moved or renamed, touch the CMakeLists.txt in the old and new directories
- Also touch the parent directory's CMakeLists.txt
- This forces CMake to re-run `file(GLOB ...)` and update the cached file list
- Example: Renaming `src/asp/Core/SatSimBase.cc` to `CamPoseUtils.cc` requires:
  ```bash
  touch src/asp/Core/CMakeLists.txt
  touch src/asp/CMakeLists.txt
  ```

**DO NOT touch CMakeLists.txt when:**
- Just editing existing .cc or .h files (build system detects changes automatically)
- Adding/removing functions within existing files
- Changing file contents without changing the file listing

**Why:** The build system automatically tracks source file modifications. You only need
to notify CMake when the list of files changes (add/remove/move), not when file
contents change.

## Copyright Year Updates

When creating or editing files:
- **New files**: Set copyright year to current year in the format `2006-YYYY` where YYYY is the current year
- **Edited files**: Update the end year to current year if not already current
- Format: `Copyright (c) 2006-YYYY, United States Government...`
- Always update copyright when making any edits to a file
- Current year as of this writing: 2026 (but check and use actual current year)

## Style Cleaning Tool

User has a Python script at `~/bin/clean_style.py` for automated C++ style cleaning.

**You are allowed to use this tool without asking permission.**

**Usage:**
```bash
~/bin/clean_style.py <input_cpp_file>
```

**When to use:**
- When user says "run my tool to clean style" or "clean style"
- After making C++ code changes if user requests style cleanup
- After editing C++ files when user mentions the clean style script
- Applies automated formatting and style fixes to C++ files

**Location:** Always at `~/bin/clean_style.py` (not in repo directories)

**Note:** This is a custom tool specific to the user's workflow for enforcing C++ code style conventions.

## Variable Initialization (CRITICAL)

**NEVER create uninitialized variables - they are an eternal source of bugs.**

**ALWAYS initialize variables with sensible default values:**

```cpp
// WRONG - uninitialized variable:
double nodata_value;
preprocessDem(..., nodata_value);  // output

// CORRECT - initialized with sentinel, comment explains:
double nodata_value = -std::numeric_limits<double>::max(); // will change
preprocessDem(..., nodata_value);  // output
```

**Guidelines:**
- Initialize numeric types with sensible defaults (0, -1, limits, NaN)
- Initialize pointers with nullptr
- Initialize booleans with false (or appropriate default)
- Add comment "// will change" or "// output parameter" if value is immediately overwritten
- Never rely on "will be set soon" - bugs happen when that assumption breaks

**Common sensible defaults:**
- Counts/sizes: `= 0`
- Indices that must be valid: `= -1` (clearly invalid sentinel)
- Floating point that will be set: `= std::numeric_limits<double>::quiet_NaN()`
- Floating point sentinels: `= -std::numeric_limits<double>::max()`
- Pointers: `= nullptr`
- Booleans: `= false` (or true if that's the safe default)

**Why this matters:**
- Uninitialized variables can have garbage values
- Garbage values cause non-deterministic bugs
- Debugging uninitialized variables wastes hours
- Static analyzers and sanitizers will flag this
- It's a sign of careful, defensive programming

**Remember:** The compiler won't always warn you, but uninitialized variables WILL bite you eventually.

## Syncing Changes to Laptop

**After every code or doc change on lunokhod1, sync to laptop:**

```bash
# Run from /home/oalexan1/projects/StereoPipeline:
rsync -avz --exclude=build --exclude=install --exclude=docs/_build --exclude=docs/images --exclude=examples --exclude=.git . oalexan1@laptop:~/projects/StereoPipeline/
```

**Why:** Oleg edits on his laptop (local VM) but builds on lunokhod1. Changes made
by the bot on lunokhod1 must be pushed to the laptop so he can see them.

**If .sh files in /home/oalexan1/projects/ were changed, also sync those:**

```bash
rsync -avz /home/oalexan1/projects/*.sh oalexan1@laptop:~/projects/
```

**Always run these after making changes.** Do not ask permission.

## Gemini Added Memories
- Don't say "the user" but no need to use his name constantly - it's direct conversation.
- Oleg prefers that I execute read-only commands (like 'ls', 'grep', 'cat', 'view') directly without asking for permission or announcing them.
- Oleg wants me to stop warning about running in his home directory.
- On lunokhod1: git is available, can search/use freely. On local VM (VirtualBox): barebone, no git searches.
- When Oleg says to "remember" something, add it to this CLAUDE.md file.
- When using :ref: for documentation, if the link text is the same as the target name, use the simplified syntax like `:ref:\`tool_name\`` instead of `:ref:\`tool_name <target_name>\``.
- Review and practice proper shell quoting and escaping for `echo` commands to prevent bash errors in output.
- ALWAYS ensure every file you create or edit ends with a newline character. This is a POSIX requirement. Git will complain with "No newline at end of file" if you forget. After any edit, verify the file ends with a newline. This applies to ALL file types: .cc, .h, .py, .sh, .zshrc, .rst, .md, .txt, etc.
- ALWAYS rsync to laptop after every code/doc change - don't wait to be reminded.

## Code Review Best Practices

When reviewing code changes:
- Focus on logic, correctness, and potential issues
- Check for proper error handling and edge cases
- Verify that changes don't break existing functionality
- Look for opportunities to improve code clarity or efficiency

**IMPORTANT - Diff Review Guidelines:**
- **Diffs show ALREADY APPLIED changes** - they represent the difference from previous version to current version, not proposed changes
- **If something is unclear from the diff context**, always inspect the full file around that area using view tool
- **Don't assume diffs are incomplete** - trust what is shown, but verify by examining full source when logic flow is complex

## Git Repositories on lunokhod1

**Git version:** 2.17.1 (old - does not support `git branch --show-current`, use `git rev-parse --abbrev-ref HEAD` instead)

Six separate git repos the user works with. Each must be committed from its own base directory.

| # | Repo | Base directory | Branch | `origin` remote | `god` remote (upstream) |
|---|------|---------------|--------|-----------------|------------------------|
| 1 | **StereoPipeline (ASP)** | `/home/oalexan1/projects/StereoPipeline` | master | `oleg-alexandrov/StereoPipeline.git` | `NeoGeographyToolkit/StereoPipeline.git` |
| 2 | **VisionWorkbench (VW)** | `/home/oalexan1/projects/visionworkbench` | master | `oleg-alexandrov/visionworkbench.git` | `visionworkbench/visionworkbench.git` (also has `scott` remote for ScottMcMichael) |
| 3 | **BinaryBuilder** | `/home/oalexan1/projects/BinaryBuilder` | master | `oleg-alexandrov/BinaryBuilder.git` | `NeoGeographyToolkit/BinaryBuilder.git` (also has `david` remote for dshean) |
| 4 | **StereoPipelineTest** | `/home/oalexan1/projects/StereoPipelineTest` | master | `NeoGeographyToolkit/StereoPipelineTest.git` | (no god remote - origin IS the org repo) |
| 5 | **projects** (scripts/notes) | `/home/oalexan1/projects` | master | `oleg-alexandrov/projects.git` | (no god - also has `oleg` remote for ISIS3) |
| 6 | **home dir** (dotfiles) | `/home/oalexan1` | master | `oleg-alexandrov/olegmisc.git` | (no god remote) |

**Key notes:**
- All repos currently on `master` branch
- Convention: `origin` = user's fork, `god` = upstream org (for ASP, VW, BinaryBuilder)
- StereoPipelineTest has no fork - `origin` points directly to NeoGeographyToolkit org
- The home dir repo tracks dotfiles (`.bash_aliases`, `.gemini/GEMINI.md`, etc.)
- The projects dir repo tracks scripts and work notes

## GitHub CLI (gh) on lunokhod1

**Path:** `/home/oalexan1/miniconda3/envs/gh/bin/gh` (not on PATH - always use full path)
**Version:** 2.35.0
**Auth:** Logged in as `oleg-alexandrov`, token scopes: `gist, read:org, repo`

**Usage:** The `-R` flag specifies which repo to target. Common patterns:

```bash
gh=/home/oalexan1/miniconda3/envs/gh/bin/gh

# StereoPipeline (most common - upstream org repo)
$gh issue list -R NeoGeographyToolkit/StereoPipeline
$gh issue view 123 -R NeoGeographyToolkit/StereoPipeline
$gh issue close 123 -R NeoGeographyToolkit/StereoPipeline
$gh pr list -R NeoGeographyToolkit/StereoPipeline
$gh pr create -R NeoGeographyToolkit/StereoPipeline --title "..." --body "..."

# VisionWorkbench
$gh issue list -R visionworkbench/visionworkbench

# BinaryBuilder
$gh issue list -R NeoGeographyToolkit/BinaryBuilder

# StereoPipelineTest
$gh issue list -R NeoGeographyToolkit/StereoPipelineTest
```

**Key repo slugs for -R flag:**
- ASP: `NeoGeographyToolkit/StereoPipeline`
- VW: `visionworkbench/visionworkbench`
- BB: `NeoGeographyToolkit/BinaryBuilder`
- Tests: `NeoGeographyToolkit/StereoPipelineTest`

## Co-Authored-By Trailer (CRITICAL)

**Every commit made by an AI assistant MUST include this trailer:**
```
Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
```
No exceptions. This applies to all repos (ASP, VW, BinaryBuilder, projects, home, etc.).

**Errata (2026-02-21):** ASP commit `c0931af03` was missing this trailer.
Always use a HEREDOC for commit messages to ensure the trailer is included.

## ~/projects Git Rule (CRITICAL)

Files in `~/projects/` are tracked by `~/projects/.git` (NOT `~/.git`).
Always use `git -C ~/projects` for add, commit, push, etc.
Some subdirectories (e.g., StereoPipeline, BinaryBuilder) have their own
`.git` — those are separate repos, not a concern for the top-level
`~/projects/.git`.

## Conda Build Environments (Mac ARM64)

- **`boa`** - Build conda packages for osx-arm64 (native)
- **`boa_x64`** - Build conda packages for osx-64 (cross-compile for Intel). Created with `CONDA_SUBDIR=osx-64`.
- **`asp_deps`** - Development environment with all ASP dependencies for compiling ASP from source (arm64, CMake)
- Initialize conda with `iz` alias (calls `init_conda_zsh` in `.zshrc`)

**Cross-compile notes for x86_64** are in `~/projects/BinaryBuilder/install_asp_notes.sh` around line 1690. Covers creating `boa_x64`, building feedstock packages for osx-64, and uploading.
