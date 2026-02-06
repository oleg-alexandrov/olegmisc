# Style Guidelines for Gemini Bot

## FILE CREATION NEWLINE REQUIREMENT (CRITICAL)

**🚨 GEMINI REPEATEDLY FORGETS THIS: ALL FILES MUST END WITH NEWLINE 🚨**

**EVERY TIME you create a file with the `create` tool:**
1. **ALWAYS end the file_text parameter with `\n`**
2. **NO EXCEPTIONS - this is a POSIX requirement**
3. **Check your file_text - does it end with newline? If not, ADD IT**

**Examples:**
```
// WRONG - no newline at end:
file_text: "int main() {\n  return 0;\n}"

// CORRECT - ends with newline:
file_text: "int main() {\n  return 0;\n}\n"
```

**Why this is CRITICAL:**
- Git shows "\ No newline at end of file" warning
- Text processing tools (cat, grep, sed) expect it
- Some compilers require it
- File concatenation works cleanly
- Standard Unix convention

**MEMORY AID: Before calling create tool, mentally check "Does my file_text end with \\n?"**

## Line Boundary Calculations (CRITICAL)

**You and Gemini are both prone to off-by-one errors when deleting or extracting code blocks.**

**CRITICAL: This also applies to sed line ranges for bulk replacements!**

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

4. **For sed deletions** - always view first, delete second:
   ```bash
   sed -n 'START,ENDp' file.cc  # Preview what will be deleted
   # Check output carefully before proceeding
   sed -i 'START,ENDd' file.cc  # Only after verification
   ```

**Common mistakes to avoid:**
- Deleting the closing brace of the PREVIOUS function
- Including the opening of the NEXT function
- Missing nested closing braces at the end
- Trusting grep output without viewing context
- **Using sed line ranges that are too narrow** - if function body starts at line 891, don't use `901,1046s/.../'`, use `'891,1046s/.../'` or wider

**Remember:** One extra or missing line can break compilation. Always verify boundaries!

**For sed bulk replacements:**
- Always be generous with line ranges - it's safer to include a few extra lines than miss one
- If replacing within a function that starts at line N, use N as the start, not N+10
- Verify there are no stragglers just before or after your range
- Example of what NOT to do: Function body starts line 891, but using `'901,1046s/...'` misses line 900

## User's Workflow and Error Recovery (CRITICAL)

**The user commits after each successful change as safety checkpoints.**

**What this means:**
- User builds, tests, and commits after each of your changes
- User can easily rollback if you make mistakes
- This is their safety net, NOT an excuse for you to be less careful

**If you mess up:**
- **ASK the user to undo** with `git checkout file` or `git reset --hard`
- Don't try to "fix forward" by manually reverting your changes
- You might make it worse or miss something
- User can restore clean state faster than you can fix your mistakes

**Stay disciplined:**
- Follow all backup and verification strategies above
- Don't get reckless just because user has git rollback
- Aim for zero mistakes requiring rollback
- User's commits are last resort, not primary error handling

## Backup Strategy for Complex Operations (CRITICAL)

**NEVER use old backups when doing complex work - they may not have recent changes!**

**ALWAYS follow this strategy for complex operations (formatting, refactoring, bulk edits):**

1. **Make a fresh backup RIGHT BEFORE the complex operation:**
   ```bash
   cp file.cc file.cc.backup_$(date +%s)  # Timestamped backup
   ```

2. **Perform the complex operation** (sed, awk, rebuild from temp file, etc.)

3. **After completing the work, diff against the fresh backup:**
   ```bash
   diff -u file.cc.backup_TIMESTAMP file.cc
   ```
   - Verify ONLY the intended changes are present
   - Check for accidentally reverted prior work
   - Look for missing function renames or other recent modifications

4. **If unintended changes detected:**
   - Restore from the fresh backup immediately
   - Redo the operation more carefully
   - Never try to "fix forward" - start clean

**Why this matters:**
- Complex operations like "rebuild from temp file" can revert recent work
- Old temp files may have outdated function names, variable names, etc.
- Formatting fixes should never change logic or recent refactoring
- A fresh backup lets you verify you preserved all prior work

**Common scenarios requiring this:**
- Fixing spacing/indentation across a file
- Rebuilding a file from extracted sections
- Complex sed/awk multi-line operations
- Any operation that touches 50+ lines

**Remember:** One minute making a backup prevents hours recovering lost work!

**Backup file management:**
- Always move backups to `/tmp/` not user's repo directory (keeps repo clean)
- Periodically clean old backups from `/tmp/` to avoid disk clutter:
  ```bash
  ls -ltr /tmp/*.backup_* | head -20  # Review old backups
  rm /tmp/*.backup_* # Clean when accumulating
  ```

## Diff Display Convention

When showing code changes:
- Use **red (-)** for removed/old lines
- Use **green (+)** for added/new lines
- Follow standard diff format conventions

## Braces for Single-Line Statements

**CRITICAL: Remove braces from single-statement control flow blocks.**

Do NOT use braces when a control flow statement (if, else, for, while, do-while, etc.) has only one statement in its body.

**When editing code:**
- If you see a control flow block with braces containing only one statement, remove the braces
- Examples:
  ```cpp
  // WRONG:
  if (condition) {
    doSomething();
  }
  
  // CORRECT:
  if (condition)
    doSomething();
  
  // WRONG:
  for (int i = 0; i < n; i++) {
    process(i);
  }
  
  // CORRECT:
  for (int i = 0; i < n; i++)
    process(i);
  ```

**IMPORTANT: Keep braces for scope blocks**
- Do NOT remove braces from blocks that exist purely for scoping (not attached to if/for/while/etc.)
- These change semantics and must be preserved:
  ```cpp
  // CORRECT - Keep braces for scope control:
  {
    int temp = 5;
    doSomething(temp);
  } // temp goes out of scope
  ```

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
- **Option help text strings:** Wrap at ~90 characters per line. Split long help text into multiple quoted strings that are automatically concatenated:
  ```cpp
  ("left-bathy-mask", po::value(&global.left_bathy_mask),
    "Mask to use for the left image when doing bathymetry. Pixels classified as "
    "water must be either no data or have zero value in the mask, while land pixels "
    "must have positive value.")
  ```
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

**When removing `using namespace vw;` statements:**
1. Make a fresh backup first
2. Remove the using statements
3. Use sed carefully to add prefixes, but check for:
   - Don't add `vw::` if `vw::` already present (creates `vw::vw::`)
   - Don't add `vw::cartography::` if already present (creates double prefix)
   - **Don't forget type constructors**: `BBox2()` needs to become `vw::BBox2()`, not just `BBox2` in declarations
   - **Don't forget template instantiations**: `DiskImageView<double>` needs `vw::` prefix
4. Common sed patterns that miss things:
   - `s/\bBBox2\b/vw::BBox2/g` - misses `BBox2()` constructors, need `s/\bBBox2/vw::BBox2/g` (no \b at end)
   - `s/\bDiskImageView</vw::DiskImageView</g` - correct (matches template use)
5. Fix double prefixes: `sed 's/vw::vw::/vw::/g'` and `sed 's/vw::cartography::vw::cartography::/vw::cartography::/g'`
6. Run clean_style.py after
7. Diff against backup to verify only intended changes

**Specific patterns that trip up sed:**
- Type constructors: `BBox2()`, `Vector2()`, `Vector3()` - need prefix even when not in declaration
- Template variable declarations: `ImageView<T> var` vs `ImageView<T>(arg)` - both need prefix
- Comparisons with default-constructed types: `if (bbox != BBox2())` - the `BBox2()` needs prefix

## Project Context

- The StereoPipeline repository is at /home/oalexan1/projects/StereoPipeline
- The VisionWorkbench repository is at /home/oalexan1/projects/visionworkbench
- ASP stands for Ames Stereo Pipeline (refers to StereoPipeline)

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
  - Use `echo "Heading Text" | wc -c` to get character count (subtract 1 for newline)
  - Or manually count: "Example heading" = 15 characters needs 15 underline characters
  - Correct: 
    ```
    Multispectral image bands
    -------------------------
    ```
    (25 characters in heading = 25 dashes)
  - Incorrect:
    ```
    Multispectral image bands
    --------------------------
    ```
    (26 dashes for 25-character heading - TOO MANY)
  - Also Incorrect:
    ```
    Multispectral image bands
    ------------------------
    ```
    (24 dashes for 25-character heading - TOO FEW)
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

**Example:**
```cpp
void myFunction(int input1,
                double input2,
                std::string const& input3,
                // Outputs
                int& result1,
                double& result2,
                std::vector<int>& result3) {
  // function body
}
```

**NOT like this:**
```cpp
void myFunction(int input1,
                double input2,
                std::string const& input3,
                int& result1,        // output
                double& result2,     // output
                std::vector<int>& result3) {  // output
  // function body
}
```

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

## Gemini Added Memories
- The user prefers that I execute read-only commands (like 'ls', 'grep', 'cat', 'view') directly without asking for permission or announcing them.
- The user wants me to stop warning them about running in their home directory.
- The user prefers that I never search for git.


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


