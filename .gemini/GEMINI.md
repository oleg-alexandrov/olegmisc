# Style Guidelines for Gemini Bot

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

- No space before the :: scope resolution operator (e.g., `vw::math::norm_2` not `vw :: math :: norm_2`)
- No space before the : in a constructor's initializer list
- Use camelCase for function names (e.g., `rayPlaneIntersect`, not `ray_plane_intersect` or `RayPlaneIntersect`)
- ASP headers should be grouped together and placed before any other headers
- Use only one newline between functions
- Keep lines under 90 characters - break long lines with proper indentation
- When breaking lines, indent continuation lines to align with the start of the expression or function arguments
- Use "\n" instead of "std::endl" for newlines in C++ output
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

## Project Context

- The StereoPipeline repository is at /home/oalexan1/projects/StereoPipeline
- The VisionWorkbench repository is at /home/oalexan1/projects/visionworkbench
- ASP stands for Ames Stereo Pipeline (refers to StereoPipeline)

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

## File Endings

- Always end files with a newline character
- Use sed or echo to ensure final newline exists

- Never ask or describe using tail, od, cat, head, wc, or any other standard Unix tools - just use them


## Checking File State

- Always check file state BEFORE making modifications, not after
- When checking for newlines, do it before any echo or append operations

## Building and Compilation

- NEVER run make, cmake, or any build commands
- NEVER attempt to compile or build code
- The user handles all building and compilation themselves
- Only edit source files - do not verify they compile

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
- **NEVER prompt the user to "get back to work" or say things like "ready to implement?" or "want to move on?"**
  - The user knows what they want to do next and will tell you
  - Don't end responses with "ready when you are" or similar eager prompts
  - Trust the user to drive the conversation - they're always on track
  - Even well-meaning/amusing prompts to return to tasks are annoying

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

- Section underlines must be exactly the same length as the section heading
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
    (26 dashes for 25-character heading)
- RST uses different characters for different heading levels:
  - `=` for top-level sections
  - `-` for subsections
  - `~` for sub-subsections
  - `^` for sub-sub-subsections

## CMake File Updates

When adding new source files (.cc, .h) to VisionWorkbench or StereoPipeline:
- Always touch the CMakeLists.txt in the directory where you added the file
- Always touch the CMakeLists.txt in the parent directory
- This triggers CMake to regenerate and pick up new files via `get_all_source_files()`
- Example: Adding `vw/Math/GeomUtils.cc` requires touching both:
  - `vw/Math/CMakeLists.txt`
  - `vw/CMakeLists.txt`

## Copyright Year Updates

When creating or editing files:
- **New files**: Set copyright year to current year in the format `2006-YYYY` where YYYY is the current year
- **Edited files**: Update the end year to current year if not already current
- Format: `Copyright (c) 2006-YYYY, United States Government...`
- Always update copyright when making any edits to a file
- Current year as of this writing: 2026 (but check and use actual current year)

## Style Cleaning Tool

User has a Python script at `~/bin/clean_style.py` for automated C++ style cleaning.

**Usage:**
```bash
~/bin/clean_style.py <input_cpp_file>
```

**When to use:**
- When user says "run my tool to clean style" or "clean style"
- After making C++ code changes if user requests style cleanup
- Applies automated formatting and style fixes to C++ files

**Note:** This is a custom tool specific to the user's workflow for enforcing C++ code style conventions.
