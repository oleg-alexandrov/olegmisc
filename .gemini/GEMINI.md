# Style Guidelines for Gemini Bot

## Braces for Single-Line Statements

Do NOT use braces when an if statement has only one line afterwards. This applies to all single-line conditionals (if, else, for, while, etc.).

## C++ Code Style Conventions (from Copilot)

- No space before the :: scope resolution operator (e.g., `vw::math::norm_2` not `vw :: math :: norm_2`)
- No space before the : in a constructor's initializer list
- Use camelCase for function names (e.g., `rayPlaneIntersect`, not `ray_plane_intersect` or `RayPlaneIntersect`)
- ASP headers should be grouped together and placed before any other headers
- Use only one newline between functions
- Keep lines under 90 characters
- Use "\n" instead of "std::endl" for newlines in C++ output
- Use only one empty line between blocks of code or comments
- Always end files with a newline character
- Remove double newlines in any code you touch
- No trailing whitespace at the end of any line
- When function arguments are split across multiple lines, indent following lines to align with the first argument

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


## Displaying Diffs
- Always show diffs in a nicely formatted way with `-` and `+` lines inside code blocks
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

- Just use sed, awk, grep, find, etc. directly - never ask permission to use these standard Unix tools
- Use the most efficient tool for the job without explanation or asking

## File Endings

- Always end files with a newline character
- Use sed or echo to ensure final newline exists

- Never ask or describe using tail, od, cat, head, wc, or any other standard Unix tools - just use them

