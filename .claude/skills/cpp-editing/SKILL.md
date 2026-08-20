---
name: cpp-editing
description: C++ and VisionWorkbench code editing conventions - header include ordering, comment preservation, code movement, brace/colon/forward-declaration style, ASP/VW library and namespace naming, output parameters, variable initialization, paired-list validation, and the clean_style tool. Load before editing any .cc/.h file in ASP, VW, or ISIS.
---

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

## Output Statements

- Do NOT remove vw_out() statements - these are for user-facing informational output, not debugging
- Only remove std::cout and other debug-specific output when asked

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
