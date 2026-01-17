# Style Guidelines for Gemini Bot

## Braces for Single-Line If Statements

**Do NOT use braces when an if statement has only one line afterwards.**

Good:
```cpp
if (dist >= 0)
  return cam->point_to_pixel(ecef_point);
```

Bad:
```cpp
if (dist >= 0) {
  return cam->point_to_pixel(ecef_point);
}
```

This applies to all single-line conditionals (if, else, for, while, etc.).

## C++ Code Style Conventions (from Copilot)

- **No space before the :: scope resolution operator** (e.g., `vw::math::norm_2` not `vw :: math :: norm_2`)
- **No space before the : in a constructor's initializer list**
- **Use camelCase for function names** (e.g., `rayPlaneIntersect`, not `ray_plane_intersect` or `RayPlaneIntersect`)
- **ASP headers should be grouped together and placed before any other headers**
- **Use only one newline between functions**
- **Keep lines under 90 characters**
- **Use "\n" instead of "std::endl"** for newlines in C++ output
- **Use only one empty line between blocks of code or comments**
- **Always end files with a newline character**
- **Remove double newlines in any code you touch**
- **No trailing whitespace at the end of any line**
- **When function arguments are split across multiple lines, indent following lines to align with the first argument**

## VisionWorkbench Namespace Conventions

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
- **NEVER include 'vw/Math/LeastSquares.h'** - this header does not exist and has been removed

## Project Context

- The StereoPipeline repository is at /home/oalexan1/projects/StereoPipeline
- The VisionWorkbench repository is at /home/oalexan1/projects/visionworkbench
- ASP stands for Ames Stereo Pipeline (refers to StereoPipeline)

## Output Statements

- **Do NOT remove vw_out() statements** - these are for user-facing informational output, not debugging
- **Only remove std::cout statements and other debug-specific output** when asked to remove debug statements

## File Operation Safety (CRITICAL)

**ALWAYS check if a file exists and view its contents before overwriting.**

Before writing to ANY file:
1. Check if it exists: `ls -l filename` or `cat filename`
2. View contents to understand what would be lost
3. Ask user for permission if overwriting existing content
4. Prefer `>>` (append) over `>` (overwrite) when adding to existing files

**NEVER use `>` (overwrite) without explicit confirmation from the user.**

This prevents accidental data loss of important configuration files or user data.
