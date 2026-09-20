---
name: gdal-rasters
description: GDAL command-line raster handling and its sharp edges - gdalwarp/gdal_translate/gdalinfo mechanics for cropping, resampling, reprojecting, warping two rasters to a common grid, nodata masking, and turning a float raster into an 8-bit PNG quick-look. Carries the hard-won gotchas: never hardcode -srcnodata (let GDAL read it, or an unmasked nodata gets smeared by cubic resampling and blows up min/max = "washed out"), a float GeoTIFF needs NO normalization, always gdalinfo -mm/-stats a produced raster, and the PROJ/GDAL_DATA env needed for -t_srs. Load before any gdalwarp/gdal_translate/gdalinfo/gdaldem work, cropping or regridding a GeoTIFF, making a PNG from a raster, or when a raster looks washed out / flat / wrong. Complements visual-inspection (eyeballing/overlays) and asp-photogrammetry (ASP tools).
---

## The #1 gotcha: nodata + resampling = "washed out" (READ FIRST)

A produced GeoTIFF that displays washed out / flat grey (all mid-tone, no
contrast) is almost always **an unmasked nodata value smeared into the valid
pixels**, whose huge magnitude blows up the min/max so any auto-stretch view
collapses. The classic cause (burned 2026-09-20 on the OHRC/NAC maxlit):

    gdalwarp -srcnodata -3.4028235e38 -r cubic in.tif out.tif   # WRONG

The file's real nodata was `-3.4028227e38` (different mantissa). The hardcoded
value did not match, so nodata was NEVER masked; then `-r cubic` interpolated the
`-3.4e38` fill into the valid pixels along data edges, spraying +/-1e37 outliers.
Interior data was intact, but the outliers made `gdalinfo` report
`Min=-3.4e38 Max=+4.4e37` and every stretched view went flat.

RULES:
- **Never hardcode `-srcnodata`.** GDAL reads the nodata tag from the file
  automatically. Just `gdalwarp -te ... -tr ... -r cubic in.tif out.tif`.
- **Never resample (cubic/bilinear) across an UNMASKED nodata** - the fill value
  bleeds into real pixels. If you must set nodata, get the EXACT value from
  `gdalinfo` first (copy it verbatim), or use `-r near` (no interpolation).
- **Always `gdalinfo -mm` (or `-stats`) the output right after.** If min/max is
  wildly outside the expected data range, nodata got smeared - redo. This is the
  1-second cheap check that catches it (matches the CLAUDE.md "cheap checks on
  produced files" rule).

## Float is fine - do NOT normalize a float raster

A float GeoTIFF holding physical values (reflectance 0-0.14, elevation in meters,
disparity in px) is a correct, final product. Do NOT rescale/normalize it to
0-1 or 0-255 "to make it look better" - viewers auto-stretch for display, and
downstream tools want the real values. Normalization is ONLY for an 8-bit
**quick-look PNG**, never for the analysis raster itself.

## Making an 8-bit PNG quick-look (the ONE place you scale)

    gdal_translate -of PNG -ot Byte -scale <realmin> <realmax> 0 255 in.tif out.png

- Give EXPLICIT `<realmin> <realmax>` from the valid-data range (from
  `gdalinfo -stats` STATISTICS_MINIMUM/MAXIMUM, or a 2-98 percentile). Do NOT
  rely on bare `-scale` (auto min/max) - it includes nodata/outliers and washes
  the result out, the same failure as above.
- PNG has no nodata channel; nodata pixels just clamp to 0 or 255. Fine for a
  look; if it matters, mask first.

## Warp two rasters to ONE common grid (for overlay / diff / correlation)

To compare rasters apples-to-apples they must share extent, pixel size, and CRS:

    gdalwarp -overwrite -te <xmin> <ymin> <xmax> <ymax> -tr <dx> <dy> -r cubic \
      in.tif on_common_grid.tif

- `-te` is xmin ymin xmax ymax in the TARGET CRS; `-tr` is x/y pixel size.
- Same `-te`+`-tr` on both inputs -> identical grids -> pixel-for-pixel overlay.
- `-r cubic` for imagery, `-r near` for masks/label rasters, `-r average` or
  `cubicspline` when downsampling a DEM a lot. Then hand off to the
  [[visual-inspection]] red/green-overlay / hillshade procedure.
- Reading only the target window means warping a 1 GB source to a small `-te`
  box only touches the needed pixels - cheap, single-thread, head-node-safe.

## Reprojecting needs PROJ/GDAL_DATA - or the CRS is silently dropped

Symptom: `Warning/ERROR 1: PROJ: proj_create_from_name: Open of .../share/proj
failed`. GDAL cannot find its PROJ database, so `-t_srs` and CRS writing fail
(the geotransform/origin/pixel-size still survive, so a same-CRS crop is fine,
but the output loses its CRS string). FIX: point GDAL at a real PROJ/GDAL data
dir. On pfe the reliable ones (call gdal by absolute path or put its bin on PATH):
- ASP bundle: `SP=/vast_swbuild/swbuild/oalexan1/projects/BinaryBuilder/StereoPipeline`
  then `export GDAL_DATA=$SP/share/gdal PROJ_DATA=$SP/share/proj PROJ_LIB=$SP/share/proj`
  and `$SP/bin` on PATH (ASP ships GDAL).
- ISIS env: `/vast_swbuild/swbuild/oalexan1/miniconda3/envs/isis10asp/bin/gdalinfo`
  (GDAL/PROJ wired inside; but its `share/proj` has been missing - if so use the
  ASP bundle share above).
- On the Mac: `conda activate asp_deps`.
Set both `PROJ_DATA` and `PROJ_LIB` (GDAL/PROJ versions disagree on which).

## Head-node hygiene (pfe/Athena)

Command-line GDAL (`gdalinfo`, `gdal_translate`, `gdalwarp`, `gdaldem`) is
single-threaded by default, so big single-op streaming on a multi-GB raster is
FINE on the head node (see [[pfe-nas]]). Do NOT add `-multi`/`-wo NUM_THREADS`
there. `gdaldem hillshade` is GDAL (single-thread, head-node-safe); ASP's own
tools default to many threads and must be qsub'd or forced to `--threads 1`.

## Quick reference

- Inspect: `gdalinfo -stats -mm file.tif` -> Type, NoData, Min/Max, valid %,
  Size, Pixel Size, Corner Coordinates (first paren = projected meters, second
  = lon/lat; near a pole the lon/lat wraps and is confusing - reason in meters).
- Corners in projected meters: `gdalinfo f.tif | grep -E "Upper Left|Lower Right"`
  then take the FIRST parenthesis (`sed -E 's/^[^(]*\(([^)]+)\).*/\1/'`).
- Refresh stale stats without deleting the sidecar: just re-run with `-stats`
  (do NOT `rm` the `.aux.xml`; per CLAUDE.md, avoid variable-path rm).
