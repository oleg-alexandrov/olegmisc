---
name: asp-geoids
description: >-
  Geoids in Ames Stereo Pipeline (MOLA Mars areoid, Earth EGM96, EGM2008, NAVD88) -
  the dem_geoid tool, geoid rasters in share/geoids/, ConstantEdgeExtension polar interpolation,
  unpadded [-180, 180] x [-90, 90] bounds, the geoids.tgz tarball release on StereoPipeline,
  the geoid-feedstock conda package, and updating the asp_deps environments across platforms.
---

# Geoids in Ames Stereo Pipeline (ASP)

This skill covers the geoid models, interpolation engine, raster bounds, packaging, and deployment pipeline for Ames Stereo Pipeline's `dem_geoid` tool.

## 1. Supported Geoids & Datums

ASP supports four standard planetary and terrestrial geoids:
- **Mars MOLA Areoid (`mola_areoid.tif`)**:
  - Datum: Mars (`D_MARS`, sphere radius 3,396,190 m).
  - Grid: 5760 x 2880 pixels, 0.0625 degree (1/16 deg) resolution.
  - Coverage: `[-180, 180]` longitude, `[-90, 90]` latitude.
  - Data format: Float32 GeoTIFF, heights in meters, nodata = -32767.
- **Earth EGM96 (`egm96-5.jp2`)**:
  - Datum: WGS84 (`WGS_1984`). Default geoid for Earth DEMs.
  - Grid: 4320 x 2160 pixels, 1/12 degree (5 arcminute) resolution.
  - Coverage: `[-180, 180]` longitude, `[-90, 90]` latitude.
  - Data format: UInt16 JPEG2000 (lossless). `dem_geoid.cc` decodes pixel values via:
    `height = ((86 - (-108)) / 65534.0) * (pixel - 0) + (-108)`.
- **Earth EGM2008 (`egm2008.jp2`)**:
  - Datum: WGS84 (`WGS_1984`). High-resolution Earth model.
  - Grid: 8640 x 4320 pixels, 2.5 arcminute resolution.
  - Interpolation engine: Evaluated via NGA Fortran routine (`interp_2p5min.f`) compiled into `libegm2008.so` / `libegm2008.dylib`.
- **North America NAVD88 (`navd88.tif`)**:
  - Datum: NAD83 (`North_American_Datum_1983`).
  - Grid: 4320 x 2160 pixels, 1/12 degree resolution.
  - Coverage: `[-180, 180]` longitude, `[-90, 90]` latitude.
  - Data format: Float32 GeoTIFF, heights in meters, nodata = -32767.
- **Custom Geoids (`--geoid-path <file.tif>`)**:
  - Any user-supplied GeoTIFF with geoid heights in meters.

## 2. Polar Interpolation & Edge Extensions (Issue #283 Fix)

Historical bug and resolution:
- **Root Cause**: `dem_geoid.cc` previously used `ZeroEdgeExtension()` with bicubic interpolation. Querying pixels at or near $\pm 90^\circ$ latitude read zeros outside the boundary, causing zero-poisoning and a 24.3% nodata hole at the poles on honest unpadded grids.
- **Crude Padding Flaw**: Earlier builds padded rasters by 5 rows/cols using row replication. At the South Pole, where the areoid has a natural tilt, this replication created an abrupt 1-meter vertical tear (-0.43 m to +0.55 m jump) right across the pole.
- **The Fix**: `dem_geoid.cc` uses `ConstantEdgeExtension()`. It repeats the nearest boundary pixel instead of zeros, preserving bicubic interpolation everywhere, delivering 100% valid coverage across the poles, and eliminating the South Pole tear.
- **Clean Bounds**: All shipped rasters are cropped to honest `[-90, 90]` latitude and rolled to standard `[-180, 180]` longitude. This permanently eliminates the GDAL and VisionWorkbench non-normal georeference warnings.
- **Longitude Wrapping**: `GeoReference::lonlat_to_point()` automatically wraps input longitudes toward the image center, so both `[0, 360]` and `[-180, 180]` inputs are seamlessly handled.

## 3. Storage & Packaging Pipeline

Geoid data files are packaged and distributed through three layers:

### Layer A: Upstream Tarball Release (`geoids.tgz`)
- Repository: `NeoGeographyToolkit/StereoPipeline` releases.
- Canonical tag: `geoid2.0` (supersedes `geoid1.0`).
- Archive structure: Top-level `geoids/` directory containing:
  - `egm2008.jp2`
  - `egm96-5.jp2`
  - `interp_2p5min.f`
  - `mola_areoid.tif`
  - `navd88.tif`

### Layer B: Standalone Conda Package (`geoid-feedstock`)
- Repository: `NeoGeographyToolkit/geoid-feedstock`.
- Recipe: `recipe/meta.yaml` and `recipe/build.sh`.
- Package name: `geoid` on Anaconda channel `nasa-ames-stereo-pipeline`.
- Builds: Compiles `interp_2p5min.f` into `$PREFIX/lib/libegm2008.{so,dylib}` and copies rasters to `$PREFIX/share/geoids/`.

### Layer C: ASP Dependencies & Bundled Distribution
- **BinaryBuilder (`Packages.py`)**: `class geoid` downloads `geoids.tgz`, compiles `libegm2008`, and installs into `share/geoids`.
- **stereo-pipeline feedstock (`recipe/build.sh`)**: Downloads `geoids.tgz`, compiles `libegm2008`, and installs into `$PREFIX/share/geoids`.
- **ASP deps tarballs (`asp_deps_*`)**: Prebuilt dependency tarballs on `NeoGeographyToolkit/BinaryBuilder` releases include `share/geoids/` and `libegm2008`.

## 4. Runbook: Updating Geoid Assets

When releasing new or updated geoid rasters:

1. **Prepare Clean Rasters**:
   - Ensure extent is exactly `[-180, 180]` longitude and `[-90, 90]` latitude.
   - Verify `gdalinfo` shows no non-normal georeference warnings.
   - Pack into `geoids.tgz` with a top-level `geoids/` directory:
     ```bash
     mkdir -p geoid_pack/geoids
     cp interp_2p5min.f egm2008.jp2 egm96-5.jp2 mola_areoid.tif navd88.tif geoid_pack/geoids/
     cd geoid_pack
     COPYFILE_DISABLE=1 tar czf geoids.tgz geoids
     shasum -a 256 geoids.tgz
     ```

2. **Publish GitHub Release**:
   - Release on `NeoGeographyToolkit/StereoPipeline` with a new incremented tag (e.g. `geoid2.0`). Do not overwrite older releases.
   - Attach `geoids.tgz`.

3. **Update `geoid-feedstock` & Build for All Four Platforms**:
   - Update `recipe/meta.yaml` with the new release tag URL and sha256 checksum.
   - Increment package version and build number (e.g. version `asp3.7.0`, build 2).
   - Build for all four platforms:
     * `osx-arm64`: Run `conda-build` locally on Mac mini (arm64).
     * `osx-64`: Run `CONDA_SUBDIR=osx-64 conda-build` on Mac mini.
     * `linux-64`: Run modern `conda-build` on `lunokhod1` (using `cassis_build` env).
     * `linux-aarch64`: Run `conda-build` inside the `asp_arm` Docker container (`/projects` mount).
   - Upload each `.conda` package to Anaconda channel `nasa-ames-stereo-pipeline`:
     ```bash
     anaconda upload -u nasa-ames-stereo-pipeline <package>.conda
     ```
   - Verify on the channel:
     ```bash
     curl -s https://api.anaconda.org/package/nasa-ames-stereo-pipeline/geoid | jq '.files[] | select(.version=="asp3.7.0") | {subdir: .attrs.subdir, basename: .basename}'
     ```

4. **Update Consumer Recipes & Environments**:
   - In `BinaryBuilder/Packages.py` (`class geoid`): Update URL and sha1 checksum.
   - In `stereopipeline-feedstock/recipe/build.sh` and `build_from_source.sh`: Update URL.
   - Install `geoid` package into `asp_deps` across local and remote environments:
     * `lunokhod1`: `~/miniconda3/envs/asp_deps`
     * Mac mini: `asp_deps` and `asp_deps_x64`
     * Docker: `asp_arm` `/opt/conda/envs/asp_deps`

5. **Update Remote CI Dependency Tarballs (`BinaryBuilder` Releases)**:
   - For `asp_deps_mac_arm64_v4`, `asp_deps_mac_x64_v4`, and `asp_deps_linux_arm_v1`:
     Unpack `asp_deps_p1.tar.gz`, copy updated `share/geoids/` and `libegm2008`, and re-tar without leading path prefix.
     **CRITICAL: Always enable `shopt -s dotglob` before `tar -czf ... *`**:
     A bare shell glob `*` skips hidden files (`.*`). In conda environments that track a root `.condarc` (e.g. `asp_deps_x64`), omitting `.condarc` causes `conda-unpack` on the CI runner to abort with `FileNotFoundError: .../.condarc`. This aborts prefix rewriting midway and breaks `git` (`git: 'remote-https' is not a git command`).
     ```bash
     cd env_dir
     shopt -s dotglob
     COPYFILE_DISABLE=1 tar -czf ../asp_deps_p1.tar.gz *
     cd ..
     tar -tzf asp_deps_p1.tar.gz | grep "^\.condarc$" # verify if original had it
     gh release upload <tag> asp_deps_p1.tar.gz -R NeoGeographyToolkit/BinaryBuilder --clobber
     ```
   - For `asp_deps_linux_v2` (linux intel):
     Run `conda-pack` on `lunokhod1` from `~/miniconda3/envs/asp_deps`:
     ```bash
     ~/.local/bin/conda-pack -p ~/miniconda3/envs/asp_deps -o asp_deps.tar.gz --force \
       --ignore-missing-files --ignore-editable-packages --n-threads -1 \
       --exclude "lib/libVw*" --exclude "lib/libAsp*" \
       --exclude "include/vw/*" --exclude "include/asp/*"
     split -b 1900M -d -a 1 asp_deps.tar.gz asp_deps_p
     mv asp_deps_p0 asp_deps_p1.tar.gz
     mv asp_deps_p1 asp_deps_p2.tar.gz
     gh release upload asp_deps_linux_v2 -R NeoGeographyToolkit/BinaryBuilder \
       asp_deps_p1.tar.gz asp_deps_p2.tar.gz --clobber
     ```
