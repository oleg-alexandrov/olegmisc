# projwin_fix: Work Notes

## Plan file: ~/projects/projwin_fix.sh

## Code changes made (2026-03-04, Mac ARM64)

### Files modified in StereoPipeline repo:
1. `src/asp/Core/CartographyUtils.h` - Added `snapBBox2ToGrid()` and
   `snapBBox3ToGrid()` with detailed comment about why floor/ceil not round
2. `src/asp/Tools/mapproject_single.cc` - Removed exclusive-max subtraction
   (`cam_box.max() -= current_resolution`), replaced inline round() snap
   with `asp::snapBBox2ToGrid()`, updated help text
3. `src/asp/Core/OrthoRasterizer.cc` - Replaced local `snap_bbox()` function
   with `asp::snapBBox3ToGrid()` (same floor/ceil logic, no behavior change)
4. `src/asp/Tools/dem_mosaic.cc` - Replaced inline floor/ceil with
   `asp::snapBBox2ToGrid()` in tap block (no behavior change)
5. `src/asp/Core/DemMosaicParse.cc` - Updated --t_projwin help text
6. `docs/tools/mapproject.rst` - Removed "Max is exclusive" from --t_projwin
7. `docs/tools/dem_mosaic.rst` - Removed "Max is exclusive" from --t_projwin
8. `NEWS.rst` - Added breaking change entry

### What NOT touched:
- gdal-tap paths (left alone)
- OrthoRasterizer cols/rows (round() there is fine, ratio is already integer)
- point2dem.cc help text (never said "exclusive")
- t_pixelwin (stays exclusive for tiling)

## Test results on Mac (AFTER first fix, before edge-to-center)

### TAP tests - ALL UNCHANGED (validated against gold):
- ss_mapproject_tap: 1088x999, Origin (12610941, 2580214.5) - PASS
- ss_point2dem_tap: 92x96, Origin (-191780, -2265250) - PASS
- ss_dem_mosaic_gdal_tap: 707x841, Origin (641392, 4133744) - PASS

### NON-TAP tests (first fix):
- **ss_dg_mapproject_bigdem: CHANGED** (expected!)
  - Before: 426x456, Origin (-1580615, -676595)
  - After:  427x457, Origin (-1580615, -676585)
  - projwin was: -1580610 -676590 -1576350 -681150
  - gdalinfo showed projwin offset by 0.5*gsd
- ss_point2dem_csv_proj4: 1000x1000, UNCHANGED - PASS
- ss_point2dem_projwin: 91x95, UNCHANGED - PASS
- ss_dem_mosaic_lonlat: 300x300, UNCHANGED - PASS

## Edge-to-center fix (2026-03-04, second round)

### Key insight: projwin specifies pixel EDGES (gdalinfo extent)
When gsd=1.0 and projwin=[-3.5, 6.5], internally ASP converts to pixel
centers [-3, 6] (add 0.5*gsd to min, subtract 0.5*gsd from max). Pixel
centers are at integer multiples of grid size. On write, the PixelAsArea
delta shifts back to [-3.5, 6.5] in gdalinfo. So gdalinfo = projwin
exactly when projwin is at half-gsd offsets from integer multiples.

### Additional code change in mapproject_single.cc:
When projwin is set AND not --gdal-tap, convert projwin edges to pixel
centers before snapping:
```cpp
if (opt.target_projwin != BBox2() && !opt.gdal_tap) {
    vw::Vector2 half(0.5 * current_resolution, 0.5 * current_resolution);
    cam_box.min() += half;
    cam_box.max() -= half;
}
asp::snapBBox2ToGrid(cam_box, current_resolution);
```
The pipeline's existing PixelAsArea delta (0.5*gsd) converts snapped
centers back to edges on output. No back-conversion needed.

For --gdal-tap: no conversion, snap projwin directly (edges at integer
multiples, delta=0).

### OrthoRasterizer.cc (point2dem): already had this logic (lines 640-641)
The existing code converts edges to centers when projwin is set and
--gdal-tap is off. No change needed.

### dem_mosaic: --tap does NOT create a new grid
dem_mosaic --tap snaps mosaic_bbox but keeps the input DEM's pixel grid
(georef not modified). Only --gdal-tap resets the georef origin to create
a new aligned grid. So the edge-to-center conversion does NOT apply to
dem_mosaic --tap. Left the snap in the --tap block as plain
snapBBox2ToGrid (no edge conversion).

### Test changes:
- **ss_dg_mapproject_bigdem/run.sh**: Changed projwin from integer multiples
  to half-gsd offsets: `-1580615 -676585 -1576345 -681155` (was
  `-1580610 -676590 -1576350 -681150`). Same image content (pixel centers
  unchanged at -1580610, ..., -1576350), but gdalinfo now shows EXACTLY
  the projwin values. Gold regenerated. Size 427x457, Origin (-1580615,
  -676585).

### Verified gdalinfo = projwin for half-gsd projwin:
- **mapproject**: ss_dg_mapproject_bigdem - gdalinfo UL(-1580615, -676585)
  LR(-1576345, -681155) = projwin exactly. PASS.
- **point2dem**: ss_point2dem_projwin - gdalinfo UL(-191775, -2265255)
  LR(-190865, -2266205) = projwin exactly. PASS.
- **dem_mosaic**: ss_dem_mosaic_gdal_tap covers --gdal-tap path. --tap
  cannot match projwin exactly (input grid determines output grid).

### All 7 tests PASS on Mac after final fix.

## Environment for running tests
```bash
# Mac
export ISISROOT=$HOME/anaconda3/envs/asp_deps
export PATH=~/projects/StereoPipeline/install/bin:~/projects/StereoPipelineTest/bin:$HOME/anaconda3/envs/asp_deps/bin:/usr/bin:/bin:/usr/sbin:/sbin

# lunokhod1
export ISISROOT=$HOME/miniconda3/envs/asp_deps
export PATH=~/projects/StereoPipeline/install/bin:~/projects/StereoPipelineTest/bin:$HOME/miniconda3/envs/asp_deps/bin:$PATH
```

## l1 Instructions

After fetching and building on lunokhod1:

1. **Fetch and build:**
   ```bash
   cd ~/projects/StereoPipeline
   git pull origin master
   make -C build -j16 && make -C build install
   ```

2. **Set environment:**
   ```bash
   export ISISROOT=$HOME/miniconda3/envs/asp_deps
   export PATH=~/projects/StereoPipeline/install/bin:~/projects/StereoPipelineTest/bin:$HOME/miniconda3/envs/asp_deps/bin:$PATH
   ```

3. **Fetch test repo:**
   ```bash
   cd ~/projects/StereoPipelineTest
   git pull origin master
   ```

4. **Run all 7 tests:**
   ```bash
   for t in ss_mapproject_tap ss_dg_mapproject_bigdem ss_point2dem_tap \
            ss_point2dem_csv_proj4 ss_point2dem_projwin \
            ss_dem_mosaic_gdal_tap ss_dem_mosaic_lonlat; do
     echo "=== $t ==="
     cd ~/projects/StereoPipelineTest/$t
     bash run.sh > output.txt 2>&1
     bash validate.sh
     echo
   done
   ```

5. **Expected results on l1 (gold from old release build):**

   **Tests that MUST NOT CHANGE on l1** (rerun and validate against old gold):
   The dem_mosaic code was reverted (no changes shipped). The point2dem
   OrthoRasterizer change is a pure refactor (local `snap_bbox` moved to
   `asp::snapBBox3ToGrid` with identical floor/ceil logic). These tests
   must produce identical output on l1 after fetch and build:
   - ss_mapproject_tap — tap path unchanged
   - ss_point2dem_tap — tap path unchanged
   - ss_dem_mosaic_gdal_tap — dem_mosaic code reverted, no changes
   - ss_dem_mosaic_lonlat — dem_mosaic code reverted, no changes
   - ss_point2dem_csv_proj4 — snap_bbox refactor only, no behavior change
   Verified on Mac: all five rerun with zero diff against gold.

   **Tests that WILL FAIL on l1 and need gold regenerated:**
   - **ss_dg_mapproject_bigdem**: projwin in run.sh changed to half-gsd
     offsets (`-1580615 -676585 -1576345 -681155`) and code now converts
     projwin edges to centers before snapping. Expected new output:
     427x457, Origin (-1580615, -676585), gdalinfo extent = projwin exactly.
     Regenerate gold: `cp run/run-DG.tif gold/`
   - **ss_point2dem_projwin**: NEW test, needs gold generated:
     `mkdir -p gold && cp run/run-DEM.tif gold/`
     Expected: 91x95, Origin (-191775, -2265255), gdalinfo = projwin.

6. **After regenerating gold, revalidate all 7.**

7. **Verify gdalinfo = projwin for half-gsd tests:**
   After gold is regenerated, confirm that gdalinfo UL/LR match projwin
   exactly for ss_dg_mapproject_bigdem and ss_point2dem_projwin:
   ```bash
   gdalinfo StereoPipelineTest/ss_dg_mapproject_bigdem/run/run-DG.tif | grep -E "Upper Left|Lower Right"
   # Should show: UL(-1580615, -676585) LR(-1576345, -681155)
   gdalinfo StereoPipelineTest/ss_point2dem_projwin/run/run-DEM.tif | grep -E "Upper Left|Lower Right"
   # Should show: UL(-191775, -2265255) LR(-190865, -2266205)
   ```
