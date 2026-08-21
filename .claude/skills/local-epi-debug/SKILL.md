---
name: local-epi-debug
description: Inspect and debug ASP local_epipolar (local_epi) stereo per-tile behavior - reproduce a single parallel_stereo tile faithfully, find/analyze notches (missing tiles) and edge loss, map shp tile_id <-> pixel box, compare DEMs correctly. Load whenever debugging local_epipolar stereo, tile notches, edge coverage, or per-tile correlation in Ames Stereo Pipeline.
---

# Debugging ASP local_epipolar (local_epi) stereo, per tile

Hard-won, keeps getting relearned. Read this before touching local_epi tile issues.

## How parallel_stereo tiles for local_epi (the facts)

- The image is split into **nominal 512x512 output tiles**. For local_epi (and any
  non-BM algorithm) each tile is **grown by a 128 px collar -> 768x768 padded box**
  (`grow_crop_tile_maybe`, `use_padded_tiles`). `--sgm-collar-size` is forced to 0
  per tile; the 128 collar is the tile padding, separate from SGM collar.
- Per-tile command `parallel_stereo` actually issues (`tile_run` in the source):
  `stereo_corr <stereo args> --sgm-collar-size 0 --corr-tile-size <padded=768>`
  `--trans-crop-win <padded box: nx-128 ny-128 768 768> --output-prefix-override <tiledir>/<name>`.
  So **corr-tile-size == the padded box size (768)**, NOT the default 1024 and NOT
  the nominal 512. `max_tile_size` inside stereo_corr = corr_tile_size_ovr = 768.
- Tile subdir naming: `<out_prefix>-<x>_<y>_<w>_<h>` where (x,y,w,h) is the NOMINAL
  512 tile (e.g. `run-512_46080_512_512`). The tile list is `<out_prefix>-dirList.txt`.
- **Tile-layout shapefile**: `<out_prefix>-tiles.shp` (+ `.qml` QGIS style). Field
  `tile_id` (integer). Polygon is the NOMINAL 512 tile box in the OUTPUT/disparity
  grid, y negative (e.g. tile 1066 = `POLYGON((512 -46080,1024 -46080,...))` =
  nominal box x[512,1024], y[46080,46592]). Grid is row-major; +1 = next tile right,
  +row_width = next row down (here 11 wide: 1055->1066 is +11).

## Map a projwin / DEM notch to a tile id

- Overlay `run-tiles.shp` on **`run-F_band3.tif`** (the F.tif validity/goodness mask,
  band 3) in QGIS - they share the OUTPUT grid, so the empty (notch) region sits
  under a specific tile box -> read its `tile_id`. Do NOT overlay the shp on
  `run-DEM.tif` (that is the PROJECTED result, a different grid).
- projwin (from stereo_gui on the DEM) locates the notch on the DEM; to get the
  tile, cross to the F_band3 grid via the shp, not the DEM.

## Reproduce ONE tile faithfully (two ways)

1. **`parallel_stereo --tile-id N --entry-point 1 --stop-point 2 ...`** (corr only;
   Step: pprc=0 corr=1 blend=2 rfne=3 fltr=4 tri=5). This is the official path but
   the tile-id worker branch does NOT create the subdir/symlinks itself - it needs
   `<prefix>-dirList.txt` AND the per-tile subdir with symlinked run-L/R/D_sub/masks
   already present (normally made by the main process `create_subdirs_symlink`). A
   completed run has cleaned those, so standalone `--tile-id` needs the setup redone.
2. **Manual replay (equivalent, quick)** - issues the exact `tile_run` command. In a
   fresh subdir of the big run dir (which already has run-L/R/D_sub/masks/align),
   symlink the shared `run-*` products, then:
   `stereo_corr --bundle-adjust-prefix ba/run --stereo-algorithm asp_mgm`
   `--num-matches-from-disparity 40000 --alignment-method local_epipolar`
   `--local-alignment-debug --corr-seed-mode 1 --sgm-collar-size 0`
   `--corr-tile-size 768 --threads 1 --trans-crop-win <nx-128> <ny-128> 768 768`
   `--output-prefix-override <subdir>/run  L.cub R.cub L.json R.json  <big_run>/run`
   Ready-made: `~/projects/ctx_jitter_study/debug_tiles.sh` (6 named shp tiles) and
   `tile_ab_test_aug21.sh` (uses the peer debug build; see below). ALWAYS clear the
   subdir first (`rm -rf <literal subdir>`) so no cached D.tif is reused.

## Metrics - the gotchas that cause wrong conclusions

- **Measure the CORE 512, not the padded 768 box.** The final DEM/F.tif keeps only
  the nominal 512 tile ( `[128:640, 128:640]` of the 768 D.tif). A padded-box % looks
  fine while the core is a notch. (Bit us: t1066 full 33% but core 26%.)
- **D.tif band 3 is the validity mask** (>0 = valid). VALID_PERCENT from gdalinfo
  can be 100% (no nodata) while band 3 is all zeros (all-invalid disparity) - i.e. a
  notch. Count `band3 > 0`, do not trust VALID_PERCENT alone.
- **F.tif band3 is POST-BLEND** (stereo_blend merges each tile's collar), so a raw
  per-tile D.tif does NOT equal the F.tif over that tile. Compare like with like.
- **Comparing two DEMs for coverage: pixel-align with `gdalwarp -tap`** (targetAligned
  Pixels) to a common `-te/-tr/-t_srs`. Without `-tap`, point2dem's slightly different
  auto-extents misalign the grids and the occupancy diff is GARBAGE (this falsely
  showed valid-aware "eroding" an edge for a whole night). See `eval_occupancy.sh`,
  `render_4way.sh`, `find_notch.sh` in `~/projects/ctx_jitter_study/`.

## Why local_epi tiles notch (observed causes)

- **Low-contrast / textureless tile**: `--local-alignment-debug` prints
  `Left image value bounds: [lo hi]`. A near-flat range (e.g. [0.108, 0.154]) means
  the aligned tile has almost no texture -> MGM finds no valid matches -> all-invalid
  disparity (the notch). Common on smooth terrain and footprint-edge tiles. (This is
  what the 2.5*NMAD per-tile clamp/normalization tried to fix - it worked on the flat
  tile but added noise everywhere, so it was reverted.)
- **No interest points in the base box**: attempt 1 (768 box) can get `Num ip from
  D_sub: 0`, `Num global ip: 0` -> empty right box -> `0x0 dataset illegal` -> fail;
  the naive code then retries with a bigger box (see history).
- **Search-range blow-up**: a bigger blind box can pull in a wide disparity spread
  (e.g. `Min and max disparities: -57, 62`) that MGM cannot resolve -> few valid px.

## Code history (which commit does what)

- `ac809529a` (naive): blind `grow_box_to_square_with_constraint`, 3 passes at
  `left/right_extra_factor` 1.0/2.0/4.0, no valid-aware, no clamp, no skip. This is
  the "wall fine except ONE notch tile" version.
- `5ae53cb8b` (regression source): introduced BOTH valid-data-aware tiling AND the
  2.5*NMAD per-tile clamp. The clamp adds noise; the valid-aware box was suspected of
  edge erosion (that suspicion came from the non-`-tap` occupancy bug, so re-check).
- To reproduce/debug the naive notch, use the `ac809529a` code.

## Peer debug build (isolate from a running production build)

Do NOT overwrite the pfe build that a running job (e.g. jitter) uses. Make a PEER:
extract the latest release tarball into
`/home6/oalexan1/projects/BinaryBuilder/StereoPipeline_aug21` (peer to
`StereoPipeline/`), then overlay the l1-compiled code: rsync l1 `install/lib/` ->
`.../lib/`, `install/bin/` -> `.../libexec/` (FULL dirs, no --delete - keeps the
release's isis tools). Point debug scripts' PATH at `StereoPipeline_aug21/bin`.
NOTE: the `--version` Build ID can be a STALE embedded string (from the last cmake
configure), not the compiled commit - do not trust it; trust the source you built.

## Give the user a one-tile command to confirm

Hand them the exact `stereo_corr ... --trans-crop-win <nx-128> <ny-128> 768 768 ...`
line (with `--local-alignment-debug`) for the tile in question so they can run it and
see the same value bounds / disparities / D.tif you do.
