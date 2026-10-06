---
name: sfs-delivery
description: >-
  Package the final Shape-from-Shading (SfS) deliverable for a site: the results
  directory layout, the canonical product set (blended SfS DEM, max-lit and average
  ortho mosaics, LOLA, weight, height-uncertainty, image-id lists, the two ortho
  directories, and the final jitter/bundle-adjust camera JSON), the shadow-masked
  average mosaic recipe, the filled inventory.yaml manifest, and the readme. Load
  when preparing an SfS delivery, building the average mosaic, assembling a results
  dir, writing inventory.yaml, or deciding which cameras and lists to ship. The stage
  AFTER sfs-run-align (SfS + re-registration + blend). Complements [[sfs]],
  [[sfs-run-align]], [[visual-inspection]].
---

# SfS delivery packaging

The last stage of the SfS pipeline: gather the produced terrain, orthos, lists, and
cameras into a self-describing results directory for handoff. The procedure is
codified in the SfsPipeline repo, not reinvented per site:

- `~/projects/SfsPipeline/WORKFLOW.md` step 10 (mosaics) and step 12 (delivery).
- `~/projects/SfsPipeline/inventory.yaml` is the delivery + provenance manifest
  template. A filled copy goes in the project dir when work is done.
- `~/projects/SfsPipeline/bin/blend_img_mosaic.sh` builds the average mosaic.

Heavy steps run on pfe via qsub (copies, mosaics, mapproject, camera gather). The
head node is only for list-building and the yaml/readme text. Gate each step on
`job_state=F`, then gdalinfo and eyeball every product before the next.

## The canonical delivery set (current, VIPER / SP / Mons Mouton)

Final results:
- `sfs_dem_blend.tif` - SfS DEM blended toward LOLA in deep shadow (`sfs_blend`).
- `max_lit_mosaic.tif` - max-lit ortho (`dem_mosaic --max`), brightest pixel per
  location. The matching ortho, the `sfs_blend` input, and the best smear detector.
- `average_mosaic.tif` - the shadow-masked seamless BLEND over the SfS images
  (customer request, ships alongside the max-lit one). See the recipe below.
- `height_uncertainty.tif` - height error in meters (optional, `estimError=1` pass).
  Run it on the FINAL blended SfS DEM, not LOLA: tile the blend, `launch_sfs_tiles.sh
  ... 1`, then mosaic the per-tile `-height-error.tif`. So it waits for a settled blend.

Visualizable: `sfs_dem_blend_hill.tif` (hillshade of the blend; ASP `hillshade -e 10`).

Auxiliary: `sfs_dem.tif` (raw SfS, pre-blend), `lola_1mpp.tif` (the regridded LOLA
domain DEM), `sfs_dem_weight.tif` (blend weight), `sfs_dem_blend_lola-diff.tif`
(geodiff vs LOLA).

Image lists: `bundle_adjust_image_ids.txt` (the full BA set) and `sfs_image_ids.txt`
(the SfS subset). The SfS set must be a subset of the BA set, and any dropped
offender must be absent from the SfS list.

Ortho directories (two, per inventory.yaml):
- `map_images/` - every delivered SfS ortho at 1 m/pixel, flat `<id>.map.tif`.
  (Only a very large tiled site like Mons Mouton uses per-tile subdirs; single-site
  deliveries are flat.)
- `map_images_native_res/` - the sub-1 m frames mapprojected at their own native
  GSD, plus `gsd.csv` (id, native GSD). Intersect the <1 m list with the shipped
  ortho set so this is the native cut of what ships. Build it with
  `mapproject_native_res.sh` (a loop over a paired image/camera list that mapprojects
  each at native GSD, no `--tr`, and writes `gsd.csv` from the actual output pixel
  sizes). The <1 m list comes from the per-image GSD query (`query_gsd.sh`).

Cameras (when the customer asks, e.g. Ross): `cameras/` with the final jitter (or
bundle-adjust) `adjusted_state.json` CSM model-state files, the linear-reduced set,
one per image. This is the inventory.yaml `final_bundle_adjust_prefix`. Mons Mouton
instead shipped `csm_init_cubes/` (cubes with the CSM baked in via csminit); bare
.json is cheaper and is what inventory.yaml expects.

Disparity illustration (optional, `sfs_to_lola_corr/`): the SfS-to-LOLA horizontal
disparity. Ship the AFTER (post-re-registration) dh/dv, colorized. The
before/after comparison stays in the notes.

## Ship only what was used; document the rest

Ship as images and cameras ONLY the set actually used to make the delivered DEM and
mosaics (the SfS subset). The larger bundle-adjust input set is documented in
`bundle_adjust_image_ids.txt` but its images and cameras are NOT shipped - no
dead weight. The readme and inventory.yaml must state plainly that the BA input was
larger and SfS used a subset. A dropped offender image is absent from both the
shipped set and the SfS id list. (The alternative, shipping the full BA set minus
the offender, is heavier and only warranted if the customer wants every pose.)

## Process a bigger region: crop to the ROI, or ship the full domain

SfS is run on a domain PADDED beyond the product ROI (footprints and jitter anchors
spill past the ROI). Two valid delivery choices, decided per site:
- Crop back to the product ROI. Because every product shares the domain grid, snap
  the ROI box OUTWARD to the domain pixel boundaries, then crop ALL products with
  that same box so they stay co-registered. On a pixel-aligned box the crop is a
  LOSSLESS pixel extraction - `gdal_translate -projwin ulx uly lrx lry`, no
  resampling. Get the ROI from the product polygon (`ogrinfo -so <roi>.gpkg`).
- Ship the full domain, uncropped, as insurance and to keep boundary artifacts out
  of the interior. Then INCLUDE the ROI polygon (and the projwin) so the customer
  can crop to the validated box themselves, and CAVEAT the edges: SfS-to-LOLA
  registration degrades toward the domain edge (larger shift there), so the interior
  ROI is the validated product and the outer pad is a lower-quality margin.
Record the choice and the edge caveat in the readme and inventory.yaml. Either way,
build and QC each product on the full domain first; the choice is independent of any
still-in-progress blend refine.

## Colorized disparity bands (the honest way)

When shipping dh/dv (or any signed-difference) bands for viewing, colorize with the
ASP `colormap` tool on a FIXED scale SYMMETRIC about zero
(`colormap --min -5 --max 5 band.tif -o band_color.tif`), the SAME scale and colour
polarity for every band and across the whole document, one subtraction order
throughout, nodata transparent. Shipping only the colorized GeoTIFF (no raw float)
is fine when the band is illustrative rather than quantitative. Any quantitative
read (median, MAD, valid-percent) goes in the readme, never burned into the image.
A robust NMAD can understate a real problem concentrated at the edges - say so in the
readme and let the colorized map show it. See [[visual-inspection]].

## The average (blend) mosaic - the shadow-masked recipe

The default `dem_mosaic` mode is a seamless blend (NOT `--max`, NOT `--mean`): with
`--nodata-threshold 0.005` every image's shadow pixels (reflectance at or below the
LRO NAC lit/shadow cutoff) are masked as nodata first, then the remaining lit pixels
feather gently across the mask boundary. A plain `--mean` cannot do this (it steps
wherever the contributing-image count changes). Build it over the SAME images the
max-lit mosaic used (the SfS set), then snap to the delivery grid:

```bash
# single-site (VIPER / SP / BCU): one blend over the SfS maps, then snap to the grid
blend_img_mosaic.sh lists/sfs_maps.txt average_mosaic_raw.tif 0.005 $(pwd) 28
regrid_to_grid.sh average_mosaic_raw.tif average_mosaic.tif \
  "<xmin ymin xmax ymax>" 1 $(pwd)
```

`blend_img_mosaic.sh` (the blend) and `regrid_to_grid.sh` (the grid snap) are the two
canonical SfsPipeline tools for the average mosaic; do not hand-roll `dem_mosaic` +
`gdalwarp`.

For a very large tiled site (Mons Mouton) blend in two levels - per tile, then
across the per-tile results - before the regrid:

```bash
dem_mosaic --nodata-threshold 0.005 --output-nodata-value -1e+6 \
  --dem-list <tile images> -o <tile>/blend_all.tif        # per tile
dem_mosaic --nodata-threshold 0.005 --output-nodata-value -1e+6 \
  --dem-list <per-tile blend_all.tif> -o blend_mosaic.tif  # across tiles
```

Verify the average mosaic grid matches `max_lit_mosaic.tif` EXACTLY (Size, Origin,
Pixel Size) and that shadows are masked with no seams and the same footprint.

## inventory.yaml - the manifest

Copy `SfsPipeline/inventory.yaml` into the results dir and fill real relative paths:
`base_terrain_path` (the LOLA domain DEM), `final_bundle_adjust_prefix` (the shipped
cameras), `sfs_terrain_path` + `_log`, `sfs_blend_terrain_path` + `_log`,
`sfs_blend_weight_path`, `sfs_height_error_path`, `max_lit_path`, `avg_mosaic_path`,
`orthophoto_dir_1mpp`, `orthophoto_dir_natural`, `orthophoto_natural_gsd_csv`, and
the two image-id lists (`ba_image_ids_path`, `sfs_image_ids_path`). Use the comment
fields to record the camera choice and why, any dropped/offender images, the crop to
the ROI, a still-in-progress blend, and known artifacts or caveats.

## readme.md

Mirror `sfs_viper_align/sfs_viper_align_results/readme.md`: a title and one paragraph
on the site (resolution, size, projection) and the re-registration to LOLA, then the
Final / Visualizable / Auxiliary / Lists / Ortho-dirs / Cameras sections, then
Technical details with the exact `sfs_blend`, `parallel_sfs`, and average-mosaic
commands for reproducibility. Keep shipped text self-contained: never reference a
private notes `.sh`, a scratch path, or an internal subdir name in text that leaves
the project. Keep it terse - match the prior delivery readmes (VIPER, SP), not a
verbose write-up.

The readme.md is text, not data, so it is version-controlled: keep it in the
results dir under the project tree and git add and push it (the heavy rasters,
orthos, and cameras are data and are never git-added). Treat the readme as the one
tracked description of the delivery.

## Camera and version discipline

- Ship the camera version that is in production. For a site re-registered with a
  jitter solve, that is the chosen jitter run (e.g. jitovr3-clean), NOT an
  experimental peer run (e.g. a padded-anchor jitovr5) even if it looks better, and
  NOT an un-jittered bundle-adjust prefix. Shipping the production version keeps the
  shipped DEM/mosaics consistent with the cameras (no redo).
- If an offender image was dropped from SfS, decide explicitly whether its camera
  ships (it is a valid pose for the BA set) or is excluded (if it is a known
  loose-knot smear). Record the decision in inventory.yaml and the readme.

## Naming and hygiene

- Copy real files into the results dir under the clean delivery names (`sfs_dem.tif`,
  not the internal `sfs_dem_jitovr3clean.tif`); do not ship internal symlink names.
- Build pyramids (`stereo_gui --create-image-pyramids-only`) on hillshades and
  mosaics so the customer can view them fast.
- Follow the project-workflow path and naming rules: state the one work dir once,
  report products relative to it, keep derived rasters next to their source.

## Prior deliveries (read the readme in each for the exact shipped set)

`sfs_viper_align/sfs_viper_align_results/` and `sfs_m2m_sp/sfs_m2m_sp_results/` are
the flat single-site template (average mosaic + two ortho dirs, no cameras shipped).
`sfs_mons_mouton/sfs_mons_mouton_results/` is the big tiled site (per-tile
map_images, csm_init_cubes cameras, a sfs_to_lola_corr alignment-illustration dir);
`sfs_mons_mouton/sfs_delivery_plan.sh` is the long-form packaging log. The current
active delivery is `sfs_BCU2314-BDU1224-MM/sfs_delivery_plan.sh`.
