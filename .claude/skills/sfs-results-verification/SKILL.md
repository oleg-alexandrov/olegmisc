---
name: sfs-results-verification
description: >-
  Final verification pass to make an SfS delivery bulletproof for submission: confirm
  every product named in the readme and inventory.yaml actually exists with that exact
  name, counts match the id lists, no stray files remain (sub-resolution pyramids, gdal
  .aux.xml, run logs, temp previews), all rasters share the delivery grid, the whole
  directory tree is world-readable with a traversable permission chain from / down, and
  the key products pass a quick visual sanity check. Load before handing off or
  submitting an SfS results directory, or when asked to verify, QC, or bulletproof a
  delivery. The stage AFTER sfs-delivery (which assembles the package). Complements
  [[sfs]], [[sfs-delivery]], [[visual-inspection]].
---

# SfS results verification (make the delivery bulletproof)

The last gate before a delivery is handed off or submitted. The package was assembled
by [[sfs-delivery]]; this skill verifies it is complete, correct, clean, readable, and
sane. Run every check below against the actual delivery directory on disk.

## 1. Every named path exists (readme + inventory.yaml)

The two manifests are `readme.md` (human description) and `inventory.yaml` (the
machine manifest, whose required fields come from the template in
`~/projects/SfsPipeline/inventory.yaml`). A customer finds products by the names in
these two files, so EVERY name they reference must exist in the delivery with that
exact spelling. Test each one:

```bash
cd <site>_results
for p in lola_1mpp.tif sfs_dem.tif sfs_dem_blend.tif sfs_dem_blend_hill.tif \
         sfs_dem_weight.tif sfs_dem_blend_lola-diff.tif height_uncertainty.tif \
         max_lit_mosaic.tif average_mosaic.tif bundle_adjust_image_ids.txt \
         sfs_image_ids.txt product_roi.gpkg inventory.yaml readme.md cameras \
         map_images map_images_native_res map_images_native_res/gsd.csv \
         sfs_to_lola_corr/dh_color.tif sfs_to_lola_corr/dv_color.tif; do
  [ -e "$p" ] && echo "OK   $p" || echo "MISS $p"
done
```

Build the list from THIS delivery's actual readme/inventory, not a fixed list. There
must be no MISS, and no dangling reference in the prose (grep the readme for each file
name you ship). Counts must agree with the id lists: `cameras/` and `map_images/`
equal the SfS id count, `map_images_native_res/` equals the sub-1 m count.

## 2. No stray files (pyramids, aux, logs, previews)

A submission must not carry build clutter. Remove from the whole delivery tree:
- sub-resolution pyramids `*_sub*.tif` (stereo_gui overviews),
- gdal stats sidecars `*.aux.xml`,
- run logs (`log_*.txt`, `*-log-*.txt`, `*.log`),
- any temp preview PNGs.

```bash
find <site>_results -name '*_sub*.tif' -delete
find <site>_results -name '*.aux.xml'  -delete
find <site>_results \( -name 'log_*.txt' -o -name '*-log-*.txt' \) -delete
# verify: each count is 0 afterward
find <site>_results \( -name '*_sub*.tif' -o -name '*.aux.xml' -o -name 'log_*.txt' \) | wc -l
```

(These strays live on the product host, not in git, so this is a disk cleanup, nothing
to commit. The tracked text - readme, inventory, id lists, gsd.csv - stays.)

## 3. Grid consistency

Every delivered raster must share one grid (so they overlay pixel for pixel).
`gdalinfo` each and confirm identical Size, Origin, Pixel Size (e.g. 19491 x 15467,
origin half-integer, 1 m). The average mosaic in particular must match the max-lit
mosaic exactly.

## 4. Permission chain all the way to / (the customer must be able to READ it)

A perfect package is useless if the customer cannot traverse to it. EVERY directory
from `/` down to the delivery must be world-traversable (`o+rx`), and every delivered
dir must be `o+rx` and every file `o+r`:

```bash
# traversal chain: ls -ld each path component from / to the delivery
p=""; for part in $(echo $D | tr '/' ' '); do p="$p/$part"; ls -ld "$p"; done
# contents readable
find $D -type d ! -perm -o+rx | wc -l   # must be 0
find $D -type f ! -perm -o+r  | wc -l   # must be 0
```

Building with `umask 022` gives `o+r`/`o+rx` for free. If a parent dir (often the user
`/nobackup/<user>` home) is `700`, the customer cannot traverse: do NOT blindly
`chmod` a private parent (it exposes everything under it) - flag it and resolve via the
intended sharing path (a group, or a copy to a shared delivery location). Fix the
delivery subtree itself with `chmod -R o+rX` if any file is not readable.

## 5. Visual sanity of the key products

Eyeball the important products before sign-off (see [[visual-inspection]]). Make small
quicklook PNGs into a TEMP dir (never into the delivery), fetch, look, then delete the
temps:
- `sfs_dem_blend_hill.tif` - crisp terrain, no tile seams, no bubbles or blunders.
- `max_lit_mosaic.tif` - full coverage, no camera smears or streaks, no interior gaps.
- `average_mosaic.tif` - shadow-masked, seamless.
- `height_uncertainty.tif` - tiny values (sub-decimeter) over most of the site, larger
  on rims and in shadow (scale it ~0..0.5 m to see structure, not 0..5).
- `sfs_to_lola_corr/dh_color.tif` / `dv_color.tif` - uniform red/blue salt-and-pepper
  with NO coherent colored region (a coherent patch means a residual registration
  shift). Shadowed craters read as smooth nodata blobs.

```bash
gdal_translate -of PNG -outsize 1400 0 <product> /tmp/ql.png   # scale floats with -scale
# fetch, Read to eyeball, then: rm /tmp/ql*.png on both ends
```

Remove all temp preview data from both the product host and the local machine when
done - it is not part of the delivery.

## Self-contained, tracked manifests

The delivery is one self-contained directory (per [[sfs-delivery]]); `readme.md` and
`inventory.yaml` are the version-controlled description and belong in git (the heavy
rasters/orthos/cameras do not). The inventory's required fields are defined by
`~/projects/SfsPipeline/inventory.yaml`; keep the filled copy consistent with it.
