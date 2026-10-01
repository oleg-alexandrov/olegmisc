---
name: sfs-image-selection
description: >-
  Select a minimal-but-covering SUBSET of images for Shape-from-Shading after the cameras
  are refined and pruned. Covers the ASP image_subset recipe: break the terrain into
  overlapping quadrants, group by Sun azimuth (50-150 images), build low-resolution
  mapprojected sub images, run image_subset per group, and a second pass for 2x coverage.
  Load when choosing which images to feed SfS, running image_subset, preparing low-res sub
  images, splitting a site into quadrants, or thinning a large mapproj set to a
  representative cover. The output lists are mapprojected images (with the latest bundle
  cameras); converting them to cub+cam lists for the actual SfS is a later step.
---

# SfS image selection (coverage subset via image_subset)

Early in SfS one coregisters a very large, illumination-diverse image set (bundle/jitter).
For the SfS solve itself, using all of them is prohibitive, so pick a representative SUBSET
that reproduces almost the same ground coverage. Do this AFTER the cameras are refined and
the whacky ones pruned (see [[sfs-post-bundle-eval]]). The selection runs on the
mapprojected images made with the latest (pruned) bundle cameras; later they are mapped
back to cub+cam lists for SfS.

## The recipe (ASP image_subset, :numref:`image_subset`)

image_subset picks, greedily, the image contributing the most pixels at/above a threshold,
then the next-most-additional, etc., writing a ranked list "image_path count". It is SLOW
(O(n^2 * output pixels)) and reads each image fully into memory, so it wants at most
~100-200 small images per call. Therefore decompose first:
1. QUADRANTS with overlap - split the box into 4 overlapping quadrants; process each
   separately (fewer images, faster, more locally relevant). Pass the quadrant box to
   image_subset `--t_projwin` so coverage is scored only inside it.
2. Sun AZIMUTH groups - within a quadrant, split by azimuth into bins of 50-150 images
   ([[sun-illumination]] / [[sfs-azimuth]]). This bounds the per-call count AND yields a
   subset with diverse illumination.
3. LOW-RES sub images - feed image_subset low-res mapprojected `sub` images (from
   stereo_gui pyramids), not full-res. Prefer sub8, else sub4, sub2, full; never coarser
   than sub8 (sub16/sub32 lose too much for reliable coverage).
4. 2x COVERAGE - image_subset has no --exclude, so for a second, redundant cover run it
   once (primary), remove those images, run again on the remainder (extra). The primary is
   a minimal cover; the extra is an independent backup cover (redundancy + more
   illumination diversity for SfS).

CHOOSING THE DECOMPOSITION (rationale). The spatial-quadrant split is only for a VERY LARGE
terrain, where even one azimuth group is too big for one image_subset call. For a site small
enough (e.g. BCU2314 ~19x15 km), SKIP spatial quadrants and split the FULL SITE by azimuth
alone - fewer, cleaner groups, no overlap bookkeeping. Run on the NORMAL queue, not devel
(image_subset is heavy at full-site scale).

DIVIDE THE AZIMUTHS CAREFULLY, NOT BLINDLY. Do NOT just cut into fixed-width bins (e.g. 4x90
deg) - the Sun-azimuth distribution is usually CLUSTERED, especially at the poles where low
sun bunches near due-north (az ~0/360). First INSPECT the distribution (count per candidate
bin, min/median/max azimuth). Then form SEVERAL groups that each span a good, balanced range
AND count of azimuths, aiming for ~100-200 images per group: MERGE sparse/near-empty bins
into a neighbor, and SPLIT dense bins in two at their median azimuth. Worked example
(BCU2314, 926 maps): a blind 4x90-deg split gave 420 / 1 / 41 / 464 - two groups far over
image_subset's limit and one with a single image. Regrouped into FIVE balanced groups: the
lone 90-180 image (az 90.8, adjacent to the 0-90 range) clumped with the low-az range; the
big low-az range [0,180) split at its median; the sparse [180,270) left whole; the big
[270,360] range split at its median -> ~210 / 211 / 41 / 232 / 232. Compute the median split
points from the data (do not hardcode). Each group then yields a primary + extra subcover.

## Reusable tools (~/projects/sfs)

- `prepare_lowres.sh <mapList> <lowresListOut> <currDir>` - ensure each map's stereo_gui
  pyramid exists, emit the best sub level per map (sub8>sub4>sub2>full) as a list. No
  symlinks; image_subset --image-list reads the chosen paths.
- `split_quadrants.py --map-list L --out-dir D (--dem dem.tif | --extent "xmin ymin xmax
  ymax") [--overlap-m 1000] [--lowres-list LR]` - assign each map to the quadrant(s) its
  footprint intersects; writes quad_{ll,lr,ul,ur}.txt (low-res paths if --lowres-list) and
  quad_projwins.txt ("name xmin ymin xmax ymax" for --t_projwin). Needs gdal (the `geo`
  conda env on pfe, see [[pfe-nas]]).
- `image_subset_2x.sh <lowresList> <outPrefix> <currDir> [threshold] [projwin]` - two-pass
  image_subset (primary + extra-on-remainder); optional `--t_projwin` via the 5th arg
  (unquoted "xmin ymin xmax ymax"). Writes _primary.txt, _extra.txt, and the _2x.txt union.
- `batch_image_overlap.sh` - the older thin single-pass image_subset wrapper.
- `coverage_subset_tile.sh` / `run_coverage_tile.sh` - the mons_mouton precedent: per-tile,
  azimuth-binned, 2-pass (the azimuth + 2x logic these tools generalize).

Full script paths: `~/projects/sfs/prepare_lowres.sh`, `~/projects/sfs/image_subset_2x.sh`,
`~/projects/sfs/split_quadrants.py`. Project driver precedent (the settled full-site
azimuth-only recipe): the BCU2314 `sfs_select_bcu2314.sh` in that project's dir.

## Usage - full-site azimuth-group recipe (the settled default)

Runs on a NORMAL-queue qsub. Produces, per azimuth group, a primary and an extra subcover,
then a max-lit mosaic for EACH group plus a combined max-lit of all primaries and all extras
(for eyeballing coverage per subcover). All lists are mapproj-image paths; convert to
map/cub/cam for SfS later.

```bash
cd <workDir>
SEL=selection; mkdir -p $SEL
# 1. pruned full-res maps (drop the stats-pruned suspects; see [[sfs-post-bundle-eval]])
ls map_htdem/*.map.tr1.tif | grep -vF -f lists/removed_ids.txt > $SEL/pruned_map.txt
# 2. low-res list (sub8>sub4>sub2 per map)
~/projects/sfs/prepare_lowres.sh $SEL/pruned_map.txt $SEL/pruned_lowres.txt <workDir>
# 3. split the low-res list into 4 disjoint 90-deg azimuth groups (id -> az from lists/azimuth.txt col3)
#    (awk bin = int(az/90), clamp 0..3; write $SEL/azi_q0..q3.txt)
# 4. per group: primary + extra subcover, FULL SITE (no --t_projwin)
for k in 0 1 2 3; do
  ~/projects/sfs/image_subset_2x.sh $SEL/azi_q${k}.txt $SEL/sub_q${k} <workDir> 0.01
done
# 5. max-lit per group (map low-res paths back to full-res maps), + combined primaries/extras
for k in 0 1 2 3; do for kind in primary extra; do
  awk '{print $1}' $SEL/sub_q${k}_${kind}.txt | sed 's#_sub[0-9]*\.tif$#.tif#' > $SEL/ml_q${k}_${kind}.txt
  dem_mosaic --max --threads 20 --dem-list $SEL/ml_q${k}_${kind}.txt -o $SEL/maxlit_q${k}_${kind}.tif
done; done
printf '%s\n' $SEL/maxlit_q{0,1,2,3}_primary.tif > $SEL/ml_primary_all.txt
dem_mosaic --max --threads 20 --dem-list $SEL/ml_primary_all.txt -o $SEL/maxlit_primary_all.tif
printf '%s\n' $SEL/maxlit_q{0,1,2,3}_extra.tif   > $SEL/ml_extra_all.txt
dem_mosaic --max --threads 20 --dem-list $SEL/ml_extra_all.txt   -o $SEL/maxlit_extra_all.tif
```
The driver `sfs_select_bcu2314.sh` wraps exactly this (with env + counts). Grand candidate
count = unique images across all sub_q*_{primary,extra}.txt.

For the VERY-LARGE-terrain variant, insert `split_quadrants.py` before the azimuth split and
pass each quadrant's projwin to image_subset_2x as the 5th arg (`--t_projwin`).

## Pre-filter by native GSD (and sun elevation) - consider before selecting

image_subset ranks by COVERAGE, not sharpness, so it will happily keep a soft, coarse-GSD
image over a sharp one if it covers a few more pixels. Coarse-GSD / grazing images show up
as soft or rippled patches in the max-lit mosaics ("aliasing" in the BCU2314 half2). So it is
worth computing each image's native GSD and deciding whether to DROP the coarsest ones from
the candidate pool BEFORE (or alongside) the coverage subset - a data-quality lever the
coverage step itself does not provide.
- Tool: `~/projects/sfs/query_gsd.sh <imageList> <cameraList> <dem> <outPrefix> <threshold>
  <currDir>`. It runs `mapproject --query-projection <dem> <img> <cam> <dummy.tif>` per
  image (fast, writes nothing) and parses the emitted `pixel_size,<gsd>` line - the exact
  --tr mapproject would auto-pick. Writes `<outPrefix>_all.txt` (id gsd) and
  `<outPrefix>_lt<threshold>.txt`. imageList/cameraList must be paired 1-to-1; use the LATEST
  (final bundle) cameras and the site DEM (e.g. the honest padded `_extra_noblur` LOLA DEM).
- It is quick - a `devel` qsub. Inspect the GSD distribution in `_all.txt`, then choose a
  cutoff (there is usually a clear coarse tail) and drop those ids from the pruned map list
  before prepare_lowres. Related lever: a gentle sun-elevation floor drops the softest
  grazing frames - but keep useful low-sun images (low sun = strong shading for SfS).

## Gotchas and tuning

- THRESHOLD: 0.01 is the ASP default but is often TOO PERMISSIVE - nearly every image adds
  some unique pixel, so you get little reduction (mons_mouton: only ~21% off). To actually
  thin to ~100/group, raise to ~0.05-0.1, or cap at top-N per group. Tune by clicking pixel
  values in stereo_gui (the reflectance scale sets what "covered" means). It is fine to drop
  the last few images in each ranked list (marginal contribution).
- image_subset needs ALL inputs in ONE projection (mapprojected on the same DEM/grid) -
  true here since they share the reference DEM.
- Output lines are "image_path count"; take column 1 for the image list.
- These lists are LOW-RES mapproj paths. For SfS, map each back to its id -> full map / cub
  / cam (the latest pruned bundle cameras). That conversion is a later, separate step.
- VALIDATE: `dem_mosaic --max` the selected subset and compare to the full-set max-lit
  mosaic ([[sfs-post-bundle-eval]]); coverage should be nearly the same. Overlay the ranked
  list in stereo_gui to confirm it fills the area.

## Related
[[sfs-post-bundle-eval]] (prune whacky cameras FIRST, and the max-lit coverage check),
[[sfs]] (parent pipeline; SfS solve after selection), [[sfs-azimuth]] / [[sun-illumination]]
(azimuth grouping), [[pfe-nas]] (gdal `geo` env, qsub), [[bundle-adjust]] (the cameras).
