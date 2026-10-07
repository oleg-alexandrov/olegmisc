---
name: sfs-image-selection
description: >-
  Select a minimal-but-covering SUBSET of images for Shape-from-Shading after the cameras
  are refined and pruned. Covers the ASP image_subset recipe: break the terrain into
  overlapping quadrants, group by Sun azimuth ANGLE (not by count, thin dense angles and keep
  rare ones whole), build low-resolution mapprojected sub images, run image_subset per group,
  and a second pass for 2x coverage.
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

## Workflow at a glance (the rationale, in order)

1. PRUNE the pool first (see [[sfs-post-bundle-eval]]): drop frames with a big bundle
   mapproj-dem offset (we used 75th-percentile offset over ~2 m), too few matches, and
   coarse GSD. This is the good/bad stratification that feeds selection.
2. GSD caution: consider also dropping frames over ~1.75 m/px, or keep them only as a
   measure of last resort (sole cover). Prefer finer frames (see the GSD section below).
3. PLOT the azimuth distribution as a polar rose (`plot_sfs_azimuth.py`, reads the
   sfs_query azimuth table) to judge density and representativeness before grouping.
4. GROUP by azimuth ANGLE, not count: cut into fixed ~45-deg slices that respect the
   natural gaps, keep a rare direction whole, drop empty slices (the "divide by angle"
   rules below).
5. image_subset per slice (primary + extra = 2x cover) over low-res sub images, then a
   max-lit per group and a grand max-lit of the subsampled covers to validate coverage.

## The recipe (ASP image_subset, :numref:`image_subset`)

image_subset picks, greedily, the image contributing the most pixels at/above a threshold,
then the next-most-additional, etc., writing a ranked list "image_path count". It is SLOW
(O(n^2 * output pixels)) and reads each image fully into memory, so it wants at most
~100-200 small images per call. Therefore decompose first:
1. QUADRANTS with overlap - split the box into 4 overlapping quadrants; process each
   separately (fewer images, faster, more locally relevant). Pass the quadrant box to
   image_subset `--t_projwin` so coverage is scored only inside it.
2. Sun AZIMUTH groups - within a quadrant, split by azimuth ANGLE into slices
   ([[sun-illumination]] / [[sfs-azimuth]]), per the "DIVIDE THE AZIMUTHS BY ANGLE" rules
   below. This yields a subset with diverse illumination and bounds the per-call count.
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

DIVIDE THE AZIMUTHS BY ANGLE, NOT BY COUNT. The objective is a subset that is REPRESENTATIVE
in illumination DIRECTION. So the grouping axis is the Sun AZIMUTH ANGLE itself, not equal
image counts. Bin by angle, then let image_subset THIN each bin: a bin with a gazillion
near-identical-azimuth frames must be sparsed out (it only needs to fill the same ground), a
bin with few frames is kept nearly whole. Count-balancing matters ONLY as image_subset's
practical ceiling (~200-250 small images per call), not as a goal.

The method:
  1. INSPECT the distribution first (count per candidate angle bin, min/median/max azimuth).
     At the poles the low Sun is strongly CLUSTERED (often bimodal: a morning lobe and an
     evening lobe) with near-empty gaps between - never assume it is uniform.
  2. ISOLATE a RARE / precious illumination direction as its OWN group and keep it WHOLE (do
     not subset it). If you fold a rare azimuth into a dense neighbor, image_subset's
     coverage ranking will quietly DROP it - losing the very illumination diversity SfS needs.
  3. Cut the DENSE lobes into fixed-angle slices (e.g. 45 deg). Each slice is one group and
     gets thinned by image_subset; the densest slice simply gets sparsed the hardest. Do NOT
     split a slice just to equalize counts.
  4. RESPECT NATURAL BREAKS and do not subdivide dumbly: DROP an empty bin, and LUMP a lone
     stray image into the adjacent slice (an image at az 45.5 joins the [0,45) slice).
  5. SPLIT a slice further ONLY if it exceeds image_subset's size ceiling - and then split at
     a natural break if there is one, else at the slice median.
  Each resulting group yields a primary + extra subcover.

Worked example A (BCU2314, 926 maps): a blind 4x90-deg split gave 420 / 1 / 41 / 464 - two
groups far over image_subset's limit and one with a single image. Regrouped by angle with the
rules above: the lone 90-180 image (az 90.8) lumped into the adjacent low-az slice; the big
low-az range split at its median; the sparse [180,270) left whole; the big [270,360] range
split at its median -> ~210 / 211 / 41 / 232 / 232.
Worked example B (BCT0717-BCT2124, 1037 kept maps, bimodal polar): fixed 45-deg slices gave
[0,45)=209, [45,90)=140, [90,135)=0, [135,180)=12, [180,225)=43, [225,270)=179, [270,315)=214,
[315,360)=240. Kept the rare [135,180)=12 tail as its OWN whole group, DROPPED the empty
[90,135), and handed each remaining slice to image_subset 2x - the dense 209/214/240 slices
thinned hardest, the sparse [180,225)=43 kept nearly whole. Compute bin counts from the data
(do not hardcode).

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
- `plot_sfs_azimuth.py <azimuth_table> [-o rose.png] [--table2 ...]` - polar rose plot of the
  Sun azimuths (reads the sfs_query table, col3 = 0-360 az). Use it to judge illumination
  density and representativeness before grouping, and to decide the angle slices.

Full script paths: `~/projects/sfs/prepare_lowres.sh`, `~/projects/sfs/image_subset_2x.sh`,
`~/projects/sfs/split_quadrants.py`. Project driver precedents in their project dirs:
`sfs_select_bcu2314.sh` (median-split full-site) and `sfs_select_bct.sh` (the newer
fixed-ANGLE 45-deg slices with a rare group kept whole - the current recipe).

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
The canonical driver `~/projects/sfs/sfs_select_full_site.sh` wraps exactly this (with env + counts):
```bash
~/projects/sfs/sfs_select_full_site.sh <map_list.txt> <azimuth_table.txt> <out_dir> <curr_dir> [threshold]
```
Grand candidate count = unique images across all sub_q*_{primary,extra}.txt.

For the VERY-LARGE-terrain variant, insert `split_quadrants.py` before the azimuth split and
pass each quadrant's projwin to image_subset_2x as the 5th arg (`--t_projwin`).

## Pre-filter by native GSD (and sun elevation) - consider before selecting

BE MINDFUL OF GSD. image_subset ranks by COVERAGE, not sharpness, and a coarse frame has a
BIG footprint - ground area per pixel scales as GSD squared, so a 2 m/px frame covers ~4x the
real estate of a 1 m/px one. That wide footprint is exactly what makes image_subset's greedy
ranking PREFER the coarse frame, when for SfS we would rather take the finer image wherever a
sharp one also covers that ground. Coarse / grazing frames then show up as soft or rippled
patches in the max-lit mosaics ("aliasing" in the BCU2314 half2). So compute each image's
native GSD and decide whether to DROP the coarsest ones from the candidate pool BEFORE (or
alongside) the coverage subset - a data-quality lever the coverage step itself does not
provide, and one that actively counters the greedy bias toward big coarse footprints.
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

### The drop is from the SfS SUBSET only, NOT from the bundle solve (role-dependence)

A coarse, large-footprint frame is a LIABILITY here (it smears in max-lit/SfS) but an ASSET
for the bundle/jitter solve: its wide footprint ties together images that have no other
intermediate overlap and bridges illumination/temporal gaps. So dropping it for the SfS
subset does NOT mean dropping it from bundle_adjust - keep it in the solve, exclude it only
from the SfS/max-lit cover. Decide the two sets separately. Detail + the measured metric
behavior: [[sfs-post-bundle-eval]] section 2b.
- Practical cutoff (SOFT, not a hard rule): GSD over ~1.75 m/px is SUSPECT, and over ~2.0
  m/px should almost surely drop from the SfS subset (both well above the healthy ~1.3 m
  bulk). The preference is always: take a FINER frame when one also covers that ground,
  accept a coarse one only where it is the sole cover. Ideally weigh GSD ALONGSIDE a large
  bundle mapproj-dem offset. CAVEAT: GSD and mapproj-offset are correlated (~0.52 at BCU2314,
  ~0.31 at BCT), so mapproj-offset is partly a coarseness
  proxy - a high value on a coarse frame may just mean "coarse", not "misregistered". To
  tell a truly misposed frame from a merely-coarse one, use the resolution-agnostic SfS
  sim-align shift ([[sfs-run-align]]), which is independent of GSD.
- COVERAGE-HOLE GATE (do this before actually removing): a coarse frame may be the SOLE
  coverage for some ground patch or the only bridge across an illumination gap. Removing it
  then opens a HOLE - real estate with no substitute. Before dropping, confirm other frames
  still cover that footprint (re-run the coverage subset WITHOUT the candidate and check no
  region loses its only contributor; or eyeball a valid-count mosaic). If it is the only
  cover there, keep it and accept the local softness rather than lose the terrain.

## Gotchas and tuning

- THRESHOLD: 0.01 is the ASP default but is often TOO PERMISSIVE - nearly every image adds
  some unique pixel, so you get little reduction (mons_mouton: only ~21% off). To actually
  thin to ~100/group, raise to ~0.05-0.1, or cap at top-N per group. Tune by clicking pixel
  values in stereo_gui (the reflectance scale sets what "covered" means). It is fine to drop
  the last few images in each ranked list (marginal contribution).
  RAISING THE THRESHOLD IS A USER DECISION. Start at 0.01. If it barely thins, ADVISE the
  user that raising to ~0.05-0.1 is likely wise and why, but NEVER raise it on your own, and
  worst of all NEVER do it quietly - always notify and get the user's go-ahead first. The
  threshold changes which images SfS sees, so it is not a knob to turn silently.
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
