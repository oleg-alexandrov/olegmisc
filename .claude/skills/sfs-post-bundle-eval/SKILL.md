---
name: sfs-post-bundle-eval
description: >-
  Evaluate a refined linescan camera set AFTER bundle/jitter adjustment and mapprojection
  but BEFORE Shape-from-Shading: the pre-SfS max-lit alignment sanity check (illumination
  halves), detecting whacky cameras from the bundle_adjust per-camera stats (smears vs
  dropouts, and how predictive the stats are), localizing a smear to a chunk and image,
  and pruning + rebuilding the max-lit mosaics cheaply without re-mapprojecting. Load when
  a post-bundle max-lit mosaic shows smears/streaks/ghosting, when deciding which cameras
  to drop before SfS, or when running the go/no-go alignment gate before SfS.
---

# Post-bundle, pre-SfS camera-set evaluation

The stage BETWEEN the [[sfs]] refine chain (fixed -> free -> dem, see [[bundle-adjust]] /
[[jitter-solve]]) and actual SfS. Goal: prove the refined cameras co-register the whole
set and carry no whacky members, and if they do, find and remove them and rebuild the
mosaics. Distilled from the BCU2314-BDU1224 lunar polar LRO NAC work.

Inputs it assumes exist (from the refine chain + a pre-SfS mapproject pass):
- `<ba>/run-*` per-camera stats (see below), where `<ba>` is the final bundle out dir.
- `map_htdem/*.map.tr1.tif` per-image mapprojected rasters (final-stage cameras onto the
  honest DEM), each chunk's input list `map_htdem/map_list_<beg>_<end>.txt`, and the
  per-chunk max-lit partials `map_htdem/max_mosaic_<beg>_<end>.tif`.

## 1. Pre-SfS max-lit alignment sanity check (the go/no-go gate)

Mapproject all survivors with the NEWEST (final-stage) cameras onto the honest DEM (do NOT
set NO_MOSAIC, so each chunk auto-writes its max-lit partial), then max-lit the FIRST-half
and SECOND-half image groups (split by azimuth index) SEPARATELY, then max-lit those two
halves into one grand mosaic. The halves are two DISJOINT illumination groups: if the
cameras register, terrain coincides when overlaid; ghosting/doubling between halves is
residual misregistration. Two halves (not per-frame) is enough to localize while staying
cheap. Template: `sfs_m2m_ca` `sfs_ca_align_notes.sh` Step 6.

```bash
# 1. batch mapproject; pass the final-stage adjusted camera list as the 6th arg.
export CHUNK_SIZE=50          # ~20 chunks per ~1000 images
export WALLTIME=6:00:00
~/projects/sfs/batch_mapproject.sh ref/lola_1mpp_extra_noblur.tif \
  lists/filtered_images.txt ba_htdem/run map_htdem $(pwd) ba_htdem/run-camera_list.txt
# 2. split partials into two illumination halves by beg (imagecount/2), max-lit each.
cd map_htdem; ls max_mosaic_*.tif | awk -F'[_.]' '$3<500'  > half1_list.txt
              ls max_mosaic_*.tif | awk -F'[_.]' '$3>=500' > half2_list.txt; cd -
~/projects/sfs/batch_max_mosaic.sh map_htdem/half1_list.txt map_htdem/half1_max_mosaic.tif $(pwd)
~/projects/sfs/batch_max_mosaic.sh map_htdem/half2_list.txt map_htdem/half2_max_mosaic.tif $(pwd)
# 3. grand max-lit of the two halves.
printf 'map_htdem/half1_max_mosaic.tif\nmap_htdem/half2_max_mosaic.tif\n' > map_htdem/halves_list.txt
~/projects/sfs/batch_max_mosaic.sh map_htdem/halves_list.txt map_htdem/all_max_mosaic.tif $(pwd)
```
`batch_max_mosaic.sh` runs `dem_mosaic --threads 20`: qsub it (devel), NEVER > 2 threads on
a pfe login node. Inspect ([[visual-inspection]]): red/green overlay half1 vs half2 for
ghosting, and eyeball `all_max_mosaic.tif` for self-consistency and a global shift vs a
LOLA hillshade. NOTE: pass `-o` WITH the `.tif` extension or dem_mosaic writes
`<prefix>-tile-0-max.tif`.

Illumination difference vs misregistration: red/green ONLY on opposite crater walls =
shadow-direction difference between the two azimuth groups (fine). A real shift offsets
whole crater outlines uniformly.

## 2. Whacky-camera detection from the bundle stats (and how predictive they are)

A max-lit mosaic can show diagonal **brush-stroke smears** (or a washed-out blurry patch)
while the terrain underneath stays put. That is one or a few badly-posed cameras draping
stretched content in the wrong place; max-lit keeps the bright streak. The bundle
per-camera stats predict these WELL. Four files, joined on the image id:

- `<ba>/run-mapproj_match_offset_stats.txt` (image, 25/50/75/85/95%, count = METERS between
  where this image lands a feature and where the OTHERS land it). **The single best smear
  detector.** A smear lights up with a huge upper percentile (m to km) while its median
  stays small (only part of the footprint is thrown).
- `<ba>/run-final_residuals_stats.txt` (image, mean, MEDIAN, count = reprojection px).
  BLIND to a self-consistent-but-wrong camera: a smear can reproject at ~0.15 px (it fits
  its own tie points perfectly) yet be km off in absolute terms. Only catches the
  internally-poor few.
- `<ba>/run-camera_offsets.txt` (image, horiz, vert center move, m) + the match count:
  catch DROPOUTS - cameras that drifted 1e3-1e6 m with ~0 matches. They fall OFF the box
  and contribute nothing (a black notch, an absence), so they do NOT smear, but they are
  junk and should leave the camera set.
- `<ba>/run-triangulation_offsets.txt` (secondary).

SMEAR vs DROPOUT (the key distinction): mapproj-offset flags BOTH. Only a large offset
**with a big surviving in-box footprint** (hundreds-thousands of matches) actually smears.
A large offset with near-zero matches is a dropout. Both should be dropped before a
re-solve / SfS; only smears force a mosaic rebuild.

PREDICTIVENESS (measured on BCU2314, 978 cameras): every eyeballed culprit was caught by
the stats. Ranking by mapproj-offset-95% (smears) plus camera-offset/low-count (dropouts)
found them all; reprojection-median only added the internally-poor few. So trust the
stats: build a suspect list, verify a couple visually, prune preemptively.

Suspect thresholds that worked (well above the healthy bulk: reproj med ~0.44, mapoff95
~1.7 m, camoff ~130-360 m) - flag a camera if ANY:
- mapproj-offset 95% > ~5 m (smear / partial-throw)
- reproj median > ~0.75 px (poor internal fit)
- camera horiz OR vert move > ~1000 m (drifted dropout)
- match count < ~50 (starved, unreliable pose) or 0/nan (no surviving obs)

Emit the flagged ids to `lists/removed_ids.txt`. On BCU2314 this flagged 52 of 978,
concentrated in the near-north grazing-sun chunks (one chunk lost 14 of ~48 drifted
dropouts).

Canonical detection tool:
```bash
# Automated suspect camera detection from bundle statistics:
~/projects/sfs/sfs_flag_bad_cameras.py ba_htdem/run -o lists/removed_ids.txt --report lists/flagged_report.txt
```

## 2b. GSD vs mapproj-offset vs sim-shift: what each metric REALLY measures (and role matters)

Measured on BCU2314 (978 LRO NAC cameras, 2026-10-01) by joining three per-image signals:
native GSD (`query_gsd.sh` -> `lists/*_gsd*.txt`), the bundle mapproj-dem offset 95th pct
(`run-mapproj_match_offset_stats.txt`), and the SfS sim-align shift
([[sfs-run-align]] / `shift_report.txt`). The correlations are the whole lesson:
- **Spearman(GSD, mapproj-offset95) = +0.52 (strong).** A coarse image's tie points
  localize fuzzily, so meters-off-consensus inflates EVEN WHEN THE POSE IS FINE. So
  mapproj-offset is PARTLY A COARSENESS PROXY, not pure misregistration - which is exactly
  why thresholding raw mapproj-offset is a weak/self-fulfilling failure predictor (it just
  re-finds the coarsest frames).
- **Spearman(GSD, sim-shift) = -0.05 (zero).** The SfS sim-align shift is
  RESOLUTION-AGNOSTIC - it measures true image-to-terrain registration independent of pixel
  scale. It is the CLEAN pose-quality metric. (And it is independent of the bundle stats,
  so it is non-circular external validation of a prune: on BCU2314 sim-shift independently
  re-flagged 37 of 52 hand-removed cameras.)
- The two failure metrics (mapproj-offset and sim-shift) are themselves ~uncorrelated
  (Spearman ~ -0.09): they fail on different axes, so OR-combining them beats either alone.

PREDICTOR RULE (refined): to judge whether a camera is truly MISREGISTERED, use sim-shift
(or GSD-normalized mapproj-offset), NOT raw mapproj-offset. Raw mapproj-offset alone just
rejects coarse frames. Reserve a hard mapproj-offset reject for the extreme tail
(>~5-10 m) where it co-fires with low match count (genuine smears/dropouts, section 2).

### An image's value is ROLE-DEPENDENT - a bundle asset can be an SfS liability

High-GSD (coarse, large-footprint) frames are a TIE-COVERAGE ASSET for bundle_adjust:
their wide footprint stitches together images that have no intermediate overlap otherwise -
they bridge illumination and temporal gaps and supply correspondences where the set is
otherwise disconnected. KEEP them for the bundle/jitter solve. BUT the SAME coarse frames
are a LIABILITY for max-lit and SfS: draped on a fine (e.g. 1 m) DEM they stretch coarse
content and SMEAR (section 2/3), and they add no real detail. So the "keep for bundle" set
is NOT the "keep for the SfS/max-lit subset" set - decide the two separately. On BCU2314
the 5 worst primary-tier offenders were all top ~2% GSD (2.5-4.35 m/px vs median 1.28,
p90 1.54), flagged high by mapproj-offset mostly BECAUSE they were coarse, with only mildly
elevated sim-shift ("fine but coarse", not grossly misposed).

## 3. Localize a smear to a chunk and image

Each image sits in exactly ONE chunk (by azimuth index), so a culprit contaminates exactly
one chunk, one half, and the full mosaic.
- Crop each candidate half's partials to the projwin where the smear shows and montage
  them; the offending chunk stands out (a smear chunk shows streaks; a defocused one shows
  a washed-out blur). `gdal_translate -projwin <ulx uly lrx lry> -of PNG -ot Byte -scale 0
  0.11 0 255 -a_nodata 0 max_mosaic_<chunk>.tif crop.png` (reflectance ~0-0.2; pick the
  -scale ceiling from `gdalinfo -stats`).
- Then within that chunk, the flagged images from step 2 (idx inside the chunk's range)
  are the culprits; confirm against the stats.

## 4. Prune and rebuild the mosaics WITHOUT re-mapprojecting

The per-image `map_htdem/*.map.tr1.tif` survive and each chunk kept its `map_list_*.txt`,
so rebuilding is just re-running `dem_mosaic --max` with the suspects removed - no
re-mapproject. Two scopes:
- ONE culprit, few chunks: rebuild only the affected chunk -> its half -> the total
  (BCU2314 `bcu_redo_clean_mosaics.sh`, `_clean` suffix).
- MANY suspects, preemptive prune: drop the whole `lists/removed_ids.txt` from EVERY
  chunk's map list and rebuild all chunks -> both halves -> total using the canonical runner:
```bash
# Rebuild all chunks, halves, and grand mosaic with removed_ids pruned:
~/projects/sfs/sfs_prune_and_remosaic.sh map_htdem lists/removed_ids.txt $(pwd) 500 20
```
Or manually in a loop:
```bash
for ml in $(ls map_htdem/map_list_*.txt | grep -E 'map_list_[0-9]+_[0-9]+\.txt$' | sort -t_ -k3 -n); do
  be=...; en=...                                 # parse from the name
  grep -vF -f lists/removed_ids.txt "$ml" > map_htdem/map_list_${be}_${en}_pruned.txt
  dem_mosaic --max --threads 20 --dem-list map_htdem/map_list_${be}_${en}_pruned.txt \
    -o map_htdem/max_mosaic_${be}_${en}_pruned.tif
done
# then half1_pruned (beg<500), half2_pruned (beg>=500), all_pruned = max-lit of the halves
```
GOTCHAS:
- Restrict the chunk-list glob to `map_list_[0-9]+_[0-9]+\.txt$` or it also grabs your own
  `_clean`/`_pruned` intermediate lists and builds a bogus chunk.
- Name products by state (`_clean`, `_pruned`); a new prune SUPERSEDES an earlier partial
  clean - say so, don't leave two "clean" generations unlabeled.
- Same-grid max-lit (no reprojection) is quick; one `devel` qsub. `dem_mosaic --threads 20`
  needs a compute node (never > 2 on a login node).
- gdal on pfe: use the `geo` conda env (PROJ wired, no proj.db warning); `/tmp` is
  node-local and the `pfe` alias load-balances, so write previews/outputs to the SHARED
  nobackup dir, not `/tmp`. Detail: [[pfe-nas]].

VERIFY (mandatory): re-crop the projwin that was bad (must be clean now) and eyeball the
rebuilt halves + total. The streaks/blur must be gone AND the craters must NOT have moved -
if terrain shifted, a GOOD camera was dropped by mistake. Log every removed id in the
project notes.

## Related
[[sfs]] (the parent pipeline; this is its pre-SfS gate), [[bundle-adjust]] (the stats files
and the solve), [[jitter-solve]], [[visual-inspection]] (overlay/hillshade/colorbar),
[[pfe-nas]] (gdal env, /tmp node-local, qsub), [[dem-comparison]].
