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

## 0. Two standing principles (learned the hard way, 2026-10-09 BCT0717)

**(A) Build per-chunk PARTIAL mosaics FIRST, then combine. Divide and conquer.** NEVER build
one monolithic max-lit over all N images. `batch_mapproject.sh` already produces a per-chunk
`max_mosaic_<beg>_<end>.tif` for free when you leave `NO_MOSAIC` UNSET, so keep them. If you
mapprojected with `NO_MOSAIC=1` (no partials), build the partials yourself as one small qsub
PER chunk from `map_jitter/map_list_<beg>_<end>.txt` (index ranges over the azimuth-sorted
list, same chunking as the mapproject). Then the final mosaic is a cheap "batch of the
batches" (`dem_mosaic --max` over the partials). Why this is mandatory, not optional:
  - INSPECTION is per-chunk: a smear contaminates exactly ONE chunk (each image sits in one
    azimuth chunk), so you eyeball 9 small partials and the bad chunk jumps out, instead of
    hunting a needle in one 17k x 10k whole mosaic.
  - RE-PRUNE is a ONE-CHUNK rebuild: drop the offender from that chunk's list, re-max just
    that partial, recombine (seconds), vs re-maxing all N full-extent maps (~37 min each
    iteration). You WILL iterate, so pay the partition cost once.
  - Many small jobs parallelize and survive a busy queue / overnight far better than one big
    job. Start them as separate per-chunk qsubs.
  - BURNED 2026-10-09: ran the jitter mapproject with `NO_MOSAIC=1`, so a whole 426-image
    rebuild took ~37 min AND lost per-chunk inspectability; had to rebuild the partials after
    the fact. Default to partials from the start.

**(B) You ALWAYS have to LOOK. The stats miss real artifacts.** The per-camera
mapproj-offset / reproj stats (section 2) catch pose-smears and dropouts, but they are NOT
sufficient. A chunk can have EVERY image at a healthy offset (p95 ~2-3 m, reproj ~0.6 px,
even better than the prior bundle) and STILL show obvious diagonal brush-stroke smears in its
partial max-lit (seen 2026-10-09: chunk 250_300 and 350_400 flagged clean by the stats, visibly
smeared to the eye). So the stats are a FIRST pass to build a suspect list, never the last
word: crop EVERY chunk partial over the terrain and eyeball it. The eye is the real detector;
the stats only triage. Report what you see, do not declare a set clean on stats alone.

CROP GOTCHA (for the eyeballing): a one-step `gdal_translate -projwin ... -of PNG -ot Byte
-scale ... -a_nodata 0` silently produces NOTHING (PNG driver + projwin + a_nodata combo).
Crop to a GTiff first, THEN convert to PNG: `gdal_translate -projwin <ulx uly lrx lry> src.tif
/tmp/crop.tif` then `gdal_translate -of PNG -ot Byte -scale 0 0.15 0 255 /tmp/crop.tif out.png`.

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
~/projects/SfsPipeline/bin/batch_mapproject.sh ref/lola_1mpp_extra_noblur.tif \
  lists/filtered_images.txt ba_htdem/run map_htdem $(pwd) ba_htdem/run-camera_list.txt
# 2. split partials into two illumination halves by beg (imagecount/2), max-lit each.
cd map_htdem; ls max_mosaic_*.tif | awk -F'[_.]' '$3<500'  > half1_list.txt
              ls max_mosaic_*.tif | awk -F'[_.]' '$3>=500' > half2_list.txt; cd -
~/projects/SfsPipeline/bin/batch_max_mosaic.sh map_htdem/half1_list.txt map_htdem/half1_max_mosaic.tif $(pwd)
~/projects/SfsPipeline/bin/batch_max_mosaic.sh map_htdem/half2_list.txt map_htdem/half2_max_mosaic.tif $(pwd)
# 3. grand max-lit of the two halves.
printf 'map_htdem/half1_max_mosaic.tif\nmap_htdem/half2_max_mosaic.tif\n' > map_htdem/halves_list.txt
~/projects/SfsPipeline/bin/batch_max_mosaic.sh map_htdem/halves_list.txt map_htdem/all_max_mosaic.tif $(pwd)
```
`batch_max_mosaic.sh` runs `dem_mosaic --threads 20`: qsub it (devel), NEVER > 2 threads on
a pfe login node. Inspect ([[visual-inspection]]): red/green overlay half1 vs half2 for
ghosting, and eyeball `all_max_mosaic.tif` for self-consistency and a global shift vs a
LOLA hillshade. NOTE: pass `-o` WITH the `.tif` extension so dem_mosaic writes that exact
name (a bare prefix would instead give the old `<prefix>-tile-0-max.tif`).

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
~/projects/SfsPipeline/bin/sfs_flag_bad_cameras.py ba_htdem/run -o lists/removed_ids.txt --report lists/flagged_report.txt
```

## 2a. After a per-line JITTER solve: screen for blow-up cameras with cam_test

A per-line jitter solve (esp. with a loose orientation guard, e.g. a modest
`--orientation-uncertainty`, or finer orientation knots) can leave the BULK sharp while a
FEW cameras blow up (the guard is too loose to cage those poses, but they still fit their
own tie points, so reprojection and mapproj-offset stay flat and the bundle-stats detectors
above are BLIND to them). Screen for them directly with `cam_test`: for each image compare
the pre-jitter camera (e.g. `ba_htdem`) against the jitter camera and read the MAX pixel
diff. Flag any image with max diff > ~250 px (catastrophic > ~1000 px), cross-reference that
it is IN the SfS set, and drop it. These wild cameras smear only their OWN localized
footprint in the max-lit, so pruning them (a handful of a few hundred) loses no coverage
(redundant frames fill it). CONFIRM the prune: re-build the max-lit without them and diff vs
the full one, the difference should be ~0.04% of pixels confined to those footprints with
the scene-wide sharpness unchanged (burned-in lesson, BCT0717 2026-10-13: a loose-guard
jitter left 2-3 cameras at 1000-1500 px that cam_test caught but every tie-point stat missed;
pruning cleaned the mosaic without touching the sharp bulk).

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
~/projects/SfsPipeline/bin/sfs_prune_and_remosaic.sh map_htdem lists/removed_ids.txt $(pwd) 500 20
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

## 5. Dual-tier (promising vs degraded) stratification and max-lit coverage comparison

After the final bundle adjustment with height constraints (`ba_htdem`), images can be
stratified into two distinct tiers: "promising" (clean, sharp, well-registered) versus
"degraded" (coarse GSD, large mapprojection offset, or low tie-point count). Comparing the
max-lit coverage of the promising set against the full set reveals whether high-quality images
provide adequate coverage, or if excluding degraded frames causes coverage loss (notches/holes).

### Metric Extraction & Stratification
1. Query per-image native GSD using the latest `ba_htdem` cameras onto the honest reference DEM:
   ```bash
   ~/projects/SfsPipeline/bin/query_gsd.sh ref/lola_1mpp_extra_noblur.tif lists/filtered_images.txt \
     ba_htdem/run-camera_list.txt lists/image_gsd.txt
   ```
2. Extract the 75th percentile mapprojection offset and match count from `ba_htdem/run-mapproj_match_offset_stats.txt`.
3. Split images into two disjoint lists:
   - **Promising / Good Set**:
     - GSD <= 2.0 m/px
     - 75th percentile mapprojection offset <= 2.0 m
     - Sufficient tie-point count (e.g. >= 50-100 matches)
   - **Degraded / Bad Set**:
     - All remaining images (coarse GSD > 2.0 m, large offset > 2.0 m, or starved matches)
4. Sort BOTH lists strictly by solar azimuth (preserving illumination order).

### Separate Batch Mapprojection & Max-Lit Hierarchies
Mapproject each tier separately into independent directories:
- `map_htdem_good/`: Batch mapprojection of promising images.
- `map_htdem_bad/`: Batch mapprojection of degraded images.

Each batch chunk automatically produces a per-chunk max-lit mosaic (`max_mosaic_<beg>_<end>.tif`).

For the promising (`good`) images, subdivide the per-chunk max-lits into two illumination halves:
1. `good_half1_max_mosaic.tif`: Max-lit of the first half of good chunks (`beg < N/2`).
2. `good_half2_max_mosaic.tif`: Max-lit of the second half of good chunks (`beg >= N/2`).

Evaluating `good_half1` versus `good_half2`:
- Contrasts two distinct solar azimuth regimes with opposing illumination shadows.
- Highlights crisp shading relief and fine crater details that can be washed out when merging everything into a single all-sun grand mosaic.
- Provides a direct co-registration gate (red/green overlay) to confirm alignment between opposite illumination angles before final fusion.

Then produce the grand comparison mosaics:
1. `good_max_mosaic.tif`: Max-lit combination of `good_half1_max_mosaic.tif` and `good_half2_max_mosaic.tif` (all promising images).
2. `bad_max_mosaic.tif`: Max-lit combination of all chunks in `map_htdem_bad/`.
3. `all_max_mosaic.tif`: Max-lit combination of `good_max_mosaic.tif` and `bad_max_mosaic.tif`.

### Coverage Comparison & Decision Gate
Compare `good_max_mosaic.tif` against `all_max_mosaic.tif`:
- When the promising tier contains >= 80% of candidate images across balanced azimuths, it typically achieves 100.00% site coverage (missing pixels = 0).
- Run a quick raster mask comparison:
  ```python
  good_mask = (~np.isnan(good_arr)) & (good_arr != nodata) & (good_arr > 0)
  all_mask  = (~np.isnan(all_arr))  & (all_arr  != nodata) & (all_arr  > 0)
  missing_in_good = np.count_nonzero(all_mask & (~good_mask))
  ```
- If `missing_in_good == 0`, the degraded set can be completely and safely discarded for SfS and final mosaics.
- If holes or gaps appear in `good_max_mosaic.tif` that are filled in `all_max_mosaic.tif`, identify the specific missing geometries and selectively retain only the minimally necessary frames from the degraded set.

## Count-guarded, error-ranked, BATCHED max-lit (Oleg, 2026-10-25)

After the final mapproject with the refined cameras, rank the images by their
per-image mapproj offset to build a "best-registered subset" max-lit.

**Sort by RELIABILITY here, NOT by Sun azimuth.** The azimuth sort is only for
bundle-adjustment matching (it puts matched-shadow pairs within the overlap window).
For the max-lit the ordering that matters is registration reliability (75th-pct offset
and match count) - a completely different ordering for a different purpose. Do not carry
the azimuth order into the max-lit stage.

Two hard-won rules make this trustworthy and cheap to iterate:

**1. THROW OUT ON BOTH METRICS, FROM THEIR DISTRIBUTIONS - not one fixed cutoff.**
`run-mapproj_match_offset_stats.txt` columns are `image 25% 50% 75% 85% 95% count`. Two
independent quality signals live here and you must screen on BOTH:
  - **match count (field 7)** - how well an image is tied into the network. The 75th-pct
    offset is a LIE for low-count images: an image with ZERO matches reads
    `-1 -1 -1 -1 -1 0`, and a naive `sort -g` on the 75th-pct (field 4) ranks `-1` as the
    BEST image, so a totally unregistered frame lands at the top of the "best" half and
    drapes a streak into the max-lit; a handful of matches (e.g. 13) gives an artificially
    LOW 75th-pct the same way.
  - **75th-pct mapproj offset (field 4)** - the actual registration error, trustworthy
    ONLY once the count is high enough.
  - **native GSD (a THIRD, independent screen) - STANDARD RULE: for the higher-quality
    mosaic, ALWAYS filter out any frame with native GSD > 1.75 m.** A frame whose native
    ground sample distance is coarser than the product resolution (1 m/px here) contributes
    only upsampled BLUR, so it is dropped. `batch_maxlit.sh` enforces this by default
    (`MAXGSD=1.75`). Get GSD per frame from the ODE query table (`lists/siteN_table.txt`
    field 6 `resolution_m`, verified ~= the authoritative `mapproject --query-projection` /
    `query_gsd.sh` value). >1.5 m is an optional sharper (more aggressive) cut when
    resolution matters more than coverage. In practice the coarse frames are often already
    gone - they tend to be grazing/all-shadow and get dropped by `filter_by_max` or the
    count/offset screens upstream (on these south-polar sites the good-max-lit set already
    topped out at ~1.73 m, so the 1.75 m filter removed nothing and >1.5 trimmed ~20%).
    `batch_maxlit.sh` applies all three screens (MINCOUNT, MAX75, MAXGSD).
  FIRST CHARACTERIZE EACH DISTRIBUTION, THEN SET HEURISTIC THRESHOLDS FROM IT - do not
  hardcode a single number blindly. Print the spread of the count column and of the
  75th-pct column (min, a few percentiles, median, max) and look for the natural break
  between the well-connected bulk and the stragglers. Then DROP an image if it fails
  EITHER test: count below a low floor / the bottom of the bulk (unreliable), OR 75th-pct
  above a high-error cutoff / the top tail (genuine misregistration). The two catch
  different failure modes - low count = untrustworthy metric (streak risk), high 75th-pct
  = real smear - so apply both. Real case: of site1's 295 images the count column split
  cleanly as 3 at 0, 4 at 1-99, 288 at >=500, so a count floor of ~100 was the obvious
  break; `p75lo`'s streaks were exactly the two low-count frames that leaked into the best
  half (`M1219619311RE` count 0, `M1417661629RE` count 13). Pick the floor from where the
  gap actually is, which differs per site.

**2. BUILD IN BATCHES OF ~50, THEN A MAX-LIT-OF-MAX-LITS - so a defect is cheap to
remove.** Sort the count-guarded survivors by 75th-pct offset, split into batches of 50,
`dem_mosaic --max` each batch to a per-batch max-lit, then `dem_mosaic --max` over the
batch max-lits for the full ("all") and the first-K-batches ("lo", best up to a cutoff)
result. When a batch max-lit shows a streak: find the one bad image, drop it from THAT
batch's list, `rm` only that batch max-lit, and re-run - only that batch + the
max-lit-of-max-lits rebuild (the other ~5 batch max-lits are untouched), which is far
cheaper than re-mosaicking all ~300 frames. The batch max-lits also localize a streak to
a 50-image group immediately.

Scripts (per-site, take N; currently in `~/projects/sfs_south/`, generalizable to
SfsPipeline/bin): `batch_maxlit.sh <workDir> <N> [BATCHSIZE=50] [NUMLO=3] [MINCOUNT=100]`
does all of the above (qsub it, dem_mosaic is multi-thread; writes `maxlit_siteN/
max_siteN_batch_<k>.tif`, `max_siteN_molm_all.tif`, `max_siteN_molm_lo.tif`, and the
dropped ids to `lists/siteN_excluded_lowcount.txt`). `find_streak.sh <mapprojList> <x> <y>
...` samples each image's value at a streak point (the max-lit took the max, so the
culprit has the anomalously high value there) to identify which frame drapes a given
streak. Always VERIFY `VALID_PERCENT>0` on every mosaic ([[pfe-nas]]).

**3. NOT EVERY STREAK IS A REGISTRATION ERROR - radiometric/detector defects evade ALL
the stats.** A max-lit streak that is a set of FINE PARALLEL DIAGONAL LINES (after
mapprojection) is a single frame's DETECTOR/readout stripe (or a saturation band), i.e. a
RADIOMETRIC defect. That frame is perfectly well REGISTERED - good match count, good 75th-pct
offset - so BOTH screens above pass it and it lands in the "best" max-lit. The count/offset
screens catch GEOMETRIC failures only; radiometric defects need the EYE. Finding the frame
(real case: site2 molm_lo, batch 3):
  - The per-batch max-lits localize it to one batch (inspect each / sample where the streak is).
  - WITHIN the batch, the usual tricks FAIL: point-value sampling (`find_streak.sh`) misses it
    because a faint stripe produces no anomalous MAX at any pixel; an FFT "stripiness" score
    (`stripe_score.py`) is confounded because the DIAGONAL FOOTPRINT EDGES of partial-coverage
    frames give the same off-axis peak as diagonal stripes (masking the FFT central cross kills
    axis-aligned edges but NOT diagonal ones) - so FFT scoring needs the crop to be footprint-
    INTERIOR only, or it just keeps flagging edges.
  - The method that cannot be fooled is DIVIDE-AND-CONQUER on the MAX-LIT itself
    (`bisect_maxlit.sh`): split the batch into G groups, `dem_mosaic --max` each, eyeball which
    GROUP max-lit still carries the stripe, recurse on that group's list down to the one frame.
    Then drop it from its batch list, rebuild that one batch max-lit, re-combine - cheap.
  Contact-sheet helper for the eyeball step: `inspect_region_montage.py` (one subplot per frame,
  cropped to the defect region). All three tools are in `~/projects/sfs_south/`.
  **A detector-stripe frame must ALSO be dropped from the SfS SOLVE, not just the max-lit.**
  SfS models image intensity as a function of topography and illumination, so a frame whose
  radiometry is corrupted by detector stripes (or saturation) injects false intensities and
  biases the terrain. Once you identify such a frame, remove it from the SfS image/camera lists
  (the max-lit removal only cleans the inspection product). It can stay in the bundle (geometry
  is fine), but keep it out of photoclinometry.
  **Make the exclusion STICK with one authoritative list + a pre-launch check (the exact trap
  that bit site2, 2026-10-27).** The lesson above was already written here, yet a known detector-
  stripe frame (M128439353RE) got dropped from the max-lit but NOT from the SfS solve: the removal
  was deferred ("exclude from SfS later"), forgotten, and the frame rode into the SfS re-run and
  re-striped the DEM. Prevent it mechanically:
  - Keep ONE authoritative per-site exclusion list, `lists/siteN_excluded.txt`, holding EVERY
    dropped id with its reason (geometric: low count / high 75th-pct offset / coarse GSD, AND
    radiometric: detector stripe / saturation). One list, both defect classes.
  - BEFORE every SfS launch OR re-launch, ASSERT the SfS image list shares nothing with it:
    `grep -Ff lists/siteN_excluded.txt lists/siteN_sfs_images.txt` must print NOTHING. Make this
    a gate, not a memory.
  - A stripe found AFTER the SfS image list was built is a RE-RUN trigger, not a note-for-later:
    drop the id, rebuild the SfS list, and REDO the affected SfS tiles. Because `parallel_sfs
    --resume` keeps already-written per-tile DEMs, you MUST remove the contaminated tile outputs
    first (or run into a fresh output dir), or it silently keeps the striped result.

## Related
[[sfs]] (the parent pipeline; this is its pre-SfS gate), [[bundle-adjust]] (the stats files
and the solve), [[jitter-solve]], [[visual-inspection]] (overlay/hillshade/colorbar),
[[pfe-nas]] (gdal env, /tmp node-local, qsub), [[dem-comparison]].

