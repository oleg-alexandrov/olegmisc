---
name: wv-ccd-correction
description: WorldView (DigitalGlobe/Maxar) CCD artifact correction with wv_correct - measuring a per-column dx/dy correction by correlating a band against its own PAN (green->pan for the WV3 green MS work), building the correction (dg_mosaic concatenate, crop the NTF zero-padding, 4x average-downscale PAN, robust-mean disp_avg, moving-average detrend + edge clamp), applying it, verifying it (single-image re-correlate + the stereo-DEM before/after test), and SHIPPING it into wv_correct's built-in multispectral table (ms_correction_lookup.txt + correction TIF). Load before any WorldView CCD / green-band correction, wv_correct, or green<->PAN disparity work.
---

## What the artifact is

WorldView CCD artifacts are piecewise-constant per-COLUMN steps ("fences") at the
seams between detector subarrays. `wv_correct` removes them by shifting each column
by a small (subpixel) per-column dx/dy read from a correction table. The correction
is INTRINSIC to the band/sensor: only LOCAL per-CCD jumps, NET total shift ~0. The
WV3 green MS band needs its own correction (the standard wv_correct tables cover
PAN/known bands, not the green MS subarray layout).

## Measuring the correction: correlate the band against its OWN PAN

The reference is the PAN image of the SAME acquisition (NOT a stereo pair, NOT
another date). PAN is the sharp helper; the correction is the band's intrinsic
per-column error relative to it. Steps (this is recipe "v3", validated on WV3 green
Uluru 2026-09; the runnable tool is `~/projects/sdb_2026_08/uluru_cell.sh` +
`uluru_cell_analyze.py`, private notes `~/projects/sdb_2026_08/wvgreen_uluru_notes.sh`):

1. **dg_mosaic to CONCATENATE along-track parts** (P001+P002...), for BOTH PAN and the
   band. dg_mosaic concatenates, it does NOT resample. For the green band pass
   `--band 3` (the 8-band MS green); output is `<o>.r100.b3.tif`. Do NOT try to pair
   PAN part-1 with band part-1 - the along-track split is arbitrary and differs
   between PAN and MS; mosaic each fully, THEN work on the whole frame.
   - dg_mosaic reads the delivery XML. The new namespaced ISD XML (`isdc:` tags, new
     Maxar/Vantor format) needs the namespace stripped or dg_mosaic dies at
     `IMD.find("IMAGE")` (NoneType). FIXED in ASP (strip_ns, commit b091e48ab) - if a
     pfe build predates it, redeploy dg_mosaic to `.../StereoPipeline/libexec/`.
2. **CROP the NTF zero-padding FIRST.** Raw NTF frames carry zero-padding columns
   (hundreds). They skew `--individually-normalize` and produce a spurious smooth
   "wave" in the disparity that looks like parallax but is a normalization artifact.
   Compute the nonzero bounding box and `gdal_translate -srcwin` to the valid box for
   BOTH the band and the PAN. (This was THE bug behind a misdiagnosed "wave".)
3. **4x average-downscale the PAN** to the MS resolution: `gdal_translate -r average
   -outsize 25% 25%`. PAN is 4x finer than MS. Use `-r average` (not nearest).
4. **Left-crop the PAN by the focal offset** (~13 downscaled cols for WV3 green) so
   the raw median dx lands near 0. Measure it first with a WIDE `--corr-search`
   (e.g. -20 -20 20 20, no left crop) - a consistent median dx (~-13.5 for green
   Uluru) IS the offset; then crop that many cols and switch to a CENTERED search
   (`--corr-search -5 -5 5 5`). Cropping first is REQUIRED for a centered search.
5. **Correlate band(LEFT) -> PAN(RIGHT).** Direction matters: with the band as LEFT,
   the disparity is BAND-INDEXED, so the correction table applies DIRECTLY to the band
   with no frame remap. `stereo --stop-point 4 --individually-normalize
   --alignment-method none --corr-search ... --corr-kernel 9 9 --subpixel-kernel 9 9`
   with trivial identity .tsai cams. Then `disp_avg --remove-outliers-params 100 3
   --save-no-metadata run-RD.tif avg-dx.txt avg-dy.txt`.

## Averaging + detrend (deterministic, shipped)

- **disp_avg collapses the 2D disparity to a per-column ROBUST (outlier-rejected) MEAN**
  - that IS the "robust mean", NOT a median. `--remove-outliers-params 100 3` = keep
  100% but reject >3 sigma. Use MEAN here, not median: mean is the better estimator for
  this smooth per-column signal. A whole column should never go invalid; if it does,
  something upstream is wrong (bad crop/normalize) - investigate, don't paper over.
- **Across scenes (~8-10 acquisitions): nan-safe MEAN per column**, not median.
- **Detrend with a deg-6 POLYNOMIAL, deterministically, IN THE TOOL.** The poly detrend
  CENTERS the curve at 0 (removes both the residual global shift AND the smooth
  big-scale up/down = leftover parallax/perspective/rock topography) while KEEPING the
  ~880-col CCD plateaus/steps. Ship the DETRENDED grand-average as the correction. This
  is the whole point of detrend: it fixes global shift and the large-scale pattern at
  once, leaving only the local CCD jumps centered on 0.
- **Fit the detrend baseline on the INTERIOR only, and CLAMP the garbage edges before shipping**
  (learned the hard way 2026-09-09). The outer ~700 cols at each end are off-border correlation =
  garbage. If you (a) fit the deg-6 over the full length, the edge-pulled poly inflates the interior
  residual (dx detrended std rose 0.067 vs 0.032 when fit on interior [700:-700] only); and worse,
  (b) if you leave those garbage cols IN the saved table, they hold values like +13 px, and
  wv_correct applies them VERBATIM - shifting the first/last 700 image columns by ~13 px and
  corrupting the edges. So: fit the poly on interior finite cols only, and after detrend hold each
  end flat at the median of an interior window (clamp_edges) so the shipped table is benign
  (near-0, continuous) at the edges. ALWAYS `head`/`tail` the shipped table and confirm the edges
  are ~0, not ~13, before any wv_correct.
- **The clamp margin must be WIDER than the detrend fit boundary, and sourced from a CLEAN window
  PAST the boundary transient** (learned 2026-09-09, Oleg caught a raised strip at the DEM left
  edge). A deg-6 polynomial detrend fit on [C0:-C0] OSCILLATES at its fit boundary (Runge-like): the
  detrended residual spikes right at col C0 (e.g. +0.10 px over cols ~700-780 when C0=700), decaying
  into the interior. If the clamp margin equals C0 and its value is sourced from `median(a[C0:C0+50])`
  - i.e. from INSIDE that spike - two bad things happen: the flat edge value is biased, and the spike
  itself (just past the clamp) survives into the applied table, mis-correcting the first ~100 valid
  columns and raising a strip in the stereo DEM. Fix: set EDGE (clamp margin) well past the transient
  (e.g. 920 when C0=700) and source the flat value from a clean window `median(a[EDGE:EDGE+100])` that
  is in real signal, not the spike. ALWAYS plot the shipped correction's first/last ~1600 cols and
  confirm a smooth flat->signal transition with NO boundary spike before shipping. (A moving-average /
  Savitzky-Golay high-pass baseline instead of a global poly would avoid the boundary oscillation
  entirely - consider it if the poly edge keeps biting.)
- Guiding PRINCIPLE (Oleg): "median at 0". Any systematic band-vs-PAN shift beyond what
  we model is subtracted out; the shipped correction has net-zero shift and only the
  local per-CCD perturbation, so applying it to the band introduces NO systematic shift.

## Applying + sign

- Apply with `wv_correct` using the per-column dx/dy table onto the band (band-indexed,
  applies directly to the dg_mosaic'd + valid-cropped band - the same frame it was
  measured on).
- **SIGN IS EMPIRICAL.** The green->pan direction can flip the correction sign vs older
  conventions. Try BOTH +corr and -corr and keep whichever SHRINKS the fence in the
  verify re-measure. Validate on the small batch (10 scenes) before scaling to all - "if
  we screw up, only 10 not 100".

## Verifying

- **Single-image verify:** re-correlate CORRECTED-band -> the SAME PAN (rerun the
  measure pipeline on the corrected band), disp_avg; the fence std must DROP. Before/after
  per-column plot.
- **Ultimate stereo-DEM verify (the true test):** take a real green STEREO PAIR (two
  views, good convergence angle from the XML off-nadir angles), bundle_adjust them
  (`-t dg`, DG linescan; output adjusted CSM `.adjusted_state.json` - see the
  asp-photogrammetry DG rule), mapproject BOTH views at native MEANPRODUCTGSD onto a
  fetched+blurred Copernicus reference DEM, run stereo -> point2dem --errorimage ->
  colorized hillshade + triangulation-error. Do this BEFORE correction (expect BOTH
  jitter AND CCD fence artifacts) and AFTER wv_correct (subpixel/centered-at-0 -> cams
  still valid, NO re-bundle; reuse the same adjusted_state.json cams, re-mapproject).
  EXPECT: jitter unchanged, CCD artifact GONE. That is the win.

## Visual inspection is HUGE - inspect 1D graphs AND 2D products, before AND after

Never conclude from stats alone. For the stereo-DEM verify:
- Use GDAL's `gdaldem hillshade` on the DEM, NOT ASP's hillshade - GDAL's shows the
  jitter and CCD artifacts MUCH more clearly. CCD artifacts are VERTICAL streaks
  (along-column fences); jitter is a broader cross-track waviness.
- After `point2dem --errorimage`, colorize the TRIANGULATION error (a.k.a. intersection
  error - the two terms are INTERCHANGEABLE; it is `run-IntersectionErr.tif`) and look at
  it. The CCD artifact shows in BOTH the hillshade AND the tri-err.
- Plot BEFORE-correction (colorized hillshade DEM + colorized tri-err: expect jitter +
  lens distortion + CCD fences) and AFTER-correction (same two: expect jitter + lens
  distortion STILL there, CCD fences reduced/gone). Show them side by side.
For the 1D correction curves: plot dx/dy BEFORE (raw grand-avg fence) and AFTER
(re-measured on the corrected band, i.e. after applying the correction + detrend) - the
after curve should be flat/centered ~0 with the worst spike reduced and NO new spikes.
And inspect the CORRECTION ITSELF before shipping: detrended, centered ~0, overall
modest magnitude, worst spike not wild. If it is huge or spiky -> STOP, regression.

## Plotting + organization conventions

- **Fixed y-axis across ALL plots for a cell** (e.g. -0.3..0.3), so split-half, combined,
  before, and after are directly comparable. Corrections are tiny; a shared range is the
  only way to compare. Remove the median (or show the detrended) for viewing.
- **Process each TDI and each scan direction SEPARATELY** (forward vs reverse are
  different; each TDI is its own correction). A "cell" = one (TDI, scandir) group.
- Green MS native GSD ~1.2-1.4 m (read MEANPRODUCTGSD); PAN ~4x finer.
- A per-view L/R pick per date is a subtle bias (unsystematic sampling of the along-scan
  phase); going forward consider using both L+R views or a random subset. Keep noting it.

## Shipping a correction into wv_correct (the built-in-table path)

wv_correct's multispectral correction is DATA-DRIVEN - no C++ change, no rebuild of
logic. Everything lives in `src/asp/WVCorrect/` (source) and is installed to
`share/wv_correct/`. Mechanism (traced 2026-09):
- `ms_correction_lookup.txt`: rows `SATID BAND TDI SCANDIR CORRECTION_IMAGE ROW`.
  wv_correct reads the image XML (`BANDID=Multi`, `SATID`, `SCANDIRECTION`, and
  `tdi = tdi_multi[band-1]` - so `--band 3` picks green), matches a row, and loads
  that ROW of that TIF.
- The CORRECTION_IMAGE is a float32 TIF, ONE ROW per (TDI,scandir) cell, columns
  `[0:N]=dx`, `[N:2N]=dy`, where N = green image width (WV02 N=8795, WV03 N=10651).
  Pack with the existing `form_corrections_image.py` (row = concat(dx,dy)); OUR
  format matches it exactly.
- Both `--dx/--dy` and the lookup feed the SAME `wv_correct(img,dx,dy)`, so a TIF
  with the same values == `--dx/--dy`. ALWAYS test both with
  `--print-per-column-corrections` (must match to float precision) or diff the two
  corrected images (expect median 0, max ~0.002 px = float32 storage).
- INSTALL: each shipped data file needs an explicit `install(FILES "src/asp/
  WVCorrect/<name>" DESTINATION .../share/wv_correct)` in the TOP-LEVEL
  `CMakeLists.txt` (mirror the WV02 line; add one per new TIF). ONLY the TIFs +
  lookup ship - the workflow scripts (.m/.py/.sh) are source-only, in NO install
  rule (ship the product, not the "funny business").
- Add a tiny regression test (a small green crop + its XML + `wv_correct --band 3`
  + validate vs gold). Existing `ss_wv_correct*` tests are PAN only. Test data/gold
  live on disk (l1), NEVER git.
- COMPRESSION: the per-column residual is noisy, so it compresses poorly - full
  float32 DEFLATE(pred3) ~300KB, 4-digit rounding saves only ~2.5%. Keep full float32.

## Existing WV2 workflow vs our WV3 refinements (README_MULTISPECTRAL)

The old README_MULTISPECTRAL documents the SAME core (green->PAN per-column
disparity, multi-scene average, detrend, pack, lookup) with scripts ms_ccd_solve.sh
/ ccd_process.py / ms_ccd_verify.sh / form_corrections_image.py. Our refinements
(2026, `src/asp/WVCorrect/wv3_green_combine.py` + a new README section): pan-pad +
tight centered search (vs wide/raw -13 offset); moving-average-1600 detrend + edge
clamp (vs ccd_process.py; edge clamp matters - garbage edges shift the first/last
image columns ~0.2-0.3 px); per-scene outlier scan (one failed-correlation scene at
std 4.3 inflated a grand-average 8x); and 2D stereo-DEM validation (vs colormap only).
