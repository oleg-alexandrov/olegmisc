---
name: wv-ccd-correction
description: WorldView (DigitalGlobe/Maxar) CCD artifact correction with wv_correct - measuring a per-column dx/dy correction by correlating a band against its own PAN (green->pan for the WV3 green MS work), building the correction (dg_mosaic concatenate, crop the NTF zero-padding, 4x average-downscale PAN, robust-mean disp_avg, deg-6 detrend), applying it, and verifying it (single-image re-correlate + the ultimate stereo-DEM before/after test). Load before any WorldView CCD / green-band correction, wv_correct, or green<->PAN disparity work.
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

## Plotting + organization conventions

- **Fixed y-axis across ALL plots for a cell** (e.g. -0.3..0.3), so split-half, combined,
  before, and after are directly comparable. Corrections are tiny; a shared range is the
  only way to compare. Remove the median (or show the detrended) for viewing.
- **Process each TDI and each scan direction SEPARATELY** (forward vs reverse are
  different; each TDI is its own correction). A "cell" = one (TDI, scandir) group.
- Green MS native GSD ~1.2-1.4 m (read MEANPRODUCTGSD); PAN ~4x finer.
- A per-view L/R pick per date is a subtle bias (unsystematic sampling of the along-scan
  phase); going forward consider using both L+R views or a random subset. Keep noting it.
