---
name: visual-inspection
description: Visual inspection of geo rasters and artifacts - the warp-to-common-grid, hillshade, red/green overlay, colorbar procedure for eyeballing DEMs/geodiffs/orthos/tri-err, the HTML Artifact tool workflow, and image sizing for artifacts/previews. Load when asked to eyeball, inspect, look at, or compare any raster, or to build an HTML artifact.
---

## "HTML Artifact" = the Artifact Tool (know the drill)

When Oleg asks for an "HTML artifact" (or just "artifact"), that means: use the
`Artifact` tool. I write an HTML file for the CURRENT project (plots, colorized
rasters, tables, whatever), publish it, and it pops up as a claude.ai-hosted URL
in a browser tab (private by default, shareable). No need to re-explain what it
is each time. Key rule: an artifact is SELF-CONTAINED - a strict CSP blocks every
external file, so all images must be INLINED as base64 data URIs. Follow the
sizing rule just below (downsample hard). For a purely local glance instead of a
hosted URL, use `SendUserFile ... display:render`. See also
`~/projects/visual_raster_inspection.sh` (colorbar/preview recipe) and
`~/projects/html_for_google_docs.sh` (base64-embedded HTML for Google Docs).

## Artifact and Preview Image Sizing

For HTML artifacts and uploaded previews, downsample DRASTICALLY: <=1000 px long side,
<1 MB per image (dpi ~80-100). Over ~1 MB or ~1000x1000 px is overkill. Full-res stays on
disk. Detail in `~/projects/visual_raster_inspection.sh`.

## NO TEXT BAKED INTO FIGURES - put it in the caption UNDERNEATH (long-standing rule)

Do NOT draw titles/annotations/long labels as PIXELS inside a matplotlib/plot image - they get
cut off, don't reflow, and can't be edited. Keep the RASTER clean (the data, a colorbar, minimal
axis ticks) and put ALL explanation as PLAIN TEXT in the HTML `<figcaption>` BELOW the figure.
For side-by-side panels, DON'T title each panel in-image; write one caption under that says "Left:
... Right: ...". Same for stacked panels ("Top/Bottom: ..."). This keeps text legible, complete,
and reflowable, and lets the wording be revised without regenerating the PNG. (Oleg has asked for
this repeatedly; suppress `ax.set_title`, keep colorbars, describe in the figcaption.)
- When comparing two things side by side, make the panels the SAME extent/scale so differences are
  real, and say which is which in the caption. Prefer PURE grayscale hillshade (no color) for a
  shift eyeball (color distracts from whether features move); a colorized version can go alongside.
- ALWAYS hillshade with `gdaldem hillshade -multidirectional -compute_edges`, NOT a homemade
  numpy-gradient shade - it looks right at edges and matches what the correlator sees. (More
  hillshade/align examples in `docs/tools/pc_align.rst` and the dem-comparison + pc-align skills.)

## INSPECT CONSTANTLY - eyeballing IS the job (photogrammetry hat)

Wear the photogrammetry hat: **observation of IMAGE PATTERNS tells you things RMSE/medians
cannot.** A number can look fine while the raster shows a tilt, a bowl, a seam, a junk patch,
a mirrored/rotated frame, a checkerboard, an edge notch, a shifted feature. So at EVERY step,
do not just print stats - render the product and LOOK. State the HYPOTHESIS first (what it
should look like), then confirm by eye before moving on. Frequent inspection is not overhead;
it is the work, and it catches blunders (junk pairs, wrong datum, wrong convention) that stats
hide. Specific comparisons to eyeball (all after warping to a COMMON grid/extent/proj):
- **DEM vs DEM**: colorized + hillshade, side by side; and the geodiff (dz) with a symmetric
  diverging colorbar. A junk pair shows as a bright blob in the DEM AND the tri-error map.
- **image/ortho vs hillshaded DEM**: overlay or side-by-side - do features (shoreline, roads,
  buildings) land on the terrain? Mis-registration/tilt/rotation jump out.
- **hillshade vs hillshade** (your DEM vs the reference): the two must look visually SIMILAR
  before correlating them; a horizontal shift is visible as offset relief.
- **residual/pointmap CSVs**: plot the points colored by value over a terrain background, and
  ZOOM to the core (crop far outliers, cap the colorbar) to reveal spatial STRUCTURE (e.g.
  low-on-texture / high-on-water, or a per-frame radial pattern) - structure that the median
  hides. Even a dense match/disparity CSV can be gridded to a raster (point2dem) and inspected.
- **disparity** (run-F.tif / disparitydebug): a smoothly varying, near-constant field = a good
  correlation lock; blocky/noisy = a bad one. Always look before trusting dh/dv.
REVIEW YOUR OWN PLOTS after making them - re-open the PNG and read it critically, don't just
save and move on. Keep ADDING inspected figures to the running HTML artifact as you go.

## Visual Raster Inspection - "Claude has eyes"

Claude can SEE images - use vision to verify rasters (orthos, DEMs, geodiffs,
camera/rotation alignment). Full technique, recipes, and where preview files live
(with the data on pfe, not /tmp): **`~/projects/visual_raster_inspection.sh` -
READ IT before inspecting/eyeballing any geo raster.**

**WHEN THE USER SAYS "eyeball" / "inspect" / "look at" / "compare" an ortho, DEM,
geodiff, tri-err, or any geo raster, it ALWAYS means this EXACT procedure (do NOT
re-derive it each time, do NOT skip a step):**
1. Put EVERY raster on ONE identical grid first: `gdalwarp -t_srs <one proj>
   -te <one extent> -ts <one size> -r cubicspline` (same PROJECTION, same EXTENT,
   same GRID, cubicspline). Comparing rasters on different grids/framing - or with
   raw numpy (index-aligned) - is NONSENSICAL and worthless.
   **`-te` order is `xmin ymin xmax ymax`** - NOT the `--t_projwin`/`-projwin` order
   (`xmin ymax xmax ymin`). `gdal_win.sh` emits projwin order by default; pass a 2nd
   arg (`gdal_win.sh dem minmin`) for `-te`. Feeding projwin order to `gdalwarp -te`
   swaps ymin/ymax and silently builds a FLIPPED south-up grid (positive Y pixel),
   which surfaces later as an upside-down plot. After any warp, `gdalinfo` and confirm
   identical Size/extent, NEGATIVE Y pixel size (north-up), right proj - especially
   before correlation, where both inputs must be pixel-for-pixel identical. When
   overlaying points on a raster, plot in projected coords (imshow `extent` in UTM,
   scatter easting/northing; flip south-up arrays to north-up) - never raw pixel indices.
2. HILLSHADE any DEM before viewing: `gdaldem hillshade -multidirectional`. NEVER
   eyeball raw elevation; you compare terrain by its hillshade.
   **HILLSHADE AT FULL RES *THEN* DOWNSAMPLE (CRITICAL gotcha).** Always run gdaldem
   on the full-resolution DEM and downsample the *hillshade* for the preview. If you
   downsample the DEM first and hillshade after, low-relief/flat terrain (Key West
   ~0-5 m) loses all its slope and the hillshade comes out uniform/blank. For flat
   terrain also add vertical exaggeration (`-z 3`) to reveal the relief. Same order
   for any slope/aspect product: compute at native res, then downsample the product.
3. Downsample to <=1000 px, write PNG, THEN look.
4. Judge REGISTRATION only by the red/green hillshade overlay (aligned = yellow),
   NEVER by dz/geodiff std (blind to horizontal misregistration on low relief).
5. Colorize a geodiff/tri-err/dz with a matplotlib colorbar (per-panel, numeric
   ticks only; plasma for error, RdBu_r for signed), not bare grayscale.
This applies to the mapproj ortho-on-hillshade geometry check too: warp the ortho
and the DEM hillshade to the same grid, then look.

**NEVER combine two geo-referenced rasters with raw python/numpy pixel ops.** numpy
aligns arrays by row/col INDEX, not ground coordinates, so differencing/overlaying/
comparing rasters on different grids or projections in python is silently wrong.
ALWAYS use the projection-aware tools: `geodiff` to difference two DEMs/rasters (it
regrids the 2nd onto the 1st, respects proj+datum), and `gdalwarp` to put rasters on
ONE common grid (-t_srs + -te + -tr/-ts, -r cubicspline). Warp BOTH onto the shared
canonical grid FIRST, THEN read into python only to DISPLAY (imshow) or compute robust
stats. Do NOT warp the raster whose pattern you care about onto a DIFFERENTLY-centered
projection - it tilts/rotates it into an artifact (burned 2026-07-21: warped a dz onto a
CTX pair's stereographic center 0.12 deg off, faking a cross-track pattern). Detail in
`~/projects/visual_raster_inspection.sh`.

Colorizing a raster for inspection (geodiff/dz, disparity, tri-err): render WITH a
colorbar (matplotlib, not bare `gdaldem color-relief`). EACH plot gets its OWN vertical
full-height colorbar on the RIGHT, **numeric ticks ONLY, no unit label** (unit goes in
the caption); tick `labelsize ~16`; NEVER a shared colorbar. Robust clamp, not min/max.
Multidirectional hillshade (`gdaldem hillshade -multidirectional`) for DEMs. Full recipe
(pfe gdal-vs-matplotlib env split): `~/projects/visual_raster_inspection.sh` section 5.

**Interest-point / match-point plots: RED FILLED balls.** When overlaying tie-point
matches or interest points on an image, draw them as red FILLED circles
(`scatter(..., c='red', marker='o')`) - filled, not hollow, not yellow. House style
for all match plots (docs, notes, chat). Same rule in the asp-photogrammetry skill.

**COLORMAP CONVENTION (permanent, Oleg 2026-08-18; used in the ASP doc figures):**
- **Unsigned / error** (tri-err, |dz|, residual/disparity magnitude): **`plasma`,
  `vmin=0`**. Chosen deliberately because plasma's low end is deep PURPLE, so it stays
  visually distinct from BLACK nodata (black is reserved for nodata - never let a
  colormap's min be black). Not magma, not viridis - plasma.
- **Signed** (dz, dh, before-minus-after, DEM-minus-ref): **`RdBu_r`** (blue negative,
  white zero, red positive), **symmetric clamp** about 0.
- **nodata = BLACK** for every colorized plot (`cmap.set_bad("black")`, nodata->NaN),
  so it reads as "not covered", distinct from real low values.
The ASP tools `colormap`/`point2dem --colormap-style` take these names too
(`plasma`, `inferno`, `viridis`, ...); the full list is in the ASP
`docs/tools/colormap.rst`. Canonical plotting recipe with these conventions:
`~/projects/visual_raster_inspection.sh` (colormap-conventions section).
**Reusable single-panel renderer (copy it):**
`~/projects/cassis_asp/ctx_k19_jitter_scripts/render_panels.py` — reads a raster
via gdal, tight valid-data crop, p95 clamp, plasma(error)/RdBu_r(signed), nodata
black, own full-height right colorbar via `make_axes_locatable` (numeric ticks,
labelsize 16), `ax.axis("off")`, writes a self-contained PNG. Run it in a
matplotlib+gdal env (pfe `geo`/`isis10`; the ASP env has gdal but NO matplotlib).
Note: ASP's `colormap --legend` gives only an UNLABELED strip — for a real labeled
colorbar use matplotlib (this renderer), not the ASP tool.

**No text inside a figure that ships with an RST/HTML caption - the caption below carries ALL of it.** No panel titles ("before"/"after"/"hillshade"), no colorbar unit label ("meters"/"pixels"), no baked-in descriptions. KEEP only the colorbar tick numbers (they carry the range/scale). The prose caption names the panels left-to-right and states the units and clamp. (Oleg 2026-08-20.)

**No black FRAME around any image panel (hillshade, ortho, colorized raster) - NEVER put one.** Call `ax.axis('off')` (or hide all four spines) so matplotlib draws no box or ticks around the imshow. A framed panel reads as sloppy. This applies to every panel in every figure. NOTE the distinction: the FRAME (axis spine rectangle) is gone, but nodata PIXELS are rendered BLACK (`cmap.set_bad("black")`) - black is reserved for nodata, which is why error plots use `plasma` (purple min, not black min). Do not confuse "no black frame" with "no black nodata".

**All image panels in a multi-panel figure must be the SAME height (1:1).** A colorbar attached with `fig.colorbar(im, ax=a)` steals width from that panel, and with `aspect='equal'` a narrower image is also shorter - so panels with a colorbar end up shorter than those without. Fix: give EVERY panel an equal-width right slot via `make_axes_locatable(a).append_axes("right", size="6%", pad=0.05)` - a real colorbar on the panels that need one, `cax.axis("off")` (invisible spacer) on the rest. Then all panels render at identical height and the colorbars are full panel height. (Oleg 2026-08-20.)

Match-point inspection: `~/bin/plot_matches.py` overlays an ASP .match file on both images and reports the residual to the best-fit translation (the real-vs-junk metric for co-registered pairs). For the stereo_gui solid-red-dot look use `--red --radius N`.

**Low-texture pc_align (CRITICAL, see the alignment primer in `~/projects/visual_raster_inspection.sh`):** on bland terrain (few craters) the correlator dh/dv MEDIAN and geodiff/dz BOTH LIE - swamped by spurious ~0 matches on featureless plains, so a real ~20 px crater misalignment reads as "2 px". NEVER judge an align by dh/dv median or dz there; ALWAYS eyeball a ZOOMED, fully-covered textured window (crater/ridge) as a red/green hillshade overlay (aligned = yellow, misaligned = red/green fringes). Sparse IP (`pc_align --initial-transform-from-hillshading rigid`, no match file) beats dense `--correlator-mode` (which locks onto the plains); `--compute-translation-only` kills spurious-rotation blowups; regrid both DEMs to the same grid first. Burned a whole session trusting the correlator median.

Checking a bundle_adjust `pointmap.csv` (GCP / from-DEM points) against a reference DEM with `geodiff` (split by population, the `--csv-srs` gotcha): see `~/projects/visual_raster_inspection.sh`. Keywords: bundle_adjust pointmap.csv, geodiff --csv-format, heights-from-dem on-DEM check, fix-gcp-xyz.

Google-Doc-ready section (prose + real tables + inline figures, in one copy-paste): build a self-contained HTML with base64-embedded images, open in Chrome, select-all, copy, paste. See `~/projects/html_for_google_docs.sh`.

## Eyeball after EVERY step - hypothesis then confirm (the core discipline)

Geospatial/mapping tools are FRAGILE and fail SILENTLY (bad otsu/KDE mask, junk
correlation, wrong-grid mapproject, spurious pc_align rotation). The recurring
expensive failure is running a multi-step pipeline WITHOUT LOOKING, so a bad
product at step 2 is caught 10 steps later after wasted compute. For EACH product
(image or DEM - both inspectable): FIRST state the hypothesis (what it must look
like), THEN colorize/hillshade -> PNG -> LOOK to confirm, BEFORE the next step.
Never proceed on an unlooked-at product. To compare two rasters by eye you MUST
first `gdalwarp` both to the SAME grid+extent+projection, then PNG them - otherwise
the comparison is meaningless. MASKS: overlay the mask on its source image (or show
masked-vs-raw side by side) and confirm the boundary sits at the shoreline - keeps
land (runway/buildings), drops water (coral/underwater). Frequent inspection IS the
work, not overhead.
