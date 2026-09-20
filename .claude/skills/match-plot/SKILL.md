---
name: match-plot
description: How to plot/overlay interest-point matches, tie points, or an ASP .match file on their two images. Load EVERY time you are about to visualize matches/tie points/interest points/a .match or .vwip file, or overlay correspondences on a pair of images (hillshades, orthos, raw frames) - BEFORE writing any plotting code. Enforces the house style (solid red filled balls, no connecting lines) and the canonical tool.
---

# Plotting interest-point / tie-point matches

## Generating matches when sparse IP fails (dense-from-disparity)

Before plotting, you need matches. Sparse IP (ipfind/ipmatch, or bundle_adjust
`--ip-per-tile`/`--matches-per-tile`, `--ip-detect-method 1` = SURF) can FAIL between
radiometrically-different images (e.g. OHRC byte vs NAC reflectance gave 9 garbage
matches). When the dense CORRELATOR still locks (census `--cost-mode 3`, `--corr-search`
alone with `--corr-seed-mode 0` is enough), get DENSE matches instead: run
`parallel_stereo` WITH cameras on the two MAPPROJECTED images + the proper DEM and
`--num-matches-from-disparity N` - it computes the disparity, samples dense matches on
the mapproj images, then UNPROJECTS them to the RAW images, writing raw-image matches
usable by bundle_adjust/jitter_solve even when IP failed. (Doc: bundle_adjustment.rst;
mechanics in [[bundle-adjust]].) THEN plot those with the house style below.


**Trigger:** any request to plot, overlay, show, or sanity-check matches, tie points,
interest points, a `.match` file, or `.vwip` points on their image(s). Load this BEFORE
writing plotting code - do not hand-roll a match plot from memory.

## The one rule (house style, non-negotiable)

- **Solid RED FILLED balls. No lines.** Draw each match/IP as a filled red circle
  (`c='red'`, filled, not hollow, not yellow, not position-colored), big enough to see
  (radius ~6-8 px). **NEVER draw lines connecting the corresponding points across the
  two panels** - no "fan" of correspondence segments. Correspondence is read by matching
  the red-dot CLUSTERS by eye between the two side-by-side panels, never by drawn lines.
  (Hand-rolled lime dots + connecting lines is exactly the wrong thing; caught 2026-09-17.)

## Use the canonical tool - do NOT hand-roll

```
~/anaconda3/envs/bathy/bin/python ~/bin/plot_matches.py \
  LEFT.tif RIGHT.tif matches.match out.png [width] [maxpts] --red --radius 7
```
- `--red` gives the solid-red-filled-ball / stereo_gui look; `--radius 7` makes them big.
- It prints `residual to best-fit translation: median ... px [good/mediocre/...]` - the
  real-vs-junk metric for co-registered pairs (tight = real, large = junk or warp).
- It pairs `left[i]` with `right[i]` using the match file's block order: the FIRST IP
  block goes with `LEFT.tif`, the SECOND with `RIGHT.tif`.

## Panel order MUST match the document (consistency rule)

pc_align writes the match file as `<ref>__<src>.match` (reference block first), so the
tool puts the REFERENCE image on the left. If an existing figure in the same
document/report already shows the pair in a specific order (e.g. "Left: our DEM, Right:
COP"), the match plot MUST use the SAME order - do not flip it (Oleg gets confused by
swapped panels). To control the order while still using `plot_matches.py`, SWAP the two
IP blocks in the `.match` file (write source block first, then reference block; swap the
two leading `uint64` counts too), then pass the images in the desired left/right order.
Each IP record is `8(x,y f32)+8(xi,yi i32)+12(orient,scale,interest f32)+1(polarity i8)
+8(octave,scale_lvl u32)+8(ndesc u64)+ndesc*4(desc f32)` bytes - read each record's raw
bytes to reorder blocks losslessly.

## Related

Part of the [[visual-inspection]] family (the inspection hub). The same red-filled-ball
rule is stated in [[asp-photogrammetry]]. For judging whether the matched DEMs actually
align (red/green hillshade overlay, warp-to-common-grid), route back to visual-inspection.
