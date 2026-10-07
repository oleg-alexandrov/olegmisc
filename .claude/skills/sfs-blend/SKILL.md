---
name: sfs-blend
description: >-
  Blend an SfS DEM back toward LOLA in permanent shadow with ASP sfs_blend, and
  babysit the result crater by crater when the shadow fill leaves a visible seam.
  Carries the default production parameters, the blend mechanism (weight ramp
  width = lit + shadow band, Gaussian sigma), the recurring "blunder inside the
  crater" (LOLA sits above SfS in shadow so a narrow band packs the height step
  into a sharp ring), and the provably-lossless clip workflow: crop a padded
  clip, re-blend variants locally with wider smear, validate bit-for-bit against
  the full-site blend, eyeball the crater interior, pick the smear, patch back.
  Load when running sfs_blend, tuning lit/shadow/sigma, or fixing a seam or step
  at the edge of a permanently shadowed region in an SfS DEM.
---

# sfs_blend: shadow fill, and babysitting the seam

`sfs_blend` takes the SfS DEM and, in permanently shadowed regions (where the
max-lit image mosaic is below a threshold), replaces it with the LOLA DEM, with
a smooth transition at the lit/shadow boundary. SfS has no illumination signal
in permanent shadow, so it fabricates garbage there (typically a spurious pit or
a bright mound split by a hard line). The blend swaps that for LOLA.

The default production wrapper is `~/projects/sfs/sfs_blend.sh` (and a per-site
copy such as `~/projects/sfs_BCU2314-BDU1224-MM/run_sfs_blend.sh`). Terrain
bounds requirement: both DEMs must be on the same half-integer 1 m grid (see the
[[sfs]] skill, make_ref_dem, and the terrain-bounds section of sfs_usage.rst).

## Default parameters (the production recipe)

```
sfs_blend --lola-dem ref/lola_1mpp_extra_noblur.tif \
  --sfs-dem sfs_dem.tif --max-lit-image-mosaic max_lit_mosaic.tif \
  --image-threshold 0.005 \
  --lit-blend-length 25 --shadow-blend-length 5 \
  --min-blend-size 25 --weight-blur-sigma 5 \
  --output-dem sfs_dem_blend.tif --output-weight sfs_dem_weight.tif
```

- `--image-threshold 0.005` is the LRO NAC lit-vs-shadow cutoff (same value the
  azimuth cull uses). Do NOT move it to fix a seam: lowering it shrinks the
  shadow and exposes more fabricated SfS, raising it grows the shadow onto the
  good lit walls. Leave it at 0.005 and fix the seam with the blend width.
- `--min-blend-size 25`: shadow holes smaller than this keep SfS (not blended).

## The mechanism (from sfs_blend.cc, do not guess it)

The SfS-vs-LOLA weight ramps linearly from 0 (pure LOLA, deep shadow) to 1 (pure
SfS, deep lit) across a band whose total width is
`shadow-blend-length + lit-blend-length`, centered on the boundary, then the
whole weight field is Gaussian-smoothed by `weight-blur-sigma`. At the boundary
the weight is `shadow/(shadow+lit)`. The blended height is
`weight*SfS + (1-weight)*LOLA`. So the **band width is the real lever** and sigma
just rounds the ramp's corners. The blend is LOCAL: a pixel is only influenced
within the band plus the blur reach.

## The recurring blunder: a seam inside the crater

Inside the shadow LOLA sits systematically ABOVE SfS (measured on
BCU2314-BDU1224-MM: median +0.3 to +1.4 m, p90 up to +6 to +9 m near the rim).
The blend therefore has a positive height step to absorb at the boundary. A
narrow band packs that step into a steep ramp, which reads as an abrupt tonal
ring on the crater floor. A step Δh over band width W makes an added slope of
about Δh/W: a 9 m near-rim step across the default 30 px band is roughly a 30
percent grade seam, across a 120 px band about 7 percent. Widening the band
spreads the step into a gentle ramp. It does NOT recover terrain: the floor
stays at LOLA height, because SfS has no signal there. The goal of smearing is
only to make the transition honest-looking, not to invent detail. Say that
plainly.

## Babysitting a seam on a clip (provably lossless)

Never re-run the full-site blend to try smear settings. Because the blend is
local, a padded clip reproduces the full-site result exactly over the display
window, so experiment on a tiny clip (even on the Mac) and patch back.

1. **Crop a padded clip** of the three inputs (LOLA, SfS DEM, max-lit mosaic)
   around the crater. Pad by at least the blend reach = `lit-blend-length` plus
   about `7*weight-blur-sigma`. **300 px covers everything up to lit 150 / sigma
   15.** Crop the DEMs where they live (pfe head node, single-thread
   `gdal_translate -projwin`, head-node-safe); the max-lit mosaic can be cropped
   locally. All three snap to the same half-integer grid, so they stay aligned.
2. **Re-blend variants locally** with progressively wider smear (scale the three
   knobs together), tiny clips, no OOM risk.
3. **Validate** by re-blending the clip with the PRODUCTION settings and
   differencing against the shipped full-site blend over the display window:
   expect `max|Δ| = 0.0000 m`. If it is not zero, the pad is too small or the
   grids do not match. This bit-exact check is what licenses the whole workflow
   and lets a clip verdict be patched straight back into the big blend.
4. **Eyeball the crater interior** (see below), pick the smear, patch back (or
   re-run the full-site blend once with the chosen settings, which is identical).

Variant ladder used (lit / shadow / sigma, band width):

| name    | lit | shadow | sigma | band px | use |
| :------ | --: | -----: | ----: | ------: | :-- |
| current |  25 |      5 |     5 |      30 | production |
| v50     |  50 |     10 |     8 |      60 | mild |
| v75     |  75 |     15 |    10 |      90 | conservative |
| v100    | 100 |     20 |    12 |     120 | **recommended default** |
| v150    | 150 |     30 |    15 |     180 | aggressive, for the worst steps |

**Recommendation:** v100 removes essentially the whole interior seam while the
crater walls keep their real SfS texture. v150 buys only marginal extra
smoothing and starts replacing good near-rim SfS with featureless LOLA, so
reserve it for the largest steps (a c5-class ~9 m near-rim step). v75 is the safe
fallback if softening the rim is a worry. One global setting for the whole site
(v100) is preferred: a per-crater override is a per-site special-case and is
better avoided unless big steps are common.

## Eyeballing is the judge, not a number

The verdict is visual: you must SEE the seam in the crater interior and see it
relax. Two renders, both after warping to the display window:
- **Plain grayscale multidirectional hillshade** is the most sensitive to a slope
  seam: the fabricated SfS mound, the hard boundary of the default blend, and its
  disappearance under wider smear all read clearly. Use this to pick the smear.
- **Colour-hillshade** (elevation colorized on a shared scale per crater, draped
  on the hillshade) shows the height relationship, so the reader sees that LOLA
  is higher and why the step exists.
Follow the [[visual-inspection]] rules: no in-image titles (caption carries it),
nodata black, per-panel colorbar labeled "meters", hillshade with
`gdaldem hillshade -multidirectional -compute_edges` at full res then crop. Also
quantify the step as a supporting number (median and p90 of LOLA minus SfS over
shadow pixels), never as the verdict.

## Reference implementation

The full worked example (five craters, scripts, figures, the validation, and the
report) lives in `~/projects/sfs_BCU2314-BDU1224-MM/blend_clip/`:
`crop_dems_pfe.sh` (padded DEM crops on pfe, list-driven from `craters.txt`),
`run_blend_variants.sh` (the variant sweep + display-window hillshades),
`make_figs.py` (colour-hillshade panels), `build_html.py` (the report).

Related: [[sfs]] (the pipeline hub and make_ref_dem grid rules),
[[sfs-run-align]] (the stage that produces the SfS DEM and max-lit mosaic fed
here), [[sfs-delivery]] (the blend is a shipped product), [[visual-inspection]]
(the eyeball mechanics).
