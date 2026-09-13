---
name: jitter-solve
description: Refine per-line camera poses of a CSM LINESCAN with jitter_solve to remove jitter, twist, or a low-frequency bend, constrained to a reference DEM and/or GCP. Load when asked to run jitter_solve, "solve for jitter", remove a linescan twist/wobble, or set num-lines-per-position/orientation, anchor points, or GCP for a linescan refine. Carries the knot-count rule, the anchor/heights/GCP options, and the hard lessons on when it explodes and the tri-error-vs-terrain-match tension.
---

# jitter_solve: refining linescan per-line poses (jitter / twist)

`jitter_solve` resamples a CSM linescan's position and orientation samples and
optimizes them against tie points, a reference DEM (`--heights-from-dem`), anchor
points, and optionally GCP. Use it to remove jitter, a low-order twist, or a
residual bend AFTER the linescan is already well-initialized and aligned.

## VALIDATE the linescan first (do not skip)

Before jitter, run plain stereo with the linescan (no mapproject,
`--alignment-method affineepipolar`) and grid a DEM. If that DEM is horrible, the
linescan MODEL is bad - fix the initialization (refit from a good pinhole, then
pc_align the CAMERAS; see [[solve-intrinsics]] prereqs and [[dem-sanity-check]]),
do NOT jitter. A recognizable-but-tilted DEM is the right starting point; the tilt
is usually a rigid rotation pc_align removes, leaving a small residual for jitter.

## Knot count = the single most important stability knob

`--num-lines-per-position` and `--num-lines-per-orientation` set how densely the
pose is sampled. With `NL` total scan lines, `NL/K` gives about `K` knots. FEW
knots (3 positions, 3 orientations) fit a low-order TWIST or tilt and CANNOT
oscillate. MANY knots chase high-frequency jitter but readily OSCILLATE. Start
minimal (K=3) and only add knots if a real high-frequency signal is present.
Count NL from the camera json (`m_nLines`).

## Constraints

    jitter_solve img1 img2 cam1.json cam2.json [gcp.gcp] \
      --match-files-prefix dense/run \
      --num-lines-per-position $((NL/3)) --num-lines-per-orientation $((NL/3)) \
      --heights-from-dem ref.tif --heights-from-dem-uncertainty 200 \
      --num-anchor-points 10000 --anchor-dem ref.tif \
      --anchor-dem-uncertainty 500 --num-anchor-points-extra-lines 500 \
      --camera-position-uncertainty 500,500 \
      --num-iterations 100 -o out/run

- `--heights-from-dem` + `--heights-from-dem-uncertainty` pin triangulated tie
  points to the reference surface (smaller = stiffer height pin).
- Anchor points (`--num-anchor-points`, `--anchor-dem`, `--anchor-dem-uncertainty`)
  hold the geometry over the whole strip; `--num-anchor-points-extra-lines` adds
  anchors BEYOND the imaged lines (the linescan pose extends past the image).
- `--camera-position-uncertainty H,V` (meters) limits how far the camera centers
  move. It is a SOFT constraint - strong GCP can still overpower it (see below).

## GCP - optional, and a double-edged lever

dem2gcp turns an ours-vs-reference hillshade disparity into GCP (gcp-sigma =
ground accuracy, e.g. 30 m for Copernicus GLO-30). GCP can pull the solution to
the reference, but they are a STRONG lever: they can force huge (especially
vertical/radial) camera moves that `--camera-position-uncertainty` cannot hold.
The clean approach: run jitter BOTH with GCP and without (heights-from-dem +
anchors + ties only), and compare `run-final_residuals_stats.txt` /
`pointmap.csv` - keep whichever is better behaved.

### GUARD noisy GCP with `--gcp-robust-threshold` (essential when the dh/dv is noisy)

When the ours-vs-reference disparity that dem2gcp is built from is noisy or patchy
(common - the hillshades are hard to correlate), some GCP are garbage and, being a
strong lever, they can dominate the cost and drag the solution wild. Guard against
this with `--gcp-robust-threshold` (a robust cost of `--cost-function` type applied
to the GCP residuals).
- DEFINITION (jitter_solve.rst / bundle_adjust.rst): the GCP residual is
  `|optimized GCP position - measured GCP position| / gcp_sigma` (the sigma from
  the GCP file). The threshold is in those NORMALIZED (sigma) units. Above it, a
  GCP's contribution is attenuated by the robust loss.
- CHOOSE it comparable to the largest normalized residual expected from REASONABLE
  GCP - not to the noisy outliers. Do NOT guess a big number: run once, read the
  GCP residual distribution from `run-final_residuals_stats.txt` / `pointmap.csv`,
  and set the threshold near the knee (a bit above the bulk of good-GCP residuals)
  so crazy movers are down-weighted while good control still pulls. Too large a
  threshold = no guarding (noisy GCP dominate); too small = even good GCP are
  attenuated and the control does nothing.
- See also `--max-gcp-reproj-err` (a hard cap on GCP reprojection error).
This is exactly the fix when the dh/dv feeding dem2gcp has visible noise.

## HARD LESSONS (learned on KH-7, 2026-09; watch for these)

- Jitter can EXPLODE. Loose rope (large `--camera-position-uncertainty`, loose
  height/anchor, strong GCP) makes the cameras move kilometers and BENDS the strip
  into a "banana". Symptom: `run-camera_offsets.txt` shows km-scale moves; the DEM
  curls; coverage collapses.
- On a TWO-VIEW panoramic strip there is a FUNDAMENTAL TENSION: jitter lowers the
  triangulation error (a more self-consistent geometry) but can DEGRADE terrain
  agreement (high-pass correlation vs the reference), because the per-line pose
  freedom reduces tri error by bending the surface AWAY from the true terrain.
  ALWAYS judge by hp correlation / dz / dh-dv vs the reference, NOT tri error
  alone. If tri error drops but hp drops too, the jitter hurt.
- Fitting a plausible GCP set to ~2 px residual is NOT success if the resulting
  DEM matches the reference worse. Re-stereo and re-measure every time.

## Inspect (report files)

`run-camera_offsets.txt` (how far cameras moved - km = trouble),
`run-initial/final_residuals_stats.txt`, `pointmap.csv` initial vs final,
`anchor_points.csv` initial vs final. Plot pointmap/anchor side by side.

## Doc pointer

`docs/tools/jitter_solve.rst` (num-lines-per-position/orientation, anchor points,
heights-from-dem, GCP, output report files). Alignment/intrinsics context:
`docs/bundle_adjustment.rst`. Related: [[solve-intrinsics]], [[dem-sanity-check]].
