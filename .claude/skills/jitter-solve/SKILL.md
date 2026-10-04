---
name: jitter-solve
description: >-
  Refine per-line CSM linescan poses with jitter_solve to remove jitter, twist, or a
  low-frequency bend, constrained to a reference DEM and/or GCP. Load when asked to run
  jitter_solve, solve for jitter, remove a linescan twist or wobble, or set
  num-lines-per-position/orientation, anchor points, or GCP for a linescan refine.
---

# jitter_solve: refining linescan per-line poses (jitter / twist)

`jitter_solve` resamples a CSM linescan's position and orientation samples and
optimizes them against tie points, a reference DEM (`--heights-from-dem`), anchor
points, and optionally GCP. Use it to remove jitter, a low-order twist, or a
residual bend AFTER the linescan is already well-initialized and aligned.

## VALIDATE the linescan first (do not skip)

Before jitter, run plain stereo with the linescan (no mapproject,
`--alignment-method affineepipolar`) and grid a DEM. If that DEM is horrible, the
linescan MODEL is bad: fix the initialization (refit from a good pinhole, then
pc_align the CAMERAS; see [[solve-intrinsics]] prereqs and [[dem-sanity-check]]),
do NOT jitter. A recognizable-but-tilted DEM is the right starting point; the tilt
is usually a rigid rotation pc_align removes, leaving a small residual for jitter.

## OBALoG vs SIFT: match detector for varying illumination

When generating match files to feed `jitter_solve` (via `bundle_adjust` or
`parallel_stereo --num-matches-from-disparity` on mapprojected images), use
`--ip-detect-method 0` (OBALoG), not SIFT (`1`), for planetary, shadow-dominated,
low-contrast, or cross-illumination scenes. Detail: the **bundle-adjust** skill (it owns
the OBALoG-vs-SIFT lesson and the ~8000-11000 vs 7-38 match counts).

## Knot count: two-view twist vs long linescan scaling

`--num-lines-per-position` and `--num-lines-per-orientation` set how densely the
pose is sampled. With `NL` total scan lines, `NL/K` gives about `K` knots.
Count NL from the camera json (`m_nLines`).

1. **Two-view panoramic / short strip (low-order twist)**:
   - FEW knots (3 positions, 3 orientations: `NL/3`) fit a low-order TWIST or tilt and
     CANNOT oscillate. Start minimal (K=3) and only add knots if a real high-frequency
     signal is present.
2. **Long linescan strips (OHRC, LRO NAC, CTX)**:
   - Strips have 50,000 to 100,000 lines. Nominal settings:
     `--num-lines-per-position 5000` to `15000` (~10-20 position knots) and
     `--num-lines-per-orientation 1000` to `4000` (~25-90 orientation knots).
3. **The orientation smearing trap (OHRC lesson)**:
   - In OHRC Pass 2, dropping `--num-lines-per-orientation` to `500` (~190 knots) caused
     severe high-frequency orientation instability on frame `20241207T1022596159`,
     creating an along-track streaked/smeared image.
   - **Rule**: Keep orientation knots at `1000` lines/knot or coarser unless high-frequency
     jitter is conclusively proven.
   - **Coupling with anchors**: `1000` (the finer, floor value) is safer to use when you
     ALSO deploy MANY anchor points (lightly weighted - see the anchor-balance section): the
     dense anchors damp the extra orientation freedom so it stabilizes instead of oscillating.
     Finer orientation + sparse anchors is the oscillation risk; finer orientation + many
     light anchors is fine. Still never go below 1000.

## Constraints and invocation

    jitter_solve                                       \
      --image-list run-image_list.txt                  \
      --camera-list run-camera_list.txt                \
      --clean-match-files-prefix ba/run                \
      --num-lines-per-position 5000                    \
      --num-lines-per-orientation 1000                 \
      --heights-from-dem ref_dem.tif                   \
      --heights-from-dem-uncertainty 10.0              \
      --anchor-dem ref_dem.tif                         \
      --anchor-dem-uncertainty 25.0                    \
      --num-anchor-points 5000                         \
      --num-anchor-points-extra-lines 500              \
      --camera-position-uncertainty "100 100"          \
      --max-initial-reprojection-error 25              \
      --mapproj-dem ref_dem.tif                        \
      --robust-threshold 2                             \
      --min-triangulation-angle 1e-10                  \
      --forced-triangulation-distance 100000           \
      --num-iterations 100 --num-passes 2              \
      -o out/run

### Flag differences vs bundle_adjust
- **NO `--datum`**: `jitter_solve` reads datum directly from CSM cameras.
- **NO `--remove-outliers-params`**: The outlier lever in `jitter_solve` is
  `--max-initial-reprojection-error` (default 20, relax to 25-50 when initial poses differ).
- **Quote `--camera-position-uncertainty "100 100"`**: Two numbers must be quoted as one
  string argument, or space-separated tokens get misparsed as positional images.

## Two-pass jitter refinement workflow

When refining a multi-camera linescan network (such as OHRC co-registering to LRO NAC):
1. **Pass 1**: Run `jitter_solve` starting from bundle-adjusted cameras with coarse orientation
   (`--num-lines-per-orientation 1000`) and soft position clamp (`--camera-position-uncertainty "100 100"`).
2. **Pass 2**: Warm-start from Pass 1 cameras, tighten position clamp
   (`--camera-position-uncertainty "50 50"` to clamp position and let orientation do fine work),
   and **reuse original clean match files from the bundle adjustment** (`--clean-match-files-prefix ba/run`),
   because `jitter_solve` does not write out clean match files.

## Ground evaluation: ALWAYS pass `--mapproj-dem`

- **The `--mapproj-dem` requirement**: You MUST pass `--mapproj-dem <ref_dem.tif>` so
  `jitter_solve` generates `run-mapproj_match_offset_stats.txt` (the ground registration
  metric in meters). If omitted, this critical ground verification stat is absent.
- **Judge jitter on the ground, NEVER by reprojection error or anchor movement**:
  - Reprojection error can barely budge (e.g. 1.24 px to 1.10 px) while ground craterlets
    visibly sharpen from doubled features into crisp single rims.
  - Camera centers moving up to ~1 km along the line of sight is a degenerate direction that
    has negligible ground effect from a 100 km orbit.
  - Anchor movement in `anchor_points.csv` measures straying from initial ray intersections,
    not ground error; anchors exist to stop pose oscillation.
  - Evaluate success by:
    1. Ground offset stats (`run-mapproj_match_offset_stats.txt`, e.g. medians dropping from 0.82 to 0.61 px).
    2. Visual inspection of max-lit mosaics or hillshade overlays ([[visual-inspection]]).
    3. Mapprojected correlation dh/dv against the reference DEM ([[dem-comparison]]).

## Anchor points vs triangulated points balance

- **Set anchor STRENGTH via `--anchor-dem-uncertainty` (meters), NOT `--anchor-weight`**:
  the weight form is DEPRECATED (weight `w` == uncertainty `1/w`); the docs/`--help` both
  steer to the uncertainty. LARGER uncertainty = LOOSER = lighter anchors. To deploy MANY
  anchors that stabilize pose WITHOUT freezing the solve to the DEM, use a large uncertainty
  (~50-150 m), not a tight one.
- **`--num-anchor-points` is PER IMAGE** (not a global total): e.g. `--num-anchor-points 5000`
  makes ~5000 per image, distributed; `--num-anchor-points-per-tile` and
  `--num-anchor-points-extra-lines` are the other count levers. Over-provision and let
  `--max-anchor-points-to-tri-points-ratio` prune if needed.
- **Anchors must NOT dominate triangulated tie points**: Anchors provide stability across
  shadowed or low-match regions. They are not an accuracy lever.
- Check printed counts at startup: Ensure triangulated match points outnumber anchor points
  substantially (aim for 10:1 to 20:1 ratio, e.g. 216k tri points vs 12k anchors).
- If anchors dominate, the solver freezes the cameras to the DEM and real corrections fail.

### Explicit balance controls (NEW, build 2026/10 - verify availability first)

As of ASP build 2026/10, `jitter_solve` has three flags (all default -1 = off) to bound and
balance the network for large runs (:numref:`jitter_anchor_points`):
- `--max-num-tri-points <N>` - cap triangulated points (random subset).
- `--max-gcp-to-tri-points-ratio <r>` - cap GCP at r x the (possibly reduced) tri-point count.
- `--max-anchor-points-to-tri-points-ratio <r>` - cap anchor points at r x the tri-point count.
`bundle_adjust` has `--max-gcp-to-tri-points-ratio` only (:numref:`gcp_vs_tri`). Setting a
ratio = 1.0 is a CEILING (that control class <= tri count), not a target - the quality aim is
still tri points OUTNUMBERING anchors (~10:1); drop the anchor ratio to ~0.1-0.25 if anchors
crowd the tri points. Use these to keep a dense dem2gcp GCP set (and anchors) from hogging the
network instead of hand-tuning `--max-pairwise-matches` / `--max-num-gcp`.
**AVAILABILITY: these are build-2026/10+ only. VERIFY before use** (`jitter_solve --help |
grep max-gcp-to-tri`). Older builds LACK them; if absent, fall back to bounding counts with
`--max-num-gcp` (at dem2gcp) + `--max-pairwise-matches`.

## Large-scale linescan blocks (Mons Mouton / SfS lessons)

When running `jitter_solve` on massive datasets (e.g. 3,650 LRO NAC images at Mons Mouton,
58k x 42k DEM at 1 m/pixel; `sfs_usage.rst` :numref:`sfs_jitter`):
1. **Jacobian overflow protection**: Use `--max-pairwise-matches 75` to keep the Ceres
   Jacobian memory within system limits.
2. **Padded anchor DEM**: Use an expanded `--anchor-dem` (padded 10 to 40 km beyond the target
   DEM) together with `--num-anchor-points-extra-lines 40000` to anchor the linescan trajectory
   well before and after data acquisition, preventing wild edge departures.
3. **Anchor points per tile**: `--num-anchor-points-per-tile 1` to `4` with light `--anchor-weight 0.05`
   keeps poses stable across dark/unmatched polar terrain.
4. **Mega-GCP harvesting**: When consolidating converged geometry across multiple runs, run
   `jitter_solve --save-cnet-as-gcp` to dump triangulated tie points as a unified mega GCP file,
   then optimize with `--camera-position-uncertainty` and zero matches (`--clean-match-files-prefix empty/`).
5. **Uncertainty hierarchy**: Maintain `GCP sigma < heights-from-dem-uncertainty < anchor-dem-uncertainty`.

## Borderline / edge images: the padded anchor DEM is MANDATORY once poses get finer

The padded `--anchor-dem` above is not only a massive-dataset concern. A SINGLE borderline
image (footprint pokes past the domain edge) will smear the instant you make the orientation
poses finer, even in an otherwise-clean solve. Mechanism (BCU2314, 2026-10-03): refining
`--num-lines-per-orientation` from 4000 to 2000 gave an under-observed span of one west-edge
frame its own orientation knot. That span images terrain just OUTSIDE the domain (and in
shadow, so no tie points). Anchors are spread over the whole image and even beyond its first/
last line (:numref:`jitter_anchor_points`: "uniformly distributed over each image ... can even
go beyond the first and last image line ... where there may be no interest point matches"), but
their ground coverage is BOUNDED BY THE ANCHOR DEM EXTENT. With the anchor DEM ending at the
domain, that knot got ZERO anchors, floated, and its projected footprint EXPLODED ~7x into a
streak (mapprojected-footprint area and valid-fraction jump: 0% inside the bad window at 4000
lines vs 98.5% at 2000). The camera CENTER barely moved (<1 m), so per-camera stats (camera
offset, mapproj offset, reproj error) are BLIND; only the max-lit mosaic (a localized smear /
"explosion") and the mapprojected-footprint bbox reveal it. See [[sfs-post-bundle-eval]] for the
per-chunk-partial + per-image localization recipe.

RULE: any time you go finer on orientation knots AND the set has edge/borderline frames, the
`--anchor-dem` MUST extend well beyond the domain (a genuine source DEM, e.g. regrid the Barker
LDEM, NOT a fabricated pad). +4 km (4000 px at 1 m) each side fixed it here; 10 to 40 km for
long trajectories. Keep `--heights-from-dem` / `--mapproj-dem` at the domain DEM; only the
ANCHOR DEM grows. More anchors or tighter `--anchor-dem-uncertainty` INSIDE the domain do NOT
help this (the span has no in-domain anchors at any density/weight). Immediate mosaic fix if you
cannot re-solve: drop the one frame and rebuild the max-lit (its real footprint is a sliver
neighbors cover); the real fix is the padded anchor DEM.

## GCP: optional, and a double-edged lever

`dem2gcp` turns an ours-vs-reference hillshade disparity into GCP (gcp-sigma =
ground accuracy, e.g. 30 m for Copernicus GLO-30). GCP can pull the solution to
the reference, but they are a STRONG lever: they can force huge (especially
vertical/radial) camera moves that `--camera-position-uncertainty` cannot hold.
The clean approach: run jitter BOTH with GCP and without (heights-from-dem +
anchors + ties only), and compare `run-final_residuals_stats.txt` /
`pointmap.csv`: keep whichever is better behaved.

### Guard noisy GCP with `--gcp-robust-threshold`

When the ours-vs-reference disparity that dem2gcp is built from is noisy or patchy:
- DEFINITION (jitter_solve.rst / bundle_adjust.rst): the GCP residual is
  `|optimized GCP position - measured GCP position| / gcp_sigma` (the sigma from
  the GCP file). The threshold is in those NORMALIZED (sigma) units.
- CHOOSE it comparable to the largest normalized residual expected from REASONABLE
  GCP. Run once, inspect `run-final_residuals_stats.txt`, and set the threshold near the knee
  so crazy movers are down-weighted while good control still pulls.
- See also `--max-gcp-reproj-err` (a hard cap on GCP reprojection error, e.g. 30).

## Hard lessons (watch for these)

- **Jitter can EXPLODE**: Loose rope (large `--camera-position-uncertainty`, loose
  height/anchor, strong GCP) makes the cameras move kilometers and BENDS the strip
  into a "banana". Symptom: `run-camera_offsets.txt` shows km-scale moves; coverage collapses.
- **Two-view tension**: Jitter lowers triangulation error but can DEGRADE terrain
  agreement by bending the surface AWAY from true terrain. ALWAYS judge by dh/dv or
  geodiff vs the reference, not triangulation error alone.
- **Fitting GCP is not success**: A plausible GCP residual (~2 px) means nothing if
  the resulting DEM matches the reference worse.

## Three kinds of ground control (know which is doing what)

`jitter_solve` constrains the solution with up to three distinct kinds of control.
Plot all three ON the reference DEM and print counts:
1. **HEIGHTS-FROM-DEM tie points**: In `run-final_residuals_pointmap.csv` tagged `# from DEM`.
   Triangulated tie points whose HEIGHT is pulled to the reference DEM (vertical control).
   Stiffness = `--heights-from-dem-uncertainty`.
2. **GCP**: In the SAME pointmap file tagged `# GCP`. Carries full XYZ ground position
   (horizontal + vertical control) with `--gcp-sigma` and `--gcp-robust-threshold`. Repairs
   horizontal (dh/dv) warps.
3. **ANCHOR POINTS**: In `run-final_residuals_anchor_points.csv`. Ties image pixels to where
   their rays meet the anchor DEM. Stabilizes poses across empty or shadowed areas.

## Inspect (report files)

- `run-camera_offsets.txt`: How far cameras moved (km = trouble; check camera elevation is physical).
- `run-mapproj_match_offset_stats.txt`: Real ground misregistration metrics (generated when `--mapproj-dem` is passed).
- `run-initial_residuals_stats.txt` and `run-final_residuals_stats.txt`: Reprojection error breakdown.
- `pointmap.csv` (split by `# from DEM` vs `# GCP`) and `anchor_points.csv`: Inspect control distribution.

## Doc pointer

ASP manual: `docs/tools/jitter_solve.rst`, `docs/sfs_usage.rst` (:numref:`sfs_jitter`).
Alignment context: `docs/bundle_adjustment.rst`.
Related: [[bundle-adjust]], [[coregister-linescan]], [[solve-intrinsics]], [[dem-sanity-check]], [[lens-distortion]].
