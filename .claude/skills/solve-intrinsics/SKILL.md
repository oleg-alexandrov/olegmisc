---
name: solve-intrinsics
description: Float camera INTRINSICS (focal length, optical center, lens distortion) with bundle_adjust --solve-intrinsics to remove unmodeled distortion, for pinhole/Tsai or CSM (frame + linescan) cameras. Load when a DEM shows an odd nonlinear triangulation-error pattern that extrinsics alone cannot fix, when asked to "solve for distortion / intrinsics / lens", or when setting up a bundle_adjust intrinsics run. Carries the exact flag syntax, the seed-nonzero-coeffs gotcha, the CSM distortion-type ids, and the doc pointers.
---

# Solving camera intrinsics (distortion, focal, optical center) with bundle_adjust

`bundle_adjust --solve-intrinsics` floats the intrinsic parameters so the cameras
reproduce the imaged geometry. Use it when the triangulation error shows an odd
NONLINEAR pattern (fat at the edges, a twist, a bowl) that adjusting only the
extrinsics (position/orientation) cannot remove. Works for pinhole/Tsai and for
CSM frame and linescan cameras (`-t csm`).

## Prerequisites (do these FIRST)

- Cameras already bundle-adjusted with intrinsics FIXED, and ALIGNED to a
  reference DEM (`pc_align`, then apply the transform to the cameras;
  :numref:`ba_pc_align`). Intrinsics solving on unaligned cameras drifts.
- Use that reference DEM as a constraint: `--heights-from-dem` (very recommended,
  :numref:`heights_from_dem`).
- Dense, well-distributed interest points (:numref:`dense_ip`); poorly-distributed
  IP leave large tri errors where they are missing. `--num-matches-from-disparity`
  from a stereo run gives dense, camera-independent matches.

## The command (CSM linescan example, the shape that works)

    bundle_adjust -t csm --inline-adjustments --solve-intrinsics \
      --intrinsics-to-float "optical_center focal_length other_intrinsics" \
      --intrinsics-to-share "optical_center focal_length other_intrinsics" \
      --heights-from-dem ref.tif --heights-from-dem-uncertainty 200 \
      --camera-position-uncertainty 500,500 \
      --match-files-prefix dense/run --max-pairwise-matches 20000 \
      img1.tif img2.tif cam1.json cam2.json -o out/run

- `other_intrinsics` is the bundle_adjust name for the DISTORTION coefficients.
- `--inline-adjustments` writes NEW camera files (for CSM these are new
  `*.adjusted_state.json`); without it you get `.adjust` files instead.
- `--intrinsics-to-share`: if the frames are the SAME physical camera (one
  sensor), share all. For several sensors, group them (:numref:`kaguya_ba`).
- `--camera-position-weight 0` is the doc's simpler alternative to
  `--camera-position-uncertainty`; the uncertainty form (H,V meters) is finer.
- No GCP needed if the cameras were already pc_aligned - heights-from-dem +
  matches carry the constraint.

## CRITICAL gotcha: seed zero coeffs to a small non-zero value

bundle_adjust optimizes ONLY the NON-ZERO intrinsics, and the optimization step
size is PROPORTIONAL to each value. A distortion coefficient sitting at 0 (or
1e-16) will NOT move. Before solving, SEED each coefficient you want to float to a
small non-zero value suggestive of its final scale (e.g. ~1e-7). If the solve
fails or diverges, try flipping the sign or making the seed smaller. (Edit the
camera json's `m_opticalDistCoeffs`.)

## CSM distortion types (usgscsm `Distortion.h` enum, `m_distortionType`)

    RADIAL=0, TRANSVERSE=1, KAGUYALISM=2, DAWNFC=3, LROLROCNAC=4, CAHVOR=5,
    LUNARORBITER=6, RADTAN=7, KPLOSHADOWCAM=8, CASSIS=9

RADTAN (7) is radial + tangential = the Brown-Conrady-style model, 5 coeffs
`[k1,k2,p1,p2,k3]`. The ASP doc notes that for PINHOLE cameras the *Tsai* model is
faster in bundle_adjust and Brown-Conrady is "not advised"; but for a CSM linescan
RADTAN is the built-in distortion, so use it there. TRANSVERSE overfits easily -
avoid it as a lens model for a physically-radial/scan distortion.

## Inspect the result (do not trust the number alone)

- `pointmap.csv` before vs after (`--csv-format 1:lon,2:lat,4:height_above_datum`);
  plot side by side. The tri-error should drop where IP are dense.
- Re-run stereo with the new cameras, point2dem `--errorimage`, and look at the
  tri-error image: the nonlinear pattern should shrink. If a symmetric edge-fat
  pattern remains it is lens distortion; a one-sided twist is more likely a
  linescan pose issue for jitter_solve (see [[jitter-solve]]).
- `cam_test` (:numref:`cam_test`) checks the distortion model inverts correctly.

## Doc pointers

`docs/bundle_adjustment.rst` (NOT `stereo`/`bundle_adjust` tool ref - a separate
doc): :numref:`intrinsics_no_constraints` (first attempt, the seed-nonzero note),
:numref:`kaguya_ba` (grouped intrinsics, `--intrinsics-to-float`/`-share` syntax),
:numref:`heights_from_dem`, :numref:`dense_ip`, :numref:`ba_pc_align`. Tool option
reference: :numref:`bundle_adjust`.
