---
name: dem-sanity-check
description: Visually check a produced DEM/ortho for GLOBAL geometric errors against a reference - left-right mirror (flip), up-down flip, 180 rotation, a bulk horizontal shift, or vertical inversion - when you are not fully sure the result is oriented correctly. Carries the hard lesson that dz/geodiff/NMAD and bundle-adjust residual are BLIND to a horizontal mirror, and the crater-triplet hillshade test that actually catches it. Load whenever asked to evaluate DEM orientation/correctness, "is it flipped/mirrored/shifted", or before trusting a DEM that was built from a hand-assembled camera (sat_sim/cam_gen, custom GCPs).
---

# DEM sanity check: catching a global flip / mirror / shift

A produced DEM can be INTERNALLY perfect (tight triangulation error, sub-pixel bundle
residual, clean stereo) yet be GLOBALLY WRONG vs reality: left-right mirrored, up-down
flipped, 180-rotated, bulk-shifted, or vertically inverted. This happens when the camera
was hand-assembled (sat_sim / cam_gen) or GCPs were cooked from a vendor backplane and a
cross-track / line convention is reversed. Burned 2026-09-04 on Tianwen-1 HiRIC: the whole
DEM was L-R MIRRORED (vendor GEO "Column" ran opposite to the stored image sample order, so
GCP `col = Sc/2` was wrong-handed; the fix was `col = (W-1) - Sc/2`). It shipped through
FOUR "successful" runs before a human eyeballed craters.

## WHAT DOES NOT CATCH IT (do not rely on these for orientation)

- **dz / geodiff / NMAD is BLIND to a horizontal mirror (or shift on flat-ish terrain).**
  NMAD compares elevation VALUES pixel-by-pixel. On gently-rolling terrain a mirrored (or
  slightly shifted) DEM has nearly the same elevation histogram, so dz/NMAD stays small
  (we saw 9-17 m "agreement" on a fully mirrored DEM). A good dz number is NECESSARY, NOT
  SUFFICIENT - it says nothing about horizontal orientation.
- **Bundle-adjust reprojection residual is blind to this class.** BA only enforces
  camera<->GCP and camera<->camera CONSISTENCY; it never checks that the image CONTENT at a
  pixel actually shows the GCP's ground point. A mirrored GCP-column convention makes BA fit
  a SELF-CONSISTENTLY mirrored camera -> low residual on GCPs AND tie-points, tiny tri-err,
  mirrored DEM. (BA cannot REFLECT, so a low residual only rules out sat_sim building a
  det=-1 camera; it does NOT rule out a mirrored data-correspondence.)
- **Global NCC / correlation between two crops that are NOT pixel-co-registered** (different
  extent, zoom, or grid) is inconclusive - it can't align features, so the number is ~0 for
  both orientations. Co-register first (same -projwin, same grid) or don't bother.
- **A red/green hillshade overlay of a THIN diagonal strip is mushy** - it reads mostly
  yellow and you cannot call a flip from it. Needs a distinctive asymmetric feature IN the
  overlap, at a readable zoom.

## WHAT WORKS (the procedure - do this)

Produce TWO deliverables so a human AND you can eyeball, then run the flip-test:

1. **Full-res hillshade, side by side, ours | theirs.** `gdaldem hillshade -multidirectional
   -compute_edges` on each DEM (no color - color distracts from geometry), put them side by
   side at full/near-full res. Also do the reference alone so the shape is legible.
2. **Zoomed same-window crop, side by side.** Pick a CENTER area with a DISTINCTIVE,
   ASYMMETRIC feature group - the winner is a crater TRIPLET forming an obtuse/scalene
   triangle (or a double-crater + a lone crater). Crop the SAME ground window from both
   (`gdal_translate -projwin ULX ULY LRX LRY` in the shared CRS), hillshade, view side by
   side. Symmetric single craters are useless (a mirror looks identical); you need chirality.
3. **THE FLIP TEST (decisive).** Flip ONE crop left-right (and, separately, up-down) and
   compare to the other. If the feature triplet only COINCIDES when one is flipped -> that
   axis is mirrored. If it coincides WITHOUT flipping -> orientation is correct on that axis.
   By eye: does the obtuse-triangle triplet keep its handedness or reverse? Quantify only if
   the crops are on the SAME grid (then NCC of flipped-vs-not is meaningful).
4. **dh (horizontal disparity) is the rigorous confirmer.** A flip is not a constant offset -
   it GROWS across track: correlate the two hillshades (dem-comparison skill: parallel_stereo
   --correlator-mode -> disparitydebug) and look at dh. A mirror shows an ANTI-SYMMETRIC dh
   (the y=x vs y=-x cross - +max at one edge, 0 in the middle, -max at the other). A pure
   shift shows CONSTANT dh; correct orientation shows ~0 dh. This is the test dz cannot do.
   Clip to the overlap first so the correlator has content.

## The mechanism to check when a flip is found (hand-built cameras)

The cross-track (sample) and along-track (line) conventions. For a sat_sim/cam_gen linescan
camera fed GCPs from a vendor GEO backplane: does the stored image's SAMPLE axis run the same
direction as the vendor's "Column", and the LINE axis the same as "Row"? Read the GEO grid:
e.g. HiRIC orbit26 CCD1, sample 307 -> lon 110.456, sample 5888 -> lon 110.387, so increasing
sample = decreasing lon (west). If the stored .2B is written in the opposite sample order,
`col = Sc/2` is wrong-handed and everything downstream is a self-consistent mirror. Fixes,
either is fine: (a) GCP `col = (Wfull-1) - Sc` (then /sub), or (b) flip the input images L-R
and keep `col = Sc`. Re-run BA -> stereo and re-check with the procedure above. Same reasoning
for an up-down flip (Row/line reversed) or a 180 rotation (both reversed).

## Reporting

State the HYPOTHESIS first ("if correct, the obtuse-triangle triplet keeps its handedness
between ours and theirs"), then confirm by eye + dh. Say explicitly whether YOU independently
confirmed it or are trusting a human's call - do not launder "the user said so" as your own
verification. Keep the two side-by-sides (full-res + zoom) as the artifacts; a human will want
to make their own call. See also [[visual-inspection]] (warp-to-common-grid, hillshade, overlay
mechanics) and [[dem-comparison]] (dh/dv/dz, the correlator-mode recipe).
