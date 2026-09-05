---
name: dem-sanity-check
description: The orientation specialist of the inspection hub ([[visual-inspection]]). Catch GLOBAL geometric errors in a produced DEM/ortho vs a reference - left-right mirror (flip), up-down flip, 180 rotation, bulk horizontal shift, vertical inversion. Carries the crater pose-cluster flip-detector tool, the mapproject-vs-independent-reference test, and the hard lessons that dz/geodiff/NMAD and bundle-adjust residual are BLIND to a horizontal mirror, and that a mirror is fixed by flipping the IMAGE (bundle adjustment cannot reflect, so a GCP-column change alone is a no-op). Load whenever asked whether a DEM/ortho is flipped/mirrored/rotated/shifted or correctly oriented, or right after building a DEM from a hand-assembled camera (sat_sim/cam_gen, custom GCPs).
---

# DEM sanity check: catching a global flip / mirror / shift

A produced DEM can be INTERNALLY perfect (tight triangulation error, sub-pixel bundle
residual, clean stereo) yet be GLOBALLY WRONG vs reality: left-right mirrored, up-down
flipped, 180-rotated, bulk-shifted, or vertically inverted. This happens when the camera
was hand-assembled (sat_sim / cam_gen) or GCPs were cooked from a vendor backplane and a
cross-track / line convention is reversed. Burned 2026-09-04/05 on Tianwen-1 HiRIC: the whole
DEM was L-R MIRRORED (the stored image's sample order ran opposite to the vendor GEO sample
convention). It shipped through FOUR "successful" runs before a human eyeballed craters, and
then cost another day of wrong turns. The REAL fix (verified end-to-end 2026-09-05): **FLIP
THE INPUT IMAGES left-right** before stereo, and set GCP `col = (W-1) - Sc` to match the
flipped content. CRITICAL, non-obvious: changing ONLY the GCP column convention (`col = Sc`
-> `col = (W-1)-Sc`) with the same image is a **NO-OP** — old and column-flipped DEMs came
out byte-identical — because bundle adjustment CANNOT REFLECT (see below). The reflection the
data needs can only be supplied by flipping the IMAGE itself.

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

## FIRST, A HINT (not a proof) - the metadata cross-check (zero compute)

Read the vendor LABEL and cross-check it against the backplane/GCP source. A PDS4 (or similar)
label often carries the STORED image's geographic corners, e.g. HiRIC's
`<Image_Corner_Point_Position>` with Up_Left / Up_Right / Down_Left / Down_Right lon-lat. The
corners suggest which way col and row map to the ground (e.g. col0=west, colMax=east). Compare
that to the GEO/backplane grid you build GCPs from: does its sample increase in the SAME
direction as the image column? A mismatch is a STRONG HINT that a flip exists.
BUT DO NOT treat this as proof (burned on HiRIC, 2026-09-05): the corner reading is
interpretation-dependent (which edge is "the stored image's first column" is exactly the
ambiguous thing), and worse, it points you at the GCP-column knob, which is a NO-OP on its own
(BA cannot reflect). Use the metadata read to RAISE SUSPICION, then PROVE orientation by
measuring features against an independent reference (next section). The metadata is a smell
test, not a verdict.

## THE DECISIVE, NON-CIRCULAR TEST (measure features vs an INDEPENDENT reference)

Orientation is a fact about the IMAGE CONTENT, so prove it by comparing content to a reference
that carries NONE of your pipeline's assumptions. Two layers, cheapest first:

1. **mapproject, NOT stereo (cheap, no DEM build needed) - and do it FIRST, right after ANY
   camera generation** (sat_sim/cam_gen/bundle_adjust/jitter_solve/hand-built CSM), before
   spending hours on stereo. A flip is decided by the image, so you do not need a stereo DEM to
   test it. Mapproject ONE raw image (and, separately, its L-R flip) through its camera onto any
   prior DEM -> two orthos. Why FIRST: a mirror is insidious because the stereo can be INTERNALLY
   consistent (great tri-err) yet globally REFLECTED, and pc_align (rigid, no reflection) can
   NEVER register a mirror - so it masquerades as an un-removable "warp" downstream and burns
   days. One mapproject-and-compare at camera-gen time catches it immediately. Compare each to an INDEPENDENT
   real-texture reference of the same area: the vendor's own ortho/DOM mosaic, or a mapprojected
   CTX/HiRISE image (ASP-sane, well-understood). Whichever ortho (raw or flipped) matches the
   reference tells you directly whether the raw image is mirrored. This is FAST and settles the
   direction before you spend hours on stereo. (HiRIC 2026-09-05: raw ortho vs DOM = FLIPPED
   36/50; flipped ortho vs DOM = SAME 70/39. Decisive.)
   CIRCULARITY TRAP: the reference MUST be independent. Testing your ortho against your own
   camera/DEM/GCP re-encodes the very `col` convention you are questioning -> the test always
   "passes" and proves nothing. Only IMAGE-CONTENT vs an OUTSIDE product is non-circular.

2. **crater pose-cluster tool (automated, robust, beats the eye).** Eyeballing one crater on a
   thin diagonal lane is unreliable and burned us repeatedly ("boy we are having trouble with
   eyes"). Instead detect crater centroids in both hillshades/orthos and Hough-cluster the
   candidate translations between the two point sets; if it is the same scene, ONE translation
   collects many crater pairs. Run it again with one set mirrored (`x -> W-1-x`). Whichever
   overlap is larger - identity or reflection - is the orientation; a clear SAME shows identity
   winning at ~zero shift with roughly 2x the inliers of reflection. Reusable implementation:
   `~/projects/tianwen_hiric/crater_flip.py` (functions `craters(png,param2,minr,maxr)`,
   `pose_cluster_inliers(A,B,tol)`, `flip_test(A,B)`; needs cv2/PIL/scipy, e.g. the `asp_deps`
   env). TUNING that matters: on dense/noisy terrain LOWER `param2` (~12) to detect MANY craters
   (1000+) - too few (<~40 overlap) reads "inconclusive"; validate the tool first on a known pair
   (a tile vs itself = SAME; vs its own mirror = FLIP). Co-register both rasters to the SAME grid
   (warp reference onto the DEM's grid; mask the reference to the DEM's data footprint) so the
   two crater sets are comparable. This statistical test over 1000+ craters is FAR more reliable
   than any single-feature glance.

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
`col = Sc/2` is wrong-handed and everything downstream is a self-consistent mirror.
THE FIX (only one actually works): **FLIP THE INPUT IMAGES L-R** and set GCP
`col = (Wfull-1) - Sc` (then /sub) to match the flipped content. Do NOT expect the GCP-column
change ALONE to help - it is a NO-OP, because bundle adjustment composes only rotations and
translations and CANNOT apply a reflection: fed reflected control with an unflipped image, BA
just re-fits the same non-reflected camera and returns the identical mirrored DEM (verified:
old and column-only DEMs were byte-identical). The reflection must be supplied by the IMAGE. A
low BA residual therefore only rules out sat_sim building a det=-1 camera; it never rules out a
mirrored data-correspondence. Re-run BA -> stereo and re-check with the DECISIVE TEST above (not
just dz). Same reasoning for an up-down flip (flip images top-bottom, `row = (H-1)-L`) or a 180
rotation (both).

## Reporting

State the HYPOTHESIS first ("if correct, the obtuse-triangle triplet keeps its handedness
between ours and theirs"), then confirm by eye + dh. Say explicitly whether YOU independently
confirmed it or are trusting a human's call - do not launder "the user said so" as your own
verification. Keep the two side-by-sides (full-res + zoom) as the artifacts; a human will want
to make their own call. See also [[visual-inspection]] (warp-to-common-grid, hillshade, overlay
mechanics) and [[dem-comparison]] (dh/dv/dz, the correlator-mode recipe).
