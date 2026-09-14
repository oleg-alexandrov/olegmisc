---
name: lens-distortion
description: CSM/USGSCSM lens distortion models (RADTAN, TRANSVERSE, etc.) - the enum, the exact coefficient layout and index of each term, how to change the model on a camera state file, and the seeding gotchas for floating distortion in bundle_adjust or jitter_solve. Load before setting m_distortionType / m_opticalDistCoeffs, seeding a distortion for --solve-intrinsics, or when a distortion solve explodes at the start. Referenced by [[solve-intrinsics]] and [[jitter-solve]].
---

# CSM / USGSCSM lens distortion models

A USGSCSM camera state (`USGS_ASTRO_LINE_SCANNER_SENSOR_MODEL` etc.) carries a
distortion by two JSON fields: `m_distortionType` (an int enum) and
`m_opticalDistCoeffs` (the coefficient vector). To change or seed a model, edit
those two fields in the JSON body (skip the model-name header line first). ASP
`bundle_adjust --solve-intrinsics --intrinsics-to-float other_intrinsics` floats
ALL of `m_opticalDistCoeffs`; `focal_length` and `optical_center` are separate.

## The enum (usgscsm `include/usgscsm/Distortion.h`)

    RADIAL=0, TRANSVERSE=1, KAGUYALISM=2, DAWNFC=3, LROLROCNAC=4,
    CAHVOR=5, LUNARORBITER=6, RADTAN=7, KPLOSHADOWCAM=8, CASSIS=9

## RADTAN (type 7) - 5 coeffs

`m_opticalDistCoeffs = [k1, k2, p1, p2, k3]` (3 radial + 2 tangential; matches
OpenCV `cv::projectPoints`). Definition: `computeRadTanDistortion` in
`usgscsm/src/Distortion.cpp`. Identity = all zero (but see the float-from-0 bug).

## TRANSVERSE (type 1) - 20 coeffs, a full 2D focal-plane polynomial

The "glorified jack of all trades": (ux,uy) -> (dx,dy) via two cubic 2D
polynomials. Definition: `computeTransverseDistortion`, `Distortion.cpp:88`.
Term basis (10 terms), in order:

    f = [1, ux, uy, ux^2, ux*uy, uy^2, ux^3, ux^2*uy, ux*uy^2, uy^3]

`m_opticalDistCoeffs` has 20 entries: the FIRST 10 are the X polynomial, the
NEXT 10 are the Y polynomial (yPointer = size/2 = 10):

    dx = sum_{i=0..9}  f[i] * coeffs[i]        # X block, indices 0..9
    dy = sum_{i=0..9}  f[i] * coeffs[10 + i]   # Y block, indices 10..19

IDENTITY (dx=ux, dy=uy) => the two "1"s go at:
  - `coeffs[1]  = 1.0`   (the ux term of dx)
  - `coeffs[12] = 1.0`   (the uy term of dy; = index 10 + 2, since f[2]=uy)
All other 18 coeffs are the seed (see gotchas - NOT 1e-7, NOT 0).

## SEEDING GOTCHAS (learned on KH-7, 2026-09 - watch for these)

1. **bundle_adjust cannot float a coeff that starts at EXACTLY 0** (long-standing
   ASP bug: it normalizes each intrinsic by its initial value, so 0 -> divide by
   zero -> that coeff is frozen). Every floatable coeff MUST start non-zero.
2. **Focal-plane coords are mm-scale and LARGE** (ux up to ~100 mm on KH-7). A
   uniform seed like 1e-7 on the CUBIC terms is far too big: `ux^3 ~ 1e6`, so
   `1e-7 * 1e6 = 0.1 mm ~ 14 px` of spurious initial distortion. ATTENUATE the
   high-order terms - seed the non-identity coeffs TINY (e.g. `1e-10`) so the
   initial model is ~identity but still non-zero (floatable). (If you want, seed
   only the cubics tiny and the low-order terms a bit larger; a uniform 1e-10 is
   the simplest that works.)
3. **VALIDATE the seed before trusting the run.** After seeding, run dem2gcp and
   read `run-initial_residuals_pointmap.csv` (col 4 = mean residual, px). The
   initial median MUST match a known-good identity run (e.g. a RADTAN-identity
   run on the same cameras/GCP). KH-7 example: bad seed gave median 18.8 px,
   good identity seed gave 2.06 px (matching the RADTAN run). A large initial
   residual = wrong indices OR too-large high-order seed. The iter-0 Ceres cost
   should also match (KH-7: ~2.77e4 either way).
4. **All 20 coeffs float**, including the identity "1"s (they float too = the
   linear/scale part). That is the model's full flexibility; more than needed,
   but a sign of desperation when RADTAN cannot capture the residual.

## Change the model on a state file (python)

    raw=open(cam).read(); i=raw.find('{'); j=json.loads(raw[i:])   # skip header
    j['m_distortionType']=1                                        # TRANSVERSE
    c=[1e-10]*20; c[1]=1.0; c[12]=1.0; j['m_opticalDistCoeffs']=c  # identity seed
    open(out,'w').write(raw[:i]+json.dumps(j,indent=2)+'\n')

Write to a COPY (never edit the source cameras in place). RADTAN uses 5 coeffs,
TRANSVERSE 20 - set the right length or the CSM throws a size error.

## Which model - and EVAL BEFORE STEREO (KH-7, 2026-09)

- **TRANSVERSE (20 coeffs) OVERFITS.** Its full cubic 2D polynomial has far more
  freedom than the residual needs, so it fits the GCP/heights control while BENDING
  the surface: KH-7 transverse solve improved gross placement (dh/dv 31 m) but
  crashed the high-pass shape from 0.84 (RADTAN) to 0.33, with a grainy DEM. Prefer
  RADTAN (5 coeffs) unless you have evidence the residual is a high-order 2D warp.
  Reach for transverse only as a desperation probe, and expect overfit.
- **Catch overfit from the SOLVED COEFFS, before running stereo.** An unphysical
  coefficient is the tell: KH-7 transverse drove the identity Y-scale term (index 12)
  from 1.0 to -0.61 (a cross-track scale flip) - obvious overfit, visible with zero
  stereo cost. WORKFLOW: run bundle_adjust, then (1) print the solved
  `m_opticalDistCoeffs` and sanity-check magnitudes/signs (identity terms should stay
  near 1; a sign flip or a huge jump = overfit), (2) read `run-final_residuals_pointmap.csv`;
  only if the coeffs look sane spend the time on parallel_stereo + point2dem. (A tight
  final pointmap alone is NOT success - overfit fits the control while wrecking the DEM.)
- **Tiny-seed STALL is real for some terms.** With a 1e-10 seed most coeffs float
  fine (grow several orders), but the pure `ux^3` terms can stall near the seed
  (KH-7: index 16 stayed at 1.02e-10). If a specific high-order term matters and
  won't move, seed it a bit larger; otherwise it likely has ~0 true value.

## Focal vs distortion DEGENERACY (do not float both without reason)

`focal_length` and the distortion's linear/scale terms both set the cross-track
scale, so floating focal on TOP of distortion is redundant. KH-7: floating
`focal_length other_intrinsics` barely moved the focal (17796->17798) and DROPPED
the shape correlation (hp 0.84 -> 0.71). Float distortion ALONE unless there is a
specific reason to move focal. A sustained ACROSS-track deformation is a
cross-track scale/shape signal (distortion or, if asymmetric, optical center),
NOT along-track jitter.

## Doc pointers

- usgscsm: `include/usgscsm/Distortion.h` (enum + prototypes),
  `src/Distortion.cpp` (`computeTransverseDistortion:88`,
  `transverseDistortionJacobian:35`, `computeRadTanDistortion`, the per-type
  `applyDistortion`/`removeDistortion` branches).
- ASP: `docs/bundle_adjustment.rst` (intrinsics section), `bundle_adjust`
  `--solve-intrinsics` / `--intrinsics-to-float` (focal_length, optical_center,
  other_intrinsics). Related: [[solve-intrinsics]], [[jitter-solve]].
