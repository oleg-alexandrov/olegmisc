---
name: coregister-linescan
description: >-
  Co-register a target linescan camera set (e.g. Chandrayaan-2 OHRC) to a fixed
  well-registered reference set (e.g. LRO NAC): pairwise mapproject, dense matches,
  reference-fixed bundle_adjust, then jitter_solve if a tilt remains. Load when bringing
  one camera set into agreement with another, or deciding bundle-vs-jitter.
---

Goal: a reference set is already internally TIGHT (e.g. LRO NAC, bundle-adjusted to
itself); bring a target linescan set (e.g. OHRC) into agreement while holding the
reference FIXED. Do it PAIRWISE first (each target image vs its best single reference),
confirm it works, then consider a joint solve. This is lengthy, batch work.

## Per-pair recipe

1. **Pick the reference twin by SUN AZIMUTH.** At grazing polar sun, matching only works
   between similar-azimuth images (matched shadows) - see [[sun-illumination]]. For each
   target image find the reference with the closest sun azimuth AND real ground overlap.
   CHECK COVERAGE: there is no guarantee a given pair covers the same ground - crop both
   to the box and confirm valid overlap before spending a stereo job. Watch azimuth bands
   where the reference is sparse (fewer/looser twins there).
2. **Mapproject BOTH at the GSD sweet spot on ONE DEM.** Use ~0.5 m/pixel (between OHRC
   0.25 and NAC 1.0) in the local polar-stereographic projection, both onto the SAME
   reference DEM (the one you will also use for --heights-from-dem). Use each image's OWN
   adjusted json (never a mismatched one; see [[csm-models]], [[lro-nac]]).
3. **Get matches - dense-from-disparity (IP usually FAILS here).** Cross-instrument
   radiometry (e.g. OHRC byte vs NAC reflectance) defeats SIFT/SURF (0 matches). The
   census correlator locks fine, so: `parallel_stereo <L_map> <R_map> <L.json> <R.json>
   out/run <DEM> -t csm --alignment-method none --stereo-algorithm asp_mgm --cost-mode 3
   --corr-seed-mode 0 --corr-search <bounded> --num-matches-from-disparity 40000`. This
   writes DENSE RAW-image matches `out/run-disp-<L>__<R>.match`. Collect all such disp
   match files for a batch into ONE dir and feed bundle via `--match-files-prefix`.
   (bundle_adjustment.rst; [[bundle-adjust]], [[match-plot]].)
4. **bundle_adjust, reference FIXED, target free:**
   `bundle_adjust L.cub R.cub L.json R.json -t csm --match-files-prefix <disp prefix>
    --fixed-image-list <ref cub list> --heights-from-dem <ref DEM>
    --heights-from-dem-uncertainty 20 --min-matches 0 -o ba/run` (+ multi via
   --image-list/--camera-list). The reference cub is only a pixel/dims container paired
   with its existing adjusted json.
5. **INSPECT (never skip):** initial vs final `run-*residuals_stats.txt` (target should
   drop from ~10 px to sub-pixel; reference ~unchanged), `run-camera_offsets.txt`,
   `run-convergence_angles.txt`, `run-triangulation_offsets.txt` (ground move should
   match the known misreg, e.g. ~5-15 m). Confirm the REFERENCE json is unchanged and the
   TARGET json moved.
6. **VERIFY by re-correlation:** mapproject the target with the NEW json onto the ref DEM,
   correlator vs the reference; plot dh/dv BEFORE vs AFTER ([[dem-comparison]]). Flat/small
   after = fixed.

## Camera-position-uncertainty: do NOT over-constrain (important)

A large camera-center wander (e.g. 1551 m, even up to ~2 km) is the single-image
along-track/radial POSITION DEGENERACY and is NOT alarming if the GROUND result is good
(sub-px residual, sensible triangulation offset). Prefer a LOOSE
`--camera-position-uncertainty` (~500 m) or none: over-constraining can PREVENT undoing a
pre-existing tilt (the target may already be tilted because a prior bundle left its
position free). Ground agreement wins; don't sweat the raw camera move. Ideally less
movement, but never trade away the registration to shrink it.

## jitter_solve on top - ONLY if a residual TILT/wave remains after bundle

If, after bundle, the re-correlation still shows a smooth residual tilt or WAVY "funny
business" that pose cannot remove, run jitter_solve on the SAME pair with the LATEST
bundle cameras (jitter_solve takes `--image-list`/`--camera-list` from the bundle out
dir). Needs the same dense matches. Starting params (from mons_mouton / PNCB notes; tune):
`--num-lines-per-position 5000 --num-lines-per-orientation 1000
 --heights-from-dem <ref DEM> --heights-from-dem-uncertainty 10..20
 --anchor-dem <ref DEM> --anchor-weight 0.05 --num-anchor-points-per-tile 1
 --num-anchor-points-extra-lines <N> --camera-position-uncertainty 250`.
BALANCE: anchor points must NOT dominate the tri (match) points, or the solution over-
weights the DEM and the cameras barely move. Both bundle and jitter PRINT the number of
tri points and anchor points - read them, and afterward inspect how much anchor points
moved. Study `~/projects/sfs_mons_mouton/*` and `~/projects/pncb_registration.sh` for
worked settings; `dem2gcp.rst` for image-list/camera-list plumbing; [[jitter-solve]] for
the knot-count logic.

## Scaling / priority ladder

Pairwise first (each target image -> its closest-illumination reference), a few dozen
mapproject+num-matches-from-disparity jobs, matches pooled in one dir. Bundle is the
DEAREST HOPE. Jitter is next, only where the tilt survives bundle. Lens distortion LAST
(narrow FOV, uncalibrated). Eventually a JOINT bundle/jitter of all, but only after
pairwise proves out.
