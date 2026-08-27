---
name: pc-align
description: Aligning a DEM/point cloud to a reference with ASP pc_align - the alignment methods (ICP point-to-plane, similarity, nuth, fgr, feature-based hillshade, correlation-based), which to use when, the try-more-than-one discipline, failure modes + workarounds distilled from chandrayaan2/viking/cassis, applying the transform to a DEM (save-transformed-source-points -> point2dem) or to cameras. Load whenever running pc_align, aligning a produced DEM to a prior/reference DEM, or debugging a bad alignment.
---

# Aligning to a reference DEM with pc_align

DEM alignment is NOT fully robust; the best method depends on the data, so **try more
than one** and INSPECT the result (never trust the reported error alone). See
`docs/tools/pc_align.rst`, `examples/chandrayaan2.rst` (coarse-ref/fine-DEM, the richest
worked example), viking.rst, cassis.rst.

## Always first: put both on ONE common grid, then hillshade + eyeball
Regrid ref and source to the SAME proj/extent/resolution (coarser of the two);
`gdalwarp -r cubicspline` (or `-r average` when downsampling). The reference should extend
BEYOND the source so a shift has room. Hillshade both (`gdaldem hillshade -multidirectional
-compute_edges`) and LOOK - they must look visually similar for any correlation to lock.
(This is the dem-comparison skill's Step 1-2; do it before aligning.)

## The methods (--alignment-method) and WHEN each wins
- **point-to-plane ICP** (default): iterative closest point. Slides to the nearest local
  minimum. WINS when there is real 3D signal - notably a **VERTICAL-dominant** offset (it
  reads the height difference directly). Can slide wrong on a big horizontal shift with weak
  vertical relief. `similarity-point-to-plane` also solves scale.
- **nuth** (`--alignment-method nuth`, Nuth & Kaab): slope/aspect-driven horizontal+vertical;
  good on terrain with slopes.
- **fgr** (`--alignment-method fgr`, Fast Global Registration): global feature matching, no
  initial guess; `--fgr-options` to tune.
- **feature-based / hillshade** (`--initial-transform-from-hillshading <rigid|similarity|
  translation>`, :numref:`pc_hillshade`): matches interest points on the two HILLSHADES, fits
  a transform via RANSAC (`--initial-transform-ransac-params`). Use FIRST when a **large
  HORIZONTAL misalignment** exists.
- **correlation-based** (chandrayaan2 `pc_corr`, the robust route for a big horizontal shift
  on flattish terrain): a BOUNDED dense hillshade correlation "cannot slide globally the way
  ICP can". Recipe:
    parallel_stereo --correlator-mode --stereo-algorithm asp_mgm --subpixel-mode 9 \
      --corr-kernel 9 9 --corr-search -<S> -<S> <S> <S> \
      --ip-per-image 40000 --num-matches-from-disparity 40000 \
      ref_hill.tif src_hill.tif run_corr/run          # size S to the expected shift in px
    pc_align --max-displacement -1 --num-iterations 0 --max-num-reference-points 1000000 \
      --match-file run_corr/run-disp-ref_hill__src_hill.match \
      --initial-transform-from-hillshading rigid --initial-transform-ransac-params 1000 3 \
      --save-transformed-source-points  ref.tif src.tif -o run_align/run
  Inspect `run_corr/run-F.tif` (:numref:`raw_disp`): a smoothly varying, near-constant shift =
  a good lock; blocky/noisy = bad.

**KEY caveat (learned SDB aerial 2026-08-26):** the hillshade correlation / feature-from-
hillshading is **HORIZONTAL-only** - it CANNOT see a pure vertical offset. On a vertical-
dominant offset (e.g. a ~26 m geoid-sized Z-datum error) it returns ~0 vertical and "fixes"
nothing, while **ICP captures it**. On a horizontal along-track shift (chandra's 2.1 km) the
correlation wins and ICP slides. => pick by which axis the offset is on, and when unsure run
BOTH and compare the after-align dz/dh/dv.

## Apply the transform
- To the DEM: `--save-transformed-source-points` writes `<pfx>-trans_source.tif` (a point
  cloud); grid it with `point2dem --t_srs <proj> --tr <res>` (WITHOUT a mismatched `-te` -
  the aligned cloud is shifted; let point2dem set the extent, or use the ref's extent only if
  you know it still covers the cloud). Then geodiff/hillshade-correlate vs the ref to confirm
  the offset shrank (SDB: dz mean 26.3 -> 0.03 m).
- To CAMERAS: `<pfx>-transform.txt` is the 4x4; feed it to `bundle_adjust
  --initial-transform ... --apply-initial-transform-only` (:numref:`ba_pc_align`) to move the
  cameras, then RE-mapproject + REDO stereo. NEVER pc_align between BA stages in the camera
  pipeline (spoils horizontal - a hard CaSSIS lesson); pc_align-for-evaluation is fine.

## Failure modes + fixes (distilled)
- Flat/low-texture hillshades -> IP-seeded correlation fails ("Number of IPs left ... 5");
  use `--corr-seed-mode 0` with a bounded `--corr-search` sized to the shift.
- Search too large -> slow + can lock onto a wrong solution; size it to the visual shift.
- ICP slides to a wrong minimum on big horizontal shift -> do feature/correlation align FIRST,
  then ICP to refine.
- Water / junk pixels in the source corrupt the fit -> mask to LAND / drop high-tri-err pairs
  before aligning.
- Reported error small but result wrong -> ALWAYS regrid + hillshade + eyeball before/after;
  the image tells you (a residual tilt, a leftover junk blob) what the median hides.
Complements dem-comparison (dh/dv/dz mechanics), asp-photogrammetry, visual-inspection.
