---
name: image-gcp-gen
description: >-
  Generate Ground Control Points (GCPs) from an uncalibrated or misregistered camera image against an existing registered orthoimage and DEM with gcp_gen, undo mapprojection to raw image coordinates, quantify planar shift with image_align, and run single-camera bundle_adjust with fixed GCPs to bootstrap cameras into an established reference frame before joint bundle adjustment.
---

# Image GCP Generation and Single-Camera Registration (`image_align` + `gcp_gen`)

This skill documents how to register an uncalibrated or poorly oriented camera image against an existing georeferenced orthoimage and DEM using Ames Stereo Pipeline's `image_align`, `gcp_gen`, and single-camera `bundle_adjust`.

## Why Use This Workflow

When introducing new images to an existing block of co-registered imagery (e.g. adding new linescan swaths to an established DEM mosaic), joint bundle adjustment across all images can fail if initial camera pointings have large errors (tens to hundreds of meters).

In a joint multi-view solve:
- Matches across large initial pointing errors produce huge reprojection residuals.
- Standard outlier filters (`remove-outliers-params`) will either purge those tie points entirely or allow large pointing errors to corrupt established cameras (gauge drift).
- Optimizers can get trapped in false local minima or fail to converge.

The `gcp_gen` workflow decouples the problem:
1. Match in planar ortho space where terrain distortion is minimized.
2. Transfer correspondences back to raw sensor coordinates via `undo_mapproj`.
3. Create 3D Ground Control Points (GCPs) constrained to the reference DEM.
4. Solve each candidate camera **individually** against the fixed reference frame.

Once all cameras are brought into close agreement ($\le 1\text{ to }2\text{ meters}$), joint multi-view bundle adjustment and jitter correction can be executed safely with tight initial conditions.

---

## Canonical Pipeline

### Step 1: Initial Mapprojection
Mapproject the candidate image onto the reference DEM using its initial, unrefined camera model:

```bash
mapproject --tr 0.5 -t csm \
  ref_dem.tif candidate.cub candidate_init.json candidate_map.tif
```

Both `candidate_map.tif` and the reference `ref_ortho.tif` now share the same map projection and spatial resolution.

### Step 2: Measure Planar Shift with `image_align`
Before creating GCPs, run `image_align` in translation mode between the reference ortho and candidate map.

**Argument Order**: The fixed reference ortho MUST be specified first, and the candidate image second (the same convention used in `pc_align`):

```bash
image_align \
  --alignment-transform translation \
  --ip-detect-method 0 \
  --inlier-threshold 50 \
  --ip-per-tile 2500 \
  --ip-per-image 0 \
  ref_ortho.tif \
  candidate_map.tif \
  --output-prefix align_dir/run \
  -o align_dir/run_aligned.tif
```

Inspect the resulting transform file `align_dir/run-transform.txt`:
```text
1 0 <dx>
0 1 <dy>
0 0 1
```

Compute the planar shift magnitude:
```bash
shift_mag=$(awk 'NR==1 {dx=$3} NR==2 {dy=$3} END {print sqrt(dx*dx+dy*dy)}' align_dir/run-transform.txt)
echo "Alignment shift magnitude: ${shift_mag} px"
```

**Decision Gate**:
- If `align_dir/run-transform.txt` does not exist: interest point matching failed (poor contrast, divergent illumination, or zero overlap). Stop and do not proceed to GCP generation.
- If `shift_mag` is below the target threshold (e.g. $< 1\text{ px}$): the camera pointing is already well-registered; GCP generation can be skipped.
- If `shift_mag` is large: proceed to GCP generation and single-camera bundle adjustment.

### Step 3: Synthesize 3D GCPs with `gcp_gen`
Pass the candidate raw image, candidate mapprojection, reference orthoimage, and DEM to `gcp_gen`:

```bash
gcp_gen \
  --ip-detect-method 0 \
  --inlier-threshold 50 \
  --ip-per-tile 2500 \
  --ip-per-image 0 \
  --gcp-sigma 1.0 \
  --camera-image "$(pwd)/candidate.cub" \
  --mapproj-image candidate_map.tif \
  --ortho-image ref_ortho.tif \
  --dem ref_dem.tif \
  --output-prefix align_dir/run \
  -o candidate.gcp
```

#### Key Mechanics:
1. **Match Cache Sharing**: By passing the exact same `--output-prefix align_dir/run` as `image_align`, `gcp_gen` detects `run-*.match` on disk and reuses the cached interest points in under a second instead of rematching from scratch.
2. **Undoing Mapprojection (`undo_mapproj`)**: `gcp_gen` reads the camera model and DEM recorded in `candidate_map.tif` metadata, unprojects matched ortho coordinates back through the camera ray, and determines raw image coordinates $(col_{\text{raw}}, row_{\text{raw}})$.
3. **3D Coordinate Generation**: Looks up the reference longitude, latitude, and DEM elevation for each inlier match, producing a `.gcp` file with 3D ground coordinates and candidate sensor coordinates.

### Step 4: Single-Camera Pose Adjustment
Run `bundle_adjust` on the single candidate image with the ground points held fixed:

```bash
bundle_adjust \
  --camera-image candidate.cub \
  --camera-model candidate_init.json \
  candidate.gcp --fix-gcp-xyz \
  --camera-position-uncertainty 100 100 \
  -o ba_single/run
```

Because `--fix-gcp-xyz` keeps 3D ground coordinates rigid, Ceres adjusts only the candidate camera trajectory (position and orientation) to fit the control points. This completes in seconds and produces `run-candidate.adjusted_state.json`.

### Step 5: Verification by Re-Mapprojection
Mapproject the candidate image again using the newly adjusted camera:

```bash
mapproject --tr 0.5 -t csm \
  ref_dem.tif candidate.cub ba_single/run-candidate.adjusted_state.json \
  candidate_corrected_map.tif
```

Re-run `image_align` against `ref_ortho.tif` or overlay in `stereo_gui` to confirm that residual translation is reduced to sub-pixel level.

---

## Critical Gotchas & Best Practices

### 1. The `--camera-image` Exact Path Match Requirement
`gcp_gen` verifies that `--camera-image` matches the `INPUT_IMAGE_FILE` string embedded in the GeoTIFF metadata header of `--mapproj-image`.
- If `mapproject` recorded an absolute path (`/path/to/candidate.cub`), passing a relative path (`candidate.cub`) causes `gcp_gen` to abort with:
  `ERROR: The image file in the mapproj header does not match the camera image.`
- **Rule**: Always pass canonical absolute paths (`$(pwd)/candidate.cub` or `realpath`) to `--camera-image`.

### 2. Illumination & Solar Azimuth Matching
- Never match an image against an ortho with opposite or orthogonal shadows. In polar regions, crater shadows rotate with solar azimuth.
- Always pair each candidate image with the reference image that has the **closest solar azimuth** ($\Delta\text{az} \le 5^\circ$).
- Catalog solar azimuths first (e.g. via `sfs --query -t csm` or CSM state `m_sunPosition`) to select optimal reference pairs.

### 3. Tile DEMs vs Large Regional DEMs
- For localized tiles (e.g. central 2x2 km or 3x3 km study area), use the corresponding tile DEM (e.g. `sfs_initial_central_2km_1m.tif`).
- Ensure the bounding box of both the candidate mapprojection and reference ortho intersects the valid data extent of the DEM.

### 4. Detector & Matching Parameters
- Default to `--ip-detect-method 0` (Integral OBALoG). It is native, fast, and robust across illumination scales.
- Use `--ip-per-tile 2500 --ip-per-image 0` to ensure uniform spatial distribution of tie points across the overlap area.
- Set `--gcp-sigma 1.0` (matching the 0.5–1.0 m ground resolution of high-resolution sensors).

### 5. Restrain Degrees of Freedom
- In the bootstrap solve, solve only rigid camera position and orientation (`--camera-position-uncertainty 100 100`).
- Do NOT solve jitter or camera intrinsics during this initial stage. Jitter correction should only be run after initial alignment is achieved.

### 6. Reference Script Implementation
- Canonical reference implementation: `~/projects/sfs/sfs_sim_align.sh` (lines 184–243).
