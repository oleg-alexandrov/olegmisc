---
name: image-gcp-gen
description: >-
  Generate Ground Control Points (GCPs) from an uncalibrated or misregistered camera image against an existing registered orthoimage and DEM with gcp_gen, undo mapprojection to raw image coordinates, and run single-camera bundle_adjust with fixed GCPs to bootstrap cameras into an established reference frame before joint bundle adjustment.
---

# Image GCP Generation and Single-Camera Registration (`gcp_gen`)

This skill documents how to register an uncalibrated or poorly oriented camera image against an existing georeferenced orthoimage and DEM using Ames Stereo Pipeline's `gcp_gen` tool and single-camera `bundle_adjust`.

## Why Use This Workflow

When introducing new images to an existing block of co-registered imagery (e.g. adding new linescan swaths to an established DEM mosaic), joint bundle adjustment across all images can fail if initial camera pointings have large errors (tens to hundreds of meters).

In a joint multi-view solve:
- Matches across large initial pointing errors produce huge reprojection residuals.
- Standard outlier filters (`remove-outliers-params`) will either purge those tie points entirely or allow the large error to pull the established cameras off track (gauge drift).
- Optimizers can get trapped in false local minima or fail to converge.

The `gcp_gen` workflow decouples the problem: it matches in planar ortho space, transfers correspondences back to raw sensor coordinates, creates 3D ground control points (GCPs), and solves each camera **individually** against the fixed reference frame. Once all cameras are brought into close agreement ($\le 1\text{ to }2\text{ meters}$), a joint multi-view bundle adjustment can be executed safely with tight initial conditions.

## The End-to-End Pipeline

### Step 1: Initial Mapprojection
Mapproject the candidate image onto the reference DEM using its initial, unrefined camera model:

```bash
mapproject --tr 0.5 -t csm \
  ref_dem.tif candidate.cub candidate_init.json candidate_map.tif
```

Both `candidate_map.tif` and the reference `ref_ortho.tif` now share the same map projection and spatial resolution. Planar displacements (shifts and rotations) can be matched directly without raw camera geometric distortions.

### Step 2: Generate GCPs with `gcp_gen`
Pass the candidate raw image, its mapprojection, the reference orthoimage, and the DEM to `gcp_gen`:

```bash
gcp_gen \
  --camera-image candidate.cub \
  --mapproj-image candidate_map.tif \
  --ortho-image ref_ortho.tif \
  --dem ref_dem.tif \
  --ip-detect-method 0 \
  --individually-normalize \
  --gcp-sigma 1.0 \
  --output-prefix gcp_cand/run \
  --output-gcp candidate.gcp
```

#### What Happens Under the Hood:
1. **Matching in Ortho Space**: `gcp_gen` detects and matches interest points between `candidate_map.tif` and `ref_ortho.tif`.
2. **Undoing Mapprojection (`undo_mapproj`)**: `gcp_gen` inspects the GeoTIFF metadata of `candidate_map.tif` to identify the camera model and DEM used. For each matched point $(x_{\text{map}}, y_{\text{map}})$, it projects backwards through the camera model to calculate the raw image coordinates $(col_{\text{raw}}, row_{\text{raw}})$.
3. **Ground Control Point Synthesis**: For the corresponding reference point $(x_{\text{ortho}}, y_{\text{ortho}})$, `gcp_gen` looks up the latitude, longitude, and DEM elevation, writing a standard `.gcp` file containing fixed 3D ground coordinates paired with raw sensor coordinates.

### Step 3: Single-Camera Pose Adjustment
Run `bundle_adjust` on the single candidate image with the ground points held fixed:

```bash
bundle_adjust \
  --camera-image candidate.cub \
  --camera-model candidate_init.json \
  candidate.gcp --fix-gcp-xyz \
  --camera-position-uncertainty 100 100 \
  -o ba_single/run
```

Because `--fix-gcp-xyz` keeps the 3D ground locations rigid, Ceres adjusts only the candidate camera trajectory (position and orientation) to fit the control points. This completes in seconds and produces `run-candidate.adjusted_state.json`.

### Step 4: Verification by Re-Mapprojection
Mapproject the candidate image again using the newly adjusted camera:

```bash
mapproject --tr 0.5 -t csm \
  ref_dem.tif candidate.cub ba_single/run-candidate.adjusted_state.json \
  candidate_corrected_map.tif
```

Overlay `candidate_corrected_map.tif` with `ref_ortho.tif` in `stereo_gui` to confirm alignment.

---

## Best Practices & Practical Gotchas

### 1. Illumination & Shadow Matching (Critical for Lunar Polar Imagery)
- Never match an image against an ortho with opposite or orthogonal shadows. In polar regions, crater shadows rotate with solar azimuth.
- Always pair each candidate image with the reference image or mosaic that has the **closest solar azimuth** ($\Delta\text{az} \le 5^\circ$).
- If multiple reference images are available, catalog solar azimuths first (e.g. via `sfs --query -t csm`) and match candidate swaths against their closest illumination twin.

### 2. Interest Point Detector Selection
- Default to `--ip-detect-method 0` (Integral OBALoG). In ASP, OBALoG is native, robust to scale, and often produces orders of magnitude more valid matches on planetary surfaces than SIFT.
- Use `--individually-normalize` on floating-point rasters with deep shadows to prevent extreme dynamic range differences from hiding valid features.
- If feature detectors fail due to low contrast or subtle terrain, compute a dense match file via stereo correlation (`parallel_stereo --correlator-mode`) on the mapprojected pair and feed it to `gcp_gen` via `--match-file`.

### 3. Restrain Degrees of Freedom
- In the bootstrapping single-camera solve, refine only rigid camera position and orientation (`--camera-position-uncertainty 100 100`).
- Do NOT attempt to solve fine orientation knots (jitter) or camera intrinsics during this initial stage. Jitter correction requires an already tight alignment to prevent knot oscillations.

### 4. DEM Path Integrity
- `undo_mapproj` reads the absolute DEM path recorded in the GeoTIFF header of the mapprojected image.
- If the DEM was moved or renamed, `bundle_adjust` and `gcp_gen` will reject the unprojection unless `--accept-provided-mapproj-dem` is passed.

### 5. Inspecting Results
- Inspect the generated match file in `stereo_gui`:
  ```bash
  stereo_gui candidate.cub ref_ortho.tif gcp_cand/run-candidate__ref_ortho.match
  ```
- Inspect the GCP residuals in `ba_single/run-pointmap.csv` to ensure mean errors are sub-pixel before feeding the adjusted camera into a joint bundle.
