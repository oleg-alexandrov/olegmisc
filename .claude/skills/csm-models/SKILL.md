---
name: csm-models
description: >-
  The USGS CSM camera files ASP uses: the model-state .json format (plugin-name header
  line then JSON, not pure JSON), ISD-vs-model-state, the m_* field layout, distortion,
  and how to parse/inspect them. Load when reading, parsing, or editing a CSM .json or
  adjusted_state.json camera.
---

## The format that keeps surprising us: model-state ".json" is NOT pure JSON

A CSM camera "model state" file (what `cam_gen`, `bundle_adjust`, `jitter_solve`
write - often `*.json` or `*.adjusted_state.json`) is a plain-text file whose
**FIRST LINE is the sensor-model plugin name**, followed by a JSON object:

    USGS_ASTRO_LINE_SCANNER_SENSOR_MODEL
    {
      "m_centerEphemerisTime": 694001429.77,
      ...
    }

So `json.load(open(f))` FAILS with "Expecting value: line 1 column 1". To parse:

    d = json.loads(open(f).read().split("\n", 1)[1])   # drop the header line

The first line (also stored as `m_modelName`) tells the CSM which plugin to load.
Common headers: `USGS_ASTRO_LINE_SCANNER_SENSOR_MODEL` (linescan),
`USGS_ASTRO_FRAME_SENSOR_MODEL` (frame), plus SAR / pushframe variants.

### ISD vs model state (two different .json things - do not confuse)

- **ISD** (Image Support Data): the INPUT to `isd_generate`, usually PURE JSON
  (no header line). Describes raw metadata.
- **Model state**: the camera ASP actually uses, produced from an ISD (or by
  `cam_gen`/`bundle_adjust`). Has the plugin-name header line + JSON. This is the
  one with `m_*` keys below. `*.adjusted_state.json` is a bundle-adjusted state.

CSM model state can also be **embedded inside an ISIS .cub** (see csm.rst
`embedded_csm`); ASP reads it from the cub when present.

## The m_* fields (linescan; units = meters, seconds, body-fixed frame)

Ephemeris are FLAT arrays of a time SERIES (not one triplet):
- `m_sunPosition` len 3*N (body-fixed XYZ per sample), `m_sunVelocity` same.
- `m_positions` len 3*N (sensor XYZ), `m_velocities`, `m_quaternions` len 4*N.
- `m_t0Ephem`, `m_dtEphem`: ephemeris start time (rel. to center) and step; N
  samples span the image. Take the MIDDLE triplet for image-center geometry.
- `m_centerEphemerisTime`, `m_nLines`, `m_nSamples` (image dims),
  `m_detectorSampleSumming`, `m_focalLength`, `m_intTimeLines`/`m_intTimes`.
- `m_majorAxis`/`m_minorAxis`: body radii (Moon 2015 sphere = 1737400 m; equal
  for a sphere).
- Distortion: `m_distortionType` (int enum) + `m_opticalDistCoeffs` (array). See
  [[lens-distortion]] for the enum and per-type coeff layout (0=RADIAL,
  1=TRANSVERSE, 4=LROLROCNAC, 9=CASSIS, ...). A frame model instead has single
  `m_currentParameterValue` position+quaternion, no time series.

## ALWAYS check the distortion when auditing a camera solution (hard lesson)

`m_distortionType` + `m_opticalDistCoeffs` tells you whether distortion is even
MODELED. Burned 2026-09-20 on OHRC/NAC: all 30 Chandrayaan-2 OHRC states had
`m_distortionType: 0` with **all-zero coeffs = NO distortion modeled**, while the
LRO NAC states had type 4 (LROLROCNAC) with real coeffs. An unmodeled optical
distortion cannot be absorbed by pose-only bundle adjustment, so it shows up as a
spatially varying misregistration that GROWS across the footprint (small crop
aligns, full image does not) - the classic "is it pose or internal distortion?"
symptom. Quick audit across a camera set:

    for f in cams/*.json: d=parse(f); print(d["m_distortionType"], d["m_opticalDistCoeffs"])

If distortion is zero/absent but the data suggests a warp, the fix is to MODEL and
SOLVE it: `bundle_adjust --solve-intrinsics` (see [[solve-intrinsics]]) or set a
distortion type and float it, and/or [[jitter-solve]] for a linescan bend.

## Inspecting a CSM camera (no hand-parsing needed)

- `cam_test --image img.cub --cam1 img.cub --cam2 img.json` - compare/validate a
  camera; prints pixel<->ground residuals.
- `sfs -i DEM.tif --image-list imgs.txt --camera-list cams.txt -o x -n 1 --query`
  - prints per-image **sun position and sun azimuth/elevation** (deg) at the DEM,
  then exits. Needs image+camera (cub+json). The sun az/el it prints can be
  reproduced from `m_sunPosition` (middle triplet) at the ground point via a
  local East-North-Up frame (az from North, +East) - validated identical.
- `orbit_plot.py` - plot sensor positions/orientations over the orbit.
- `cam_gen`, `isd_generate` create states; `bundle_adjust`/`jitter_solve` refine
  them; all write the header-line-plus-JSON model state.

## pfe env to run these (non-interactive ssh has nothing on PATH)

    SP=/vast_swbuild/swbuild/oalexan1/projects/BinaryBuilder/StereoPipeline
    export PATH=$SP/bin:$SP/libexec:$PATH
    export ISISROOT=/swbuild/oalexan1/miniconda3/envs/isis10asp
    export LD_LIBRARY_PATH=$ISISROOT/lib:$SP/lib:$SP/lib/csmplugins:$LD_LIBRARY_PATH
    export GDAL_DATA=$SP/share/gdal PROJ_DATA=$SP/share/proj ISISDATA=/nobackupp19/oalexan1/isis3data

The `$ISISROOT/lib` on LD_LIBRARY_PATH is the key piece; without it CSM tools die
with `libusgscsm.so.1: cannot open shared object`. See [[pfe-nas]].
