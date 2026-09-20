---
name: reference_csm_model_state_format
description: "USGS CSM camera model-state \".json\" is NOT pure JSON (plugin-name header line + JSON); how to parse, key m_* fields, and always-check-distortion lesson"
metadata: 
  node_type: memory
  type: reference
  originSessionId: 25d71a61-8a8b-4144-a2e2-80bd37e1f214
---

USGS CSM camera "model state" files (`*.json`, `*.adjusted_state.json` from
cam_gen/bundle_adjust/jitter_solve) are NOT pure JSON: the FIRST LINE is the
plugin name (e.g. `USGS_ASTRO_LINE_SCANNER_SENSOR_MODEL`,
`USGS_ASTRO_FRAME_SENSOR_MODEL`), then a JSON object. `json.load` fails at
"line 1 column 1"; parse with `json.loads(open(f).read().split("\n",1)[1])`.

Distinct from an ISD (input to isd_generate), which IS pure JSON. Model state can
also be embedded in an ISIS .cub.

Key m_* fields (linescan, body-fixed meters/seconds): m_sunPosition / m_positions
len 3*N and m_quaternions len 4*N are TIME SERIES (take middle triplet for image
center), m_t0Ephem/m_dtEphem, m_centerEphemerisTime, m_nLines/m_nSamples,
m_majorAxis/m_minorAxis (Moon 2015 sphere = 1737400 m), m_distortionType (int
enum) + m_opticalDistCoeffs.

LESSON (OHRC/NAC audit 2026-09-20): ALWAYS check m_distortionType/coeffs when
auditing a camera. OHRC states had type 0 + all-zero coeffs = NO distortion
modeled; NAC had type 4 (LROLROCNAC) with real coeffs. Unmodeled distortion cannot
be absorbed by pose-only BA and grows across the footprint (small crop aligns,
full image does not). Full detail + coeff layout in the [[csm-models]] and
lens-distortion skills. Sun az/el per image: `sfs --query` on cub+json, or compute
from m_sunPosition. Ties to project [[ohrc_lronac_align]].
