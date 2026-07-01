---
name: feedback-inspect-before-filter
description: Set a filter/threshold only AFTER visually inspecting the data; summary stats hide a coherent systematic signal.
metadata: 
  node_type: memory
  type: feedback
  originSessionId: d01f48d0-7a85-4b9b-a062-0009c0e7c94e
---

Never pick a filter/threshold (e.g. `dem2gcp --max-disp`) from summary stats or percentiles alone. VISUALLY INSPECT the raster first (colorize + eyeball).

**Why:** in CaSSIS s4, `max-disp` was set to 10 px from the disparity percentiles (p50 ~10) without colorizing the disparity. The large-disparity tail was actually a COHERENT, red/uniform systematic shift — real misregistration the GCP exist to correct — not filterable noise. Cutting it at 10 px removed that signal and left a systematic TILT in the DEM (jezero +14.7 m residual). Oleg: "our prev rationale was bad. I did not inspect. that is why the systematic problem was left."

**How to apply:** colorize the disparity AND geodiff (`gdaldem color-relief`, diverging ramp) and look before setting any threshold. A coherent/uniform pattern = real signal to KEEP (loosen the filter); only speckle/noise justifies a tight cut. For `max-disp`, use >= p95 of the disparity norm + a margin, looser still for coherent shifts (asp_mgm makes few outliers). Ties to the "Inspect to Confirm Expectations" and "Claude has eyes" rules.
