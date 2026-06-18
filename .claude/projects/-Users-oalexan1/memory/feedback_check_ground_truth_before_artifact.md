---
name: feedback_check_ground_truth_before_artifact
description: "Before calling a structured/repeating raster pattern a processing artifact, check ground truth; it may be real."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: b5e1be74-f279-4864-93aa-d2ee95018670
---

When a raster shows a regular, blocky, or repeating structure (polygonal mesh,
seams, grid ripple), do NOT reflexively label it a processing artifact (tiling,
mosaic seams, per-tile solve). Check ground truth first.

**Why:** On lunamaps SfS uncertainty (2026-06-17) I read the polygonal "seam
network" in the recovered albedo as a per-tile albedo artifact, and said the
height-variance map inherited that artifact. Oleg pulled up Google Maps
(35.072285, -118.153938): the site is a physical engineered target - a concrete
pad with PAINTED high-contrast patches and BUILT craters. The albedo was correct;
the SfS solver recovered real paint. So the uncertainty tracking albedo edges and
dark albedo is genuine signal (less reflected light = less height signal; sharp
albedo edge = real albedo-vs-slope ambiguity), not an artifact. My "artifact" call
was wrong and would have misled the contributor.

**How to apply:** A repeating pattern is evidence, not a verdict. Before writing
"artifact", confirm against an independent source (basemap, the actual imagery,
the site description, an image-count/coverage map). State real-vs-artifact only
after that check, and say which way the evidence points. This compounds with
[[feedback_give_honest_substantive_input]] - honest input includes honestly
flagging when a tidy "it's just an artifact" story is unverified.
