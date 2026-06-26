---
name: feedback-test-on-real-data
description: Test on real representative data and eyeball actual output; synthetic tests with degenerate inputs can mask bugs.
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 131d977f-a1a0-4165-866b-95a648f0a5b0
---

When testing a feature, use REAL representative data and visually inspect the
actual output, not just a synthetic unit test that returns "pass".

**Why:** On sparse_disp `--save-match-file` (2026-06-25) I shipped a bug where
the match file held map coordinates instead of pixel coordinates. My synthetic
test passed only because it used an identity geotransform, so map coords equalled
pixel coords, the exact degenerate case that hid the bug. Another bot caught it
immediately by running on a real georeferenced pair and eyeballing stereo_gui
(matches plotted far off the image). A green synthetic check gave false
confidence.

**How to apply:** For anything touching coordinates, georeferencing, geometry,
or units, test on real data where the degenerate case does NOT hold (pixel !=
map coords, non-identity transform, real parallax). Then look at the actual
output, a GUI overlay or a plot, before declaring it done. Claude has eyes, use
them (see [[reference-geodiff-no-csv-datum]] style real-tool checks). Passing a
synthetic test is necessary, not sufficient.
