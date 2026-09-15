---
name: feedback_inspect_discrete_features
description: "Judge terrain/DEM results by inspecting discrete recognizable features side-by-side, never by a correlation score alone."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 016d12ed-0574-415b-8ccc-80ef51e01320
---

When judging a produced DEM (or ortho) against a reference, INSPECT DISCRETE,
RECOGNIZABLE FEATURES side-by-side on a common grid, and confirm the SAME features
land in the SAME places: the main valley, a specific tributary junction, a
distinctive ridge line, a named peak, a river bend. Do NOT report a scalar
correlation score (hp corr, raw corr, NMAD) as the verdict.

**Why:** Oleg's words (2026-09-11, KH-7 linescan work): "this kind of oblivious
driving without visual inspection where I can catch you... you must inspect
discrete features rather than you say corr score." A correlation number is
un-verifiable by the user and actively misleads (raw corr hides tilt, hillshade
hides inversion, a high score can come from a low-frequency ramp aligning). It let
me drive deep into a wilderness (chasing a linescan whose valley you could not even
see) while a number said "+0.222, fine."

**How to apply:** every DEM/ortho stage - warp ours + reference to the SAME grid,
colorize + hillshade both, place side-by-side, and NAME the specific features and
whether each lands correctly. The score can accompany the figure but never replaces
the feature-level look. If you cannot point at matching discrete features, the
result is bad no matter what the score says. Related: [[feedback_inspect_before_filter]],
[[feedback_check_ground_truth_before_artifact]], the visual-inspection skill.
