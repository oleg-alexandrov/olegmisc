---
name: feedback_trace_dont_guess
description: "When debugging why two code paths differ, trace the actual source with compile+cout, don't hypothesize from behavior"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 30ce5904-bf31-4cde-864b-5ac003061c1d
---

When investigating why two tools/paths behave differently (e.g. parallel_stereo
vs bundle_adjust matching the same images), do NOT settle for a plausible-sounding
hypothesis inferred from logs and behavior. Read the actual source, find the shared
function and the divergent callers, and PROVE the cause by adding cout/instrumentation,
recompiling, and running both paths to compare. State hypotheses as hypotheses until
proven; never assert a mechanism you have not read in the code.

**Why:** Oleg pushed back hard (2026-07-30, CaSSIS ox2 jitter): I claimed bundle's
`--ip-match-radius` was applied "through the jittered camera geometry." He said "that
is not possible, matching all happens in proj domain, stop guessing." He was right -
the real cause (found by reading the code + cout trace) was that bundle skips image
normalization for non-OpenCV detectors (OBALoG), so OBALoG saw a near-flat raw image.
My guess was confidently wrong and wasted his trust. He'd flagged a whole day of
guesswork.

**How to apply:** For any "why does X differ from Y" debugging, (1) grep to the shared
function and the two callers, (2) read what args/state each passes, (3) instrument with
cout, compile (`make install` on Mac), run both, compare, (4) only then state the cause.
Clean up debug code after. See [[feedback_dont_cover_bugs]], [[feedback_test_on_real_data]].
