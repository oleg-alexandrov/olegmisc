---
name: Stop when in the ballpark on uncertain camera-model work
description: On camera-model and geometry work (SPOT, Pleiades, PeruSat, ASTER, etc.), stop at "plausible" rather than churning through inherent uncertainty.
type: feedback
originSessionId: 89e156c0-91ae-45bf-a22d-4f8b05dcfcf5
---
When doing sensor-model implementation work (vendor metadata -> CSM, rigorous
camera-model ports, attitude/boresight/frame conventions), stop at "plausible
and pointing down" rather than burning time trying to nail the last pixel.

**Why:** This kind of work has irreducible uncertainty. Vendor docs sometimes
have mistakes, sign swaps, or ambiguities. Frame conventions and distortion
tricks (e.g. SPOT5's TRANSVERSE f=1.0 angle-unit detector) are chosen per
mission and not always documentable from the handbook alone. Oleg has worked
on PeruSat, Pleiades, Pleiades NEO, SPOT5, SPOT6, and ASTER; he expects
residuals to take multiple refinement passes and he is fine doing those
himself. James Hollingsworth makes the same point: "indeed... there's much
uncertainty". Spinning wheels after you're in the ballpark is waste, and the
residual may not even be your fault.

**How to apply:**
- At each sanity gate, ask: is the result plausible (camera points to ground,
  ephemeris looks right, orientation isn't grossly inverted)? If yes, the
  milestone is passed.
- If a residual won't close and the remaining gap feels like
  signed-wrong / axis-swap / convention-mismatch territory rather than a
  clear logic bug, **stop**, write down what you see, and hand it back for
  refinement. Don't brute-force through uncertainty.
- Preserve progress: commit in all three repos (VW, ASP, projects notes)
  before stopping. Never push. Let Oleg take the next turn.
- Explicitly applies to: SPOT 1-4 work, any new linescan/frame camera port,
  any distortion-convention exploration.
