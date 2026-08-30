---
name: feedback_ask_dont_serve_stale
description: "When the task's point is NEW results and fresh data isn't in hand, ASK/fetch — never quietly hand over stale/known-bad results (or reconstruct from them)."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 22bc0e59-a466-4e7a-ba1d-b00df6559c6c
---

When a task exists specifically to produce FRESH results (a new run, re-measured
data, an updated figure), and the fresh results are not in hand — remote host
down, data not pulled, job unfinished — do NOT silently substitute OLD or
known-bad results, and do NOT reconstruct/approximate the result from a stale
local artifact and present it as the deliverable.

**Why:** the old data was frequently bad on purpose and IS the reason for the new
run. Serving it quietly defeats the whole point, and a reconstruction from stale
inputs can reintroduce the exact defect the new run fixed. Burned 2026-08-30 on
the WV03 green-CCD decoupled figure: pfe was offline, so instead of asking I
built the "after correction" panels as `C1 - roll(C1,13)` from a stale local
curve — which manufactured the very +13 seam spikes the registration shift had
removed (the opposite of the shipped result). Oleg caught it; the honest fix was
to pull the REAL re-measured per-scene residuals from pfe once he restored access,
and the stds then matched the validated run exactly.

**How to apply:** no fresh data in hand → say so plainly and ASK, or go fetch the
fresh source, before producing anything. If a stopgap is unavoidable, label it
loudly as stale/reconstructed and state what the real path is — never let old
data pass as the new deliverable. Related: [[feedback_dont_cover_bugs]],
[[feedback_trace_dont_guess]], [[feedback_test_on_real_data]].
