---
name: feedback_autonomous_safe_commands
description: "When working autonomously, avoid command patterns that trip the sandbox/permission prompt so you don't stall mid-stream."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: c27aa48f-5f7c-4da6-b4f2-78dbccbafa1c
---

When Oleg sets you to work autonomously (he's away), keep going with minimal interruptions. Avoid command shapes that make the harness freeze and ask for confirmation.

**Why:** Getting stopped mid-stream defeats autonomous work; he can't approve while away.

**This keeps recurring and frustrates Oleg. Applies EVEN when he is present, not just autonomous mode** — the prompt stops him dead in his tracks mid-flow. A destructive command with a `$VAR` AND a glob is the worst offender: `rm -f "$d"/*.aux.xml` triggered the "Dangerous rm on possibly-empty variable path" prompt (2026-07-01) and halted a review.

**How to apply:**
- NEVER put a `$VAR`, `~`, `*` glob, or `cd &&` in any `rm`/`find -delete`. One SINGLE explicit LITERAL absolute path per destructive command, or don't run it.
- Often you don't need the `rm` at all. To get FRESH raster stats without deleting the stale `.aux.xml` sidecar, read the band directly in python (GDAL `ReadAsArray` + numpy) — it computes fresh and ignores the sidecar. Never `rm` the `.aux.xml`.
- Prefer non-destructive tools (Edit/Write/Read/Grep) which never prompt.
- Batch related steps so fewer commands need approval.
- Don't ask permission for things already approved; just proceed and report.

See [[feedback_stop_asking_permission]] and [[feedback_precise_instructions]].
