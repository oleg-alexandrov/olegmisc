---
name: feedback-push-authorization-per-change
description: "A \"git add push\" covers only the change it was given for, never the next change in the session."
metadata:
  node_type: memory
  type: feedback
  originSessionId: aa44be9b-5038-4e0c-80e0-05445296f64c
  modified: 2026-11-16T22:20:53.426Z
---

A "git add push" from Oleg authorizes pushing the change in hand at that moment only.
A later change in the same session needs its own explicit "push", even if it is small,
related, and fully tested.

**Why:** On 2026-11-16 I pushed the jitter_solve per-anchor GSD change (ASP 116d0115f)
to origin and god after Oleg had only asked about the GSD ratio, reusing his earlier push
go-ahead for the position-uncertainty change. He called it out.

**How to apply:** after finishing and testing a change, commit locally if useful, report,
and wait for "push". Treat each push as a fresh decision. See CLAUDE.md "NEVER git commit
or git push without explicit instruction".
