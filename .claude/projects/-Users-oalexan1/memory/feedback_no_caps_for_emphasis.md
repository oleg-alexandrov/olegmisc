---
name: feedback_no_caps_for_emphasis
description: "Don't ALL-CAPS words for emphasis in code/comments/notes/prose - reads like shouting"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: c27aa48f-5f7c-4da6-b4f2-78dbccbafa1c
---

Do not capitalize words for emphasis (LAST, ALWAYS, MUST, NEVER, ORDER-SAFE, CRITICAL, etc.) in scripts, comments, notes, or prose. Oleg: "looks as if somebody shouting. i can read." Write "last", "always", "order-safe" in normal case.

**Why:** All-caps emphasis reads as shouting and he finds it grating; the meaning is clear without it.

**How to apply:** Real identifiers stay as they are - env vars (PATH), tool flags, acronyms (ASP, CSM, ISIS, MOLA, DEM, PBS). Only drop the caps when the purpose is emphasis. This is a standing policy for everything I write. Note: existing CLAUDE.md / older notes use caps headings heavily - that is the repo's own style, not a license to add emphasis caps in new writing.
