---
name: feedback_one_ai_disclaimer_per_pr
description: One AI-assistance disclaimer per PR (in the body) is enough - do not repeat it on every comment
metadata: 
  node_type: memory
  type: feedback
  originSessionId: c27aa48f-5f7c-4da6-b4f2-78dbccbafa1c
---

State the AI-assistance disclaimer ONCE per PR, in the PR body. Do not add a second "Developed with AI (Claude) assistance" line to follow-up comments on the same PR. On ALE PR #720 I put the disclaimer in the body AND in a later heads-up comment; Oleg removed the comment's copy, noting "not sure we needed second disclaimer about ai work, that is clear enough from first."

**Why:** The body disclaimer already attributes the whole PR (and the author is the same person). Repeating it on each comment is redundant noise. Attribution on USGS repos is welcome (keep it), but once is enough.

**How to apply:** Put the AI-assistance line in the PR body (and the Co-Authored-By trailer in commits). Leave follow-up comments on that PR clean - no extra disclaimer. Same idea for an issue thread: attribute once, not per reply. Goes with the broader trim-it-down lesson in [[feedback_concise_vs_impl_detail]]; attribution policy itself is in usgs_contrib_notes.sh.
