---
name: feedback_concise_vs_impl_detail
description: "Match depth to audience - PR bodies/summaries want what+why+validation, not algorithm derivation"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: c27aa48f-5f7c-4da6-b4f2-78dbccbafa1c
---

Know when to give a concise summary versus implementation detail. On USGSCSM PR #512 Oleg shortened my PR body: he cut the dense math derivation (the chi = [x^2,xy,y^2,x,y,1] basis, the (A1.chi)/(A3.chi) ratio-of-quadratics formulas, the 36-coeff CORR/DIST split) and kept the one-line what-it-is, the enum/dispatch wiring, the pairing with the companion PR, the cam_test validation number, and the attribution.

**Why:** A PR description is read by maintainers deciding whether the change is correct and worth merging - they want WHAT changed and WHY it is trustworthy (validation), not a re-derivation of the algorithm. The math/implementation detail belongs in the code and its comments, not the prose summary.

**How to apply:** For PR bodies, commit messages, and status summaries, default to concise: what changed, why, how it was validated, and pointers (companion PR, changelog). Go into implementation/algorithm detail only when asked, in code comments, or in our own `~/projects` notes. When unsure which mode fits, lead concise and offer to expand. Related: [[feedback_avoid_jargon_in_docs]], [[feedback_precise_instructions]].
