---
name: feedback_adapt_existing_workflow
description: "When a project already has scripts/workflow, study and adapt them surgically; never improvise a new parallel one."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: b5e1be74-f279-4864-93aa-d2ee95018670
---

When a task extends an existing project that already has scripts, runners, and a
notes file (e.g. lunamaps `sfs_big.sh` / `parallel_sfs` invocation + the qsub in
`lunamaps_notes.sh`), the FIRST move is to read those and adapt them with the
minimal surgical change requested. Do NOT hand-roll a new parallel workflow from
scratch.

**Why:** On the lunamaps SfS covariance task (2026-06), Oleg asked to add
`--save-variances --save-covariances`, resume from the final DEM + final albedo,
and run 0 iterations. Instead of adapting the existing `parallel_sfs` runner, I
improvised my own `cov_tile.sh`/`cov_job.sh` calling raw `sfs` per tile, with my
own tiling, my own image selection, and exposure/haze FIXED (the real workflow
FLOATS them). It took three redirects ("run parallel_sfs not sfs", "imitate the
existing workflow", "study what we did before") to get on track, and all my OOM
+ SBU diagnostics were measured on the wrong setup, so they had to be redone.

**How to apply:** Before writing any new script, grep the project dir + notes
for the existing invocation (`grep -n parallel_sfs ... ; grep -n qsub ...`), read
the runner end to end, and produce the smallest diff that satisfies the request.
State "here is the existing invocation; here are the N surgical edits" rather
than presenting a from-scratch design. Only deviate where physically forced
(e.g. node RAM), and call that out explicitly as operational, not a recipe
change. See [[feedback_follow_explicit_plan]] and [[feedback_precise_instructions]].
