---
name: feedback_scripts_in_workdir
description: "Write scripts in the actual project work dir, not the session scratchpad (scratchpad only for throwaway experiments)."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 6e8ef10f-dc72-4b2b-9fea-5587c74768f5
---

Oleg prefers reusable scripts written **directly in the actual project work dir**
(e.g. `~/projects/cassis_olympus_mons/`), not in the Claude session scratchpad
(`/private/tmp/.../scratchpad/`).

**Why:** the scratchpad is temporary and clears when the session ends, so scripts
written there are lost unless copied out; and he tracks project scripts in git
(`~/projects/.git`). "Normally I like stuff written in actual work dir than in
scratchpad, but for experimental stuff your thing works" (2026-07-20).

**How to apply:** default to writing project scripts (.sh/.py orchestration, eval,
figure builders) into the work dir from the start. Scratchpad is fine for genuinely
throwaway/experimental one-offs. When a run produces keeper scripts, copy them into
the work dir and `git -C ~/projects add`/commit/push (only .sh/.py + notes, never
data/HTML/DEMs). Project scripts do NOT go into shared pipeline repos like
CassisPipeline - those get only real pipeline fixes, pushed only when explicitly told.
Related: [[feedback_deliver_hosted_artifact]], [[feedback_only_sh_and_metadata_in_git]].
