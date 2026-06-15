---
name: feedback_no_blocking_questions_when_autonomous
description: "When running autonomously, never use blocking AskUserQuestion; log decisions and pick a default instead"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 1b8e8c6d-1a52-45c2-87e9-e29d90b634e1
---

When Oleg has stepped away / told me to run autonomously ("do your own thing, I'm busy, don't wait on me"), do NOT use the AskUserQuestion tool or any other prompt that blocks/freezes the harness waiting on him. Instead: quietly LOG the decision point and my chosen default into the project notes file on disk, pick a sensible default, and POSTPONE anything that truly needs his input until he is back.

**Why:** A blocking question while he is away freezes the harness/session — nothing progresses and the autonomous run stalls until he happens to return.

**How to apply:** In autonomous mode, replace "ask the user" with "write the open question + my default into the project `.sh` notes and proceed." It IS fine to ask questions when the work is DONE, as long as it is NON-mandatory — i.e. in plain prose at the end (he can answer or ignore), never via the blocking AskUserQuestion tool. Reserve the blocking tool for when he is actively in the conversation. See [[feedback_give_honest_substantive_input]] (still surface my real read — just in the log or end-of-run prose, not via a blocking prompt).
