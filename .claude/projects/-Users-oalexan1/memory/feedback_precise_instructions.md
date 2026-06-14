---
name: Do exactly what was asked, no extras
description: When the user says "add to the script", do not also run the same step live. The user may be doing it manually in parallel; duplicate work can corrupt shared outputs
type: feedback
originSessionId: 6edea641-7d58-4d85-a5e7-19cea61b1599
---
When the user specifies a discrete action, do ONLY that action. Do not volunteer an adjacent "while we're at it" step, especially one that could run in parallel with the user's manual work.

**Why:** Oleg flagged 2026-04-16 - they asked me to add `stereo_gui --create-image-pyramids-only` to the correlator script for future runs. I added it AND offered to also build pyramids now for existing outputs. They were already building them manually. Two concurrent writes to the same `_sub*.tif` pyramid files would corrupt them.

**How to apply:**
- "add X to script" = edit the script only. Don't also invoke X.
- "write Y and give me the command" = write, then show the command, do not run.
- "rename foo.sh" = just rename; do not also commit unless asked.
- If a follow-on step seems natural, STATE it as a suggestion the user can reject, don't execute it.

**Nuance - when to take independent initiative vs when to ask:**
- When Oleg is working interactively alongside you, any action that touches a shared file or output is risky - ASK first if you are going to do more than the literal instruction.
- When you are working ALONE (autonomous /loop, overnight monitoring, explicit "go off and do X"), more independent choices are fine and expected. Oleg has said so explicitly.
- Rule of thumb: "two folks doing the same thing at the same time" is the failure mode. If Oleg might plausibly be doing it concurrently (building pyramids, editing a file, submitting a job), ask. If clearly solo, act.

The cost of unrequested concurrent action is potentially irrecoverable (corrupted outputs, lost work). The cost of a quick "want me to also do X?" is near zero.
