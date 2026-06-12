---
name: feedback_say_passes_not_green
description: "Say \"the tests pass\" / \"the PR passes\", not \"CI is green\" - in PR text and to Oleg"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: c27aa48f-5f7c-4da6-b4f2-78dbccbafa1c
---

Prefer "the tests pass" or "the PR passes" over "CI is green". Oleg: "saying green is awkward. saying it passes is better." He rewrote my comment line to "...makes the PR pass" instead of "turns CI green".

**Why:** "green" is CI-dashboard jargon; "passes" is plain and clearer to any reader.

**How to apply:** In PR/issue text AND in chat with Oleg, write "the tests pass", "the build passes", "all jobs pass", not "green" / "all green". Part of the same plain-language preference as [[feedback_avoid_jargon_in_docs]].
