---
name: feedback_ai_disclaimer_bot_only
description: "AI disclaimers in public GitHub text describe the bot only, never the user's circumstances"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 6d4dca73-195c-45ea-81ab-4b808a8356e5
---

When stating Claude/AI assistance in any public GitHub text (commit, PR, comment, review), describe only that a bot/Claude did the work. Never mention the user's circumstances - the hour, schedule, being asleep/awake, mood, or any personal context. Keep it minimal: "Done with Claude/AI assistance." and stop.

**Why:** The reader's only legitimate interest is that AI was involved. Anything about Oleg's personal situation is none of their business and reads oddly.

**How to apply:** Use the bare standard disclaimer. Do not editorialize or explain timing. Related: [[feedback_one_ai_disclaimer_per_pr.md]] (one disclaimer per PR), [[feedback_concise_vs_impl_detail.md]].
