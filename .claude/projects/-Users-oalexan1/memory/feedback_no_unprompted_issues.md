---
name: never file github issues without explicit instruction
description: Same rule as PRs - filing issues on any GitHub repo requires an explicit, unambiguous "file an issue" / "open an issue" / "gh issue create" from the user. Don't infer it from "track this" or "log this as an issue" or similar.
type: feedback
originSessionId: 91a965c2-2191-4fe4-b22f-47d580c5c8ec
---
Never open a GitHub issue without an explicit user instruction to do so.

**Why:** Issues are visible to maintainers and the community. An incorrectly worded or premature issue is hard to retract cleanly. Pattern-match this with the existing rule in CLAUDE.md about PRs ("NEVER open a pull request unless explicitly told to"). Same standard applies to issues.

**How to apply:**
- "Track this" / "note this" / "add as a known issue" / "log this" → write to local notes only.
- "File an issue" / "open an issue on GitHub" / "gh issue create" → only then use `gh issue create`.
- If unclear, ASK ("do you want this filed on GitHub or just logged in our notes?") rather than infer.
- Applies to all repos: DOI-USGS/ISIS3, DOI-USGS/usgscsm, NeoGeographyToolkit/StereoPipeline, NeoGeographyToolkit/visionworkbench, etc.
- Also applies to commenting on, closing, or otherwise modifying existing issues — same as PRs.
