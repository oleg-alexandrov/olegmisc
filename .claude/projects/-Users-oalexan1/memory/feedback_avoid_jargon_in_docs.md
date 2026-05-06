---
name: Avoid casual jargon in user-facing docs and commit messages
description: For ISIS/USGS PR text, changelogs, app help, commit messages — replace casual engineering jargon with plain wording. Concrete examples Oleg has flagged.
type: feedback
originSessionId: 33d1657a-4fcb-49da-8a80-2efae4df6970
---
In ISIS/USGS-facing prose (XML help text, changes/*.md changelog, commit
messages, PR descriptions, app docs), avoid casual engineering jargon.

Concrete substitutions Oleg has already flagged:
- "spiceinit'd" -> "run through spiceinit"
- "plumbing" (e.g. "mirrors the existing ISD/CSMLIST plumbing") -> "approach"
  (e.g. "mirrors the approach used for ISD/CSMLIST")

**Why:** USGS reviewers and end users include scientists who don't share Oleg's
internal vocabulary; casual shorthand reads as sloppy and obscures meaning.
Same bar applies whether the audience is a reviewer (PR/commit) or an end user
(XML help, RST docs, changelog).

**How to apply:** Treat any clipped verb-as-adjective (`-d` / `-ed` shortcut
turning a tool name into a state) and any building/factory metaphor
("plumbing", "wiring", "stitching", "machinery") as suspect on first draft.
Substitute the concrete action ("run through X", "uses X", "approach used for
X"). When unsure between concise jargon and a slightly longer plain phrasing,
pick the plain phrasing.
