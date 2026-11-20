---
name: feedback-galleries-local-html
description: "Image galleries (max-lit crop comparisons etc.) are local HTML opened in the browser, never published as Artifacts"
metadata:
  node_type: memory
  type: feedback
  originSessionId: 743f4f92-e70a-4cdc-94c5-775024987f95
  modified: 2026-11-20T20:57:47.415Z
---

Build image-comparison galleries as a local HTML file in the project dir and `open` it in the browser. Do not publish them as an Artifact.

**Why:** Oleg, 2026-11-20, on a 333-crop BCU max-lit gallery: "that gallery is too big. make local html from now on." CLAUDE.md already says generated HTML reports open locally, and the Artifact publish had to be split across two uploads.

**How to apply:** for any crop gallery or HTML report built from data, write index.html plus img/ under the project's review dir, run `open <path>/index.html`, and give the plain path. Only publish an Artifact if Oleg asks for one by name.
