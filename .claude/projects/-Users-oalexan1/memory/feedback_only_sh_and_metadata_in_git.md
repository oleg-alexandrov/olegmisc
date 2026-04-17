---
name: Only .sh and metadata in git, never data files of any kind
description: By default only track .sh and metadata/docs files. Any data file (including small JSON state files) stays out of git unless explicitly asked
type: feedback
originSessionId: 6edea641-7d58-4d85-a5e7-19cea61b1599
---
Default git-tracking rule: only .sh (and similar text/source like .py, .rst, .md, .cmake, .cc, .h) plus metadata and docs. ANY data file, including small text-format JSON camera state files, adjusted_state JSON, .txt image lists, .csv, .json - stays untracked unless Oleg explicitly asks.

**Why:** Oleg stated this 2026-04-16 after I proposed git-tracking a 3654-file jitter_constrained/ backup dir (48 MB of JSON camera state files). Those are data even though they're text - they bloat the repo and slow traversal. The existing CLAUDE.md rule already calls out large JSON, but this is broader: the category "data" excludes JSON state files even when small.

**How to apply:**
- Adding .sh, .rst, .md, .cmake, .cc, .h: fine by default.
- Adding .json camera state files, .tif, .cub, .img, lock files, per-image txt lists, any kind of structured data: DO NOT `git add` unless Oleg explicitly says so.
- If a dir has 1000s of small files, even if each is tiny text: assume don't track.
- When unsure whether something is data or source/notes: ask.
- The preservation mechanism for data backups is rsync + disk redundancy, not git.
