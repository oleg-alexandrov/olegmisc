---
name: git-repos
description: Git repository reference and GitHub CLI - the lunokhod1 repo table (paths, branches, origin/god remotes for ASP/VW/BinaryBuilder/ISIS/etc.), gh CLI paths and REST/GraphQL recipes, PR handoff URLs, and Dependabot alerts. Load when doing multi-repo git operations, pushing to a specific remote, or using gh.
---

## Git Repositories on lunokhod1

**Git version:** 2.17.1 (use `git rev-parse --abbrev-ref HEAD` not `git branch --show-current`)

| # | Repo | Base directory | Branch | `origin` remote | `god` remote (upstream) |
|---|------|---------------|--------|-----------------|------------------------|
| 1 | **StereoPipeline (ASP)** | `/home/oalexan1/projects/StereoPipeline` | master | `oleg-alexandrov/StereoPipeline.git` | `NeoGeographyToolkit/StereoPipeline.git` |
| 2 | **VisionWorkbench (VW)** | `/home/oalexan1/projects/visionworkbench` | master | `oleg-alexandrov/visionworkbench.git` | `visionworkbench/visionworkbench.git` |
| 3 | **BinaryBuilder** | `/home/oalexan1/projects/BinaryBuilder` | master | `oleg-alexandrov/BinaryBuilder.git` | `NeoGeographyToolkit/BinaryBuilder.git` |
| 4 | **StereoPipelineTest** | `/home/oalexan1/projects/StereoPipelineTest` | master | `NeoGeographyToolkit/StereoPipelineTest.git` | (origin IS the org repo) |
| 5 | **projects** (scripts/notes) | `/home/oalexan1/projects` | master | `oleg-alexandrov/projects.git` | (no god) |
| 6 | **home dir** (dotfiles) | `/home/oalexan1` | master | `oleg-alexandrov/olegmisc.git` | (no god) |

Convention: `origin` = user's fork, `god` = upstream org (for ASP, VW, BinaryBuilder).

**BinaryBuilder has several heads and is pushed DIRECTLY to BOTH remotes.**
Unlike ASP/VW (where `god` receives changes only via reviewed PRs), BinaryBuilder
changes go straight to `god` (NeoGeographyToolkit, the canonical) AND to `origin`
(user fork). So when told to push BinaryBuilder, push to `god` master and
`origin` master both, and confirm the two heads plus local `master` all match.
(Still requires an explicit push instruction, per the never-push-without-
authorization rule; this only says WHERE once authorized.)

**StereoPipelineTest: ALWAYS push to `origin` master. There is NO fork and NO
separate branch.** `origin` IS the org repo (NeoGeographyToolkit/StereoPipelineTest),
so master is the only branch. A temporary `dev` branch existed briefly and was
deleted (local and origin) on 2026-08-04 - do not recreate one. Commit test
changes on master and push there.

## GitHub CLI (gh)

Full reference (paths, repo slugs, GraphQL/REST recipes, CI): `~/projects/github_notes.sh`.
`gh` is NOT on PATH on any box; it lives in a dedicated `gh` conda env. Portable:
`GH=$(ls -d $HOME/*conda3/envs/gh/bin/gh)`. Concrete paths: Mac mini
`/Users/oalexan1/anaconda3/envs/gh/bin/gh`; lunokhod1
`/home/oalexan1/miniconda3/envs/gh/bin/gh`. Do NOT waste calls running bare `gh`
first (it is `command not found`) - go straight to the env path. **CRITICAL:** `gh
issue/pr view` and `gh pr edit` error on the deprecated Projects-classic API -
use `gh api` (REST) for any issue/PR body/comment/state/label fetch or edit; and
- editing a PR body: `gh api -X PATCH repos/OWNER/REPO/pulls/N -F body=@file`
  (issue body: `.../issues/N`; `-F body=@file` reads the text from a file). `gh
  pr edit --body-file` exits 1 on the Projects-classic GraphQL error and the
  edit does NOT apply, so always go through `gh api` PATCH for body edits; and
**never trust WebFetch summaries of issues/PRs** (it hallucinates) - pull with
`gh api`. PR/issue/comment/review prose-style rules: `~/projects/github_text_style.sh`.
When opening or editing a PR/issue/comment body, write plain prose: avoid
backticks, avoid hard newlines within a paragraph (keep each paragraph on one
line), and avoid angle brackets or other constructs GitHub can read as an HTML
tag and swallow (e.g. `get<double>` renders as nothing) - reword instead.
NO inline backticks in any PR, issue, comment, or README.md (RECURRING violation,
2026-08-28 - stop doing it). WHY: GitHub renders inline-backtick text in a monospace
font that adds ugly extra whitespace around commas and punctuation, so a technical
sentence full of `foo`, `bar` reads badly. Spell code, paths, flags, and type names
out in plain words instead; italic is acceptable when emphasis is truly needed.
Standalone code blocks (triple backticks on their own line) ARE fine - only INLINE
backticks are banned. SELF-CHECK before posting or PATCHing any body: pull it back
with gh api and confirm the inline backtick count is 0
(`gh api .../pulls/N --jq .body | grep -c '\x60'`).

**PR handoff: generate a PREFILLED "compare" URL** (title + URL-encoded body,
`expand=1`), not the plain create-PR link GitHub already offers on push. The
`?body=` param REPLACES the repo's auto PR template, so embed that repo's
`.github/PULL_REQUEST_TEMPLATE.md` in the body, checking the boxes that apply.
Do NOT open the PR (public-facing) unless told. Recipe + generator:
`~/projects/github_notes.sh`.

## Dependabot / Security Alerts

When a `git push` shows Dependabot or security vulnerability warnings, proactively
flag it and offer to investigate/fix.
