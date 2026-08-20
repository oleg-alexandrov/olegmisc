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
`gh` not on PATH: `$(ls -d $HOME/*conda3/envs/gh/bin/gh)`. **CRITICAL:** `gh
issue/pr view` and `gh pr edit` error on the deprecated Projects-classic API -
use `gh api` (REST) for any issue/PR body/comment/state/label fetch or edit; and
**never trust WebFetch summaries of issues/PRs** (it hallucinates) - pull with
`gh api`. PR/issue/comment/review prose-style rules: `~/projects/github_text_style.sh`.
When opening or editing a PR/issue/comment body, write plain prose: avoid
backticks, avoid hard newlines within a paragraph (keep each paragraph on one
line), and avoid angle brackets or other constructs GitHub can read as an HTML
tag and swallow (e.g. `get<double>` renders as nothing) - reword instead.
NO inline backticks in any PR, issue, comment, or README.md - use italic
instead. Standalone code blocks (triple backticks) are fine.

**PR handoff: generate a PREFILLED "compare" URL** (title + URL-encoded body,
`expand=1`), not the plain create-PR link GitHub already offers on push. The
`?body=` param REPLACES the repo's auto PR template, so embed that repo's
`.github/PULL_REQUEST_TEMPLATE.md` in the body, checking the boxes that apply.
Do NOT open the PR (public-facing) unless told. Recipe + generator:
`~/projects/github_notes.sh`.

## Dependabot / Security Alerts

When a `git push` shows Dependabot or security vulnerability warnings, proactively
flag it and offer to investigate/fix.
