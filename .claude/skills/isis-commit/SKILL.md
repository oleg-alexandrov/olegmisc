---
name: isis-commit
description: How to commit and land a change on a DOI-USGS ISIS3 PR - the towncrier changelog-fragment business (add a one-line changes/<n>.<type>.md file, NEVER edit CHANGELOG.md by hand), the fragment naming/type table, the Co-Authored-By + AI-attribution trailer, and the push-to-oleg-never-origin rule. Load before committing to ~/projects/ISIS3, writing an ISIS changelog entry, or opening/updating an ISIS PR.
---

## The one rule that keeps biting us: ISIS uses towncrier fragments, do NOT edit CHANGELOG.md

ISIS3 manages its changelog with **towncrier**. Contributors never edit
`CHANGELOG.md` directly. Instead you add a one-line **fragment** file under the
`changes/` directory. At release time the maintainers run the `towncrier` tool,
which collects all fragments into a new `CHANGELOG.md` section and then deletes
the fragments. Hand-editing `CHANGELOG.md` is wrong: it duplicates what towncrier
will generate and collides at release. The policy is stated at the top of
`CHANGELOG.md` itself.

If a PR "already has a changelog", it means it already has a `changes/<n>.<type>.md`
fragment. Check `changes/` before adding anything. Do not also add a `CHANGELOG.md`
line.

## Fragment mechanics

- Name: `{NUMBER}.{TYPE}.md`, e.g. `changes/6134.fix.md`.
- NUMBER is the **issue** number the change fixes. If it fixes no issue, use the PR
  number. If it is tied to no issue at all, name it `+<timestamp>.{type}.md`
  (e.g. `+20260917113046.fix.md`).
- If one change fixes two issues, make **two** fragments with the **same** one-line text.
- Content is **one short sentence**, present-tense "Fixed ..."/"Added ...", matching
  the terse house style of the other files in `changes/`. Not a paragraph. Example:
  `Fixed jigsaw failing to read cubes with a CSM sensor model by keeping their unique instrument-based serial number.`

Type suffixes (from `towncrier.toml`, listed in the `CHANGELOG.md` header comment):

- `.break`  - API-breaking change (stays in dev until next major release)
- `.add`    - new feature
- `.change` - change in existing functionality
- `.fix`    - bug fix (back-ported to most recent LTS)
- `.sec`    - security / vulnerability fix
- `.deprec` - soon-to-be-removed feature
- `.rm`     - removed feature
- `.misc`   - excluded from the changelog (tests, CI, refactors with no user impact)

Tests-and-CI-only changes get no user-facing entry (use `.misc` or no fragment).

## Commit conventions for ISIS (a DOI-USGS repo)

- **Push to `oleg` (oleg-alexandrov/ISIS3), NEVER `origin` (DOI-USGS/ISIS3).**
  Upstream lands only via a PR the maintainers merge. Same rule for usgscsm, ale, SpiceQL.
- **Co-Authored-By trailer** on every commit (use a HEREDOC so it is not dropped). Self-inspect your identity:
  `Co-Authored-By: Antigravity <noreply@google.com>` when running as Antigravity, or
  `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>` when running as Claude.
- **AI attribution is welcome** on these repos. Self-inspect identity before attributing: add a one-line
  "This work was done with AI assistance (Antigravity)." or "This work was done with AI assistance (Claude)." to the commit body and to any
  public PR/issue/review text. Describe the bot only, never Oleg's circumstances.
  One disclaimer per PR body is enough; do not repeat it on every comment.
- **Base new work on upstream dev, fetched first**: `git fetch https://github.com/DOI-USGS/ISIS3.git dev`
  and branch off that, not a possibly-stale local `dev`.
- Standard git hygiene still applies: `git status` before commit, add named files
  (new `.cc`/`.h` are not staged by `commit -a`), never commit build dirs or test data,
  rebase not merge, never push without an explicit instruction.

## Build + test before landing

Build and test in the `isis_dev` conda env (never `asp_deps`). Incremental
`ninja install` into the env, then run the focused gtests with `ctest -R <pattern>`.
Full recipe (env vars, PRE_TEST discovery gotcha, flag list): `~/projects/isis_2026/isis_2026_notes.sh`.

## See also

- `~/projects/usgs_contrib_notes.sh` - fuller DOI-USGS contribution policy (AI attribution
  history, predicting the PR/issue number before it exists, per-repo changelog formats).
- `~/projects/isis_2026/isis_2026_notes.sh` - ISIS build/ctest canonical recipe.
- The github-issues skill - prose rules for the PR body / comments (no backticks, no em dashes).
