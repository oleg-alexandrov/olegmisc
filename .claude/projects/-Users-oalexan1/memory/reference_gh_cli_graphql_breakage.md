---
name: GitHub CLI (gh) GraphQL Projects (classic) breakage and REST fallback
description: gh issue view, gh pr view, and gh pr edit fail with GraphQL "Projects (classic) is being deprecated"; use gh api REST endpoints instead.
type: reference
---
The `gh` CLI subcommands `gh issue view`, `gh pr view`, and `gh pr edit` fail
across most repositories (including `NeoGeographyToolkit/StereoPipeline` and
`DOI-USGS/*`) with the error:
`GraphQL: Projects (classic) is being deprecated in favor of the new Projects experience... (repository.issue.projectCards)`

### Rules

1. **NEVER run `gh issue view` or `gh pr view`**: They consistently fail on this GraphQL error.
2. **NEVER run `gh pr edit` or `gh issue edit`**: They fail or silently leave the body untouched.
3. **ALWAYS use `gh api` (REST)** for viewing, editing, commenting, and closing issues and PRs.
4. **NEVER pass `-f body=@file`**: The `-f` flag sends the string `@file` literally. Always send valid JSON via `--input` or stdin.

### Recipes

```bash
gh=$(ls -d $HOME/*conda3/envs/gh/bin/gh | head -1)

# View issue:
$gh api repos/OWNER/REPO/issues/NUM --jq '{title, body, state, state_reason, comments}'

# View PR:
$gh api repos/OWNER/REPO/pulls/NUM --jq '{title, body, state}'

# View comments:
$gh api repos/OWNER/REPO/issues/NUM/comments --jq '.[].body'

# Post a comment:
jq -n --rawfile body /path/to/comment.md '{body:$body}' | \
  $gh api --method POST repos/OWNER/REPO/issues/NUM/comments --input -

# Edit issue/PR body:
jq -n --rawfile body /path/to/body.md '{body:$body}' | \
  $gh api --method PATCH repos/OWNER/REPO/issues/NUM --input -

# Close issue:
$gh api -X PATCH repos/OWNER/REPO/issues/NUM -f state=closed -f state_reason=completed
$gh api -X PATCH repos/OWNER/REPO/issues/NUM -f state=closed -f state_reason=not_planned
```
