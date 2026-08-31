---
name: github-issues
description: How to write and file GitHub issues, PRs, and comments (any repo). Carries the prose formatting rules Oleg insists on (no inline backticks, no em dashes, flowing paragraphs, no horizontal rules), the verified step-by-step reproduce requirement for bug reports, and the AI-attribution line. Load before filing or drafting ANY GitHub issue, PR body, or comment.
---

## When to load

Load before writing or filing any GitHub issue, pull-request body, or comment on any
repo. These rules apply everywhere. That includes ISIS3, usgscsm, ale, ASP, and VW.

## Only act on an explicit instruction

Draft by default. File, comment on, close, or edit an issue or PR only when Oleg
explicitly says so. Examples of explicit instructions: "file an issue", "post this
comment", "create the PR". Phrases like "track this" or "log this" mean local notes,
not GitHub. If it is unclear, ask.

## Prose formatting

This is the part Oleg keeps correcting. Follow it exactly.

No inline backticks in prose. In running paragraphs, set identifiers, filenames,
commands, flags, keywords, and paths in *italics* using single asterisks. Never wrap
them in backticks. Backticks appear only inside a standalone fenced code block.

No em dashes. Never use a long em dash to join clauses. End the clause with a period
and start a new sentence, or use a colon to introduce something.

Flowing paragraphs. Write full paragraphs. Put a line break only between paragraphs,
as a single blank line. Never hard-wrap or drop single line breaks inside a paragraph.
A paragraph is one continuous line of prose that the renderer wraps.

No horizontal rules. Do not use a horizontal rule anywhere. In particular never place
one above the attribution line.

Bullet lists and fenced code blocks are fine. Prose inside a bullet follows the same
italic-not-backtick and no-em-dash rules.

## Bug reports: a verified step-by-step reproduce section

A bug report must let the reader reproduce from nothing. Include a How to reproduce
section that starts from fetching real, public data and walks every command in order.
For example: fetch the input with wget from a real public URL, run the ingest tool,
then run the command that fails. Show the exact error in a fenced code block.

Verify the entire chain locally before filing. Actually run the wget, the ingest, and
the failing command, and confirm the error reproduces. Never put an unverified URL or
command into a public issue. That is guessing, and it wastes the maintainer's time.

## Self-contained

Never reference a private work-notes file, a project subdir name, a scratch path, or
any internal plan in issue, PR, or comment text. The reader will never have those. Put
the rationale inline.

## AI attribution

End with a single plain line, with no horizontal rule above it:

    Reported with Claude/AI assistance.

Use "Done with Claude/AI assistance." for a PR. Describe only the bot. Never mention
the user's hour, schedule, or circumstances. Use one disclaimer per issue or PR, in
the body, not repeated on every comment. For DOI-USGS repos such as ISIS3, usgscsm,
and ale this attribution is welcome and expected.

## Guess the PR number in changelog entries, do not two-step

When a changelog or NEWS entry needs the PR number, GUESS it before opening the PR.
Do not open with a placeholder like XXXX and then push a correction. That makes CI
and the regression suite run twice, which wastes the maintainers' resources.
GitHub shares one counter across pull requests and issues in a repo, so the next
number is usually max(latest PR number, latest issue number) plus 1. We are usually
right. After the PR opens, VERIFY the number it actually got and correct the entry
if the guess was wrong.

## Filing mechanics

The gh CLI is not on PATH. Use the full path and target the correct repo. See the
git-repos skill for the gh path. File against the upstream repo, for example
DOI-USGS/ISIS3, not a fork. Write the body to a file and pass it with --body-file so
the formatting survives.

Editing a PR body: `gh pr edit <n> -R <repo> --body-file <file>` can SILENTLY FAIL
to apply on repos that still have Projects (classic). It aborts on a GraphQL
"Projects (classic) is being deprecated" error and leaves the body unchanged.
Always verify the body after editing. If it did not take, update via the REST API,
which does not touch the Projects path:
`gh api -X PATCH repos/<owner>/<repo>/pulls/<n> -F body=@<file>`. The same REST
fallback works for issues (`.../issues/<n>`).
