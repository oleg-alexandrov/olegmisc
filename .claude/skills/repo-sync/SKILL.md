---
name: repo-sync
description: Intelligently sync a repo to its remote before working - check the remote first, rebase home/projects safely, resolve git-pull-rebase aborts on untracked-vs-tracked collisions by comparing and preserving the RICHER local version, nuke a stale local clone with dead experiments back to god/master, and the git add/push discipline (status-first, HEREDOC trailer, right remote, rebase-not-merge). Load before rebasing/syncing any repo, when a pull aborts on untracked files, or when deciding whether to reset local to the remote. Complements git-repos (the repo/remote reference table).
---

# Intelligent repo sync-up + git add/push

## Check the remote BEFORE local work

When asked to work on a repo, FIRST `git fetch` and compare local vs remote before
editing - the remote may already be AHEAD (a peer / Mac bot pushed), possibly with a
better version than you'd write. Do not assume local is authoritative.
- `git -C <repo> fetch <remote>`; `git log --oneline HEAD..<remote>/master` (behind),
  `git log --oneline <remote>/master..HEAD` (local-only ahead).

## Rebase home + projects (the safe idioms)

- Home dir (repo olegmisc, holds private files): `cd ~ && git pull --rebase
  --autostash`. NEVER `git add .`/`-A` here - add ONE named path at a time.
- Projects: `git -C ~/projects pull --rebase --autostash`.
- Prefer rebase over merge always; when a push is rejected, `git pull --rebase`, never
  a plain merge that branches history.

## `git pull --rebase` aborts: "untracked working tree files would be overwritten"

This means untracked LOCAL files collide with files the remote now TRACKS. Do NOT
blindly `rm` them - one may be the richer copy. For EACH colliding path:
```
git show <remote>/master:<path> > /tmp/.../incoming.tmp
diff -q /tmp/.../incoming.tmp <path>   # IDENTICAL or DIFFERS?
```
- IDENTICAL: safe to `rm <path>` (explicit path) then re-pull - the same content comes
  back tracked.
- DIFFERS: the LOCAL file may be the RICHER version you built up locally, while the
  remote has an older/placeholder copy. Back the local up
  (`cp <path> /tmp/.../rich_backup`), remove the blockers, pull, then RESTORE your
  richer copy over the pulled one - it now shows as a local modification (your work
  preserved, ready to commit). (Burned 2026-08-23: local `study_log.sh` had full
  RESULTS+TIMELINE; the remote copy was a "(pending)" placeholder - a blind rm would
  have lost the real notes.)

## Nuke a stale local clone with dead experiments -> reset to god/master

When local is BEHIND the remote and carries uncommitted EXPERIMENTAL changes that
were superseded by remote commits (a peer bot committed the finished version), and
you're told to reset to the remote:
- Save the diff first: `git diff <files> > /tmp/.../stale_experiments.diff`.
- `git reset --hard <remote>/master` (moves the branch AND wipes the working tree;
  safe only when local is NOT ahead - confirm the ahead-list is empty).
- Remove stale untracked cruft (build logs, `.bak`, scratch) by EXPLICIT literal path,
  one per `rm -f` line, no glob/var (harness-safe; see the cleanup rules in CLAUDE.md).
- Confirm: `git rev-parse --abbrev-ref HEAD`, `git log --oneline -1`, clean `git status`.

## git add / push discipline

- `git status` before EVERY commit - `git commit -a` misses NEW untracked files
  (`.cc`/`.h` must be `git add`ed explicitly). In the home repo, add named paths only.
- Commit with a HEREDOC so the trailer is never dropped:
  `git commit -F - <<'EOF' ... Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com> ... EOF`
- Push to the RIGHT remote (see git-repos table): `god` = upstream org, `origin` =
  fork. ASP -> both `god` and `origin` when told to push. BinaryBuilder -> `god`+`origin`.
  StereoPipelineTest -> `origin` only. ISIS3/usgscsm -> `oleg` remote, NEVER `origin`
  (that's DOI-USGS upstream, PR-only).
- StereoPipeline is NeoGeographyToolkit, NOT DOI-USGS - no DOI changelog/AI-disclosure
  ceremony (that's only for ISIS3/usgscsm/ale/SpiceQL). Just the standard trailer.
- NEVER push without an explicit instruction. NEVER force-push. Verify heads match
  after pushing to two remotes (`git log --oneline -1 HEAD god/master origin/master`).
- ale remote naming (differs from ISIS3/usgscsm): `origin` = oleg-alexandrov/ale (fork,
  push here), `usgs` = DOI-USGS/ale (upstream, PR-only). ISIS3/usgscsm use `oleg` for the fork.

## Opening a DOI-USGS PR: PLAIN link + copy-paste body (Oleg opens it himself)

Oleg opens/reviews the PR himself. After pushing the branch to the fork:
- Give a PLAIN compare URL, `?expand=1` ONLY. Do NOT build a title/body-prefilled URL with
  `&title=&body=` and urllib.parse.quote - the %20/%0A-encoded body is an unreadable HORROR
  SHOW in chat and Oleg hates it (2026-08-28).
  `https://github.com/<UPSTREAM>/compare/<BASE>...<FORKOWNER>:<BRANCH>?expand=1`
  e.g. `https://github.com/DOI-USGS/ale/compare/main...oleg-alexandrov:my_branch?expand=1`
- Put the title + body as PLAIN TEXT in a file on his DESKTOP (`~/Desktop/<name>_pr.txt`,
  "TITLE:" line then "BODY:" then the body) for copy-paste. Give him the plain link + the file
  path, nothing else. He fills the form.

PR BODY STYLE Oleg wants (asked 2026-08-28, applies to DOI-USGS PR bodies):
- Plain text. NO backticks anywhere (spell code/paths/flags out inline).
- ONE line per paragraph - NO mid-paragraph hard wrapping. Blank line between paragraphs and
  between each section label (Description / Related Issue / How Has This Been Validated? /
  Types of changes / Checklist / Licensing) and its content.
- Checkboxes CHECKED as task-list items: `- [x] text` - the leading `- ` AND the space after
  `]` are BOTH required or GitHub renders literal `[x]` / an unspaced item. Check every box
  that applies.
- Imitate `.github/PULL_REQUEST_TEMPLATE.md` section labels. DOI-USGS AI attribution is
  welcome (check the "developed with assistance from Claude" box; add the Co-Authored-By
  trailer to the commit, but NOT a Claude-Session link in a public repo).
- Changelog stays VERY brief (bulleted enumeration of the fixes, minimal prose); the PR body
  carries the longer explanation + validation numbers.

CHANGELOG PR NUMBER (Oleg, 2026-08-28): use the HONEST number, never `[#XXXX]`. If the PR is
already open, get it from `curl .../repos/<UPSTREAM>/pulls?state=all` filtered by
`head.ref == <branch>`. If not open yet, predict = latest issue/PR number + 1 (issues and PRs
share one counter). AFTER the PR opens: `git fetch` + reconcile the branch, confirm the
changelog number matches the real PR, run the tests, and add EXTRA commits on top to fix
anything - NEVER amend/force a pushed or PR'd branch. "After PR open, double-peek, validate,
and fix if you have to."

FINAL-RESULT PROSE (Oleg, 2026-08-28): changelog, PR body, and commit messages state the FINAL
result only, not before/after. Write "ASP cam_test agrees with ISIS to ~5e-4 px", NOT
"0.707 px to ~5e-4 px". Same principle as the code-comment rule below.

CODE COMMENT STYLE (Oleg, 2026-08-28): a comment's ONLY job is to illuminate what the code
below does. NO monologue about code history or evolution - drop "previously X", "used to be
inside the except", "now that the try branch succeeds", "this used to raise ...". That
what-changed narrative goes in the commit message / changelog / PR body, never in a source
comment. Claude tends to tangent into this. Keep comments present-tense, about intent; when
in doubt make them SHORTER.
