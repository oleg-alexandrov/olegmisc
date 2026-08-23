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
