---
name: tape-extract
description: >-
  Restoring project data from lfe tape (DMF/DMF tar archives) back to pfe
  nobackup. Carries the one big rule - extract straight into the ORIGINAL
  canonical project path and clobber freely, do NOT build a restore/ wrapper dir
  - plus the DUL/OFL staging check, subset extraction, double-hop quoting, and
  the size-match verify. Load whenever pulling a project (or part of one) off
  lfe tape, extracting a .tar from /u, or deciding where a restore should land.
  Complements pfe-nas (archive side, qsub, head-node limits) and repo-sync.
---

## THE ONE RULE: restore to the ORIGINAL canonical path and clobber

When restoring a project from tape, extract it straight back into the SAME
nobackup path the data always lived at, and clobber whatever is there. Do NOT
invent a `sdb_restore/` or `*_restore/` wrapper dir "to be safe" - that is
over-caution that leaves a parallel stale tree and breaks every hardcoded path
in the notes and scripts.

Why clobbering is safe:
- **The Mac is the base.** All scripts (`*.sh`, `*.py`) and notes are authored
  and git-tracked on the Mac (`~/projects/<proj>`), which is authoritative. The
  pfe copy is a disposable working mirror. Clobbering it loses nothing durable.
- **The tape has the full copy.** Whatever you extract IS the archived truth.
- Our tarballs are packed from the `projects` root WITH the `<proj>/` prefix
  (see pfe-nas "PARENT-DIR-PREFIX packing policy"), so `tar -C <parent>` recreates
  `<proj>/...` exactly where it belongs.

So the default restore is:
```
cd /nobackupp19/oalexan1/projects          # the canonical parent
tar xf /u/oalexan1/projects/<proj>_<part>.tar   # recreates <proj>/... in place
```
or equivalently `tar xf <tar> -C /nobackupp19/oalexan1/projects`. The project
then sits at its real path `/nobackupp19/oalexan1/projects/<proj>` (the same
path the home symlink `~/projects/<proj>` points at), so every relative path in
the notes and every `ortho2.sh`-style script just works.

### The ONLY reason to extract elsewhere

Extract to a side dir ONLY when you have a STRONG, NAMED reason to believe there
is DERIVED DATA on pfe at the canonical path that is newer than the tape and
worth preserving (a later compute run that was never re-archived). In that case:
look first (`ls -la`, compare mtimes against the tar's date), and if there is
genuinely newer derived data, restore to a side dir and reconcile by hand. If
you cannot name what you would be destroying, just clobber in place.

## Staging off tape first (DMF): DUL vs OFL

lfe files are DMF-migrated. Check state before a big read:
```
ssh pfx "ssh lfe 'dmls -l /u/oalexan1/projects/<proj>.tar'"
```
- `(DUL)`/`(REG)` = already on disk, read/extract immediately, no wait.
- `(OFL)`/`(UNM)` = on tape / mid-stage. For a large archive launch a DETACHED
  `dmget` so the stage survives a killed poller (see pfe-nas "Staging a file off
  tape"); for a quick read, the read itself auto-triggers staging.

## Extracting a SUBSET (don't pull 14 GB for 3 files)

- List members fast (GNU tar skips over data on a seekable on-disk tar):
  `tar tf <tar> | grep <pattern>`.
- Extract named members: `tar xf <tar> -C <parent> <member1> <member2> ...`
  (member paths carry the `<proj>/` prefix; no funny shell chars needed).
- Many members: write the list to a file ON SHARED storage and use
  `--files-from=/nobackupp19/.../list.txt` (scp the list to nobackup first, not
  to node-local `/tmp`).
- A whole subtree: just name the dir prefix (`<proj>/ba_green <proj>/dem ...`).
- Run the extract DETACHED (`nohup ... &`) for a big tar - it reads the whole
  archive sequentially and can take minutes even from disk.

## Where extraction runs, and what it must NOT touch

- `/u` (lfe home) is mounted ONLY on lfe, so `tar` that READS `/u/...` must run
  ON lfe. `/nobackupp19` is mounted on both lfe and pfe, so extract TO it from
  lfe. The tar only READS `/u`; never write extracted output into `/u` (tape
  space) - always `-C /nobackupp19/...`. NEVER wipe/clobber anything on `/u`.
- Double-hop quoting (Mac -> pfx -> lfe): outer double quotes, inner single
  quotes, ONE simple command, no `===` markers, no `$VAR`, no `awk $N`, no
  `;`-chains inside (pfe-nas "Double-hop ssh quoting"). A char-class grep like
  `[^/]+` gets eaten - grep a plain substring and filter locally instead.

## After the pull: verify and mirror

- Pulling to the Mac: mirror the SAME relative path under `~/projects/<proj>/`
  (pfe-nas "MIRROR the remote path structure") - never flatten or rename.
- After any copy of a produced file, confirm the LOCAL size == the remote size
  (a mismatch means a partial - re-copy). Only read/crop a file after its writer
  (the rsync/extract) has fully finished.

## Logging

Record a restore the same way as an archive: one line in the project's archive
or main notes (what tar, which members, to which path, when). The durable record
is the notes line on the Mac, not a pfe log file.
