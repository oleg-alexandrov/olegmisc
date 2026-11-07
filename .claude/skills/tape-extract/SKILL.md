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

- `/u` (lfe home) is mounted ONLY on lfe and `/nobackupp19` is mounted on both
  lfe and pfe. So a MODEST tar (a few GB) can be untarred directly on lfe with
  `-C /nobackupp19/...` - it reads `/u`, writes nobackup. The tar only READS `/u`;
  never write extracted output into `/u` (tape space), and NEVER wipe/clobber
  anything on `/u`. For a LARGE restore do NOT untar on lfe - see the next section.
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

## Large or slow restore: direct I/O, shiftc, and untar OFF lfe

Two hard lessons for anything bigger than a few GB (learned 2026-06-17, the hard way):

- **A plain `tar`/`cp`/`dd` read on lfe crawls at kB/s - that is NOT a broken lfe or a
  tape/dmget problem, it is the WRONG TOOL.** Buffered reads on Lou's CXFS are
  catastrophically slow while DIRECT I/O is ~12,000x faster (same 838 GB disk-resident
  file on lfe5: buffered `dd` ~2.9 kB/s vs `dd ... iflag=direct` ~37 MB/s). Do not "wait it
  out" or file a ticket. Confirm a node is healthy with a direct-I/O probe (expect 10s-100s
  of MB/s): `ssh lfe 'dd if=/u/oalexan1/projects/<f>.tar of=/dev/null bs=1M count=4 iflag=direct'`.
- **Lou kills any process over ~1 GB RAM on the lfe front ends (it runs query_wms there),
  so a big untar/analysis is not permitted ON lfe.** The correct large-restore recipe:
  COPY the tar `/u` -> `/nobackupp19` with a direct-I/O, multi-thread, restartable tool,
  THEN untar on a pfe COMPUTE node (reading from nobackup, which is mounted there). Tools,
  best first: `shiftc` (/usr/local/bin/shiftc, the NAS Shift tool - retries, restarts, runs
  detached, auto-onlines from tape, monitor with `ssh lfe 'shiftc --status'`); `cxfscp`
  (CXFS-aware, large direct I/Os, e.g. `cxfscp -rp --bo /u/... /nobackup/...`); `mcp`
  (multi-threaded cp). NEVER plain `cp`/`dd`/`tar` for a large lfe read.

## Logging

Record a restore the same way as an archive: one line in the project's archive
or main notes (what tar, which members, to which path, when). The durable record
is the notes line on the Mac, not a pfe log file.
