---
name: tape-archive
description: >-
  Writing project data to NASA lfe (Lou) tape for long-term archive - the
  never-clobber and one-tar-at-a-time policy, the archive_to_lfe.sh tool and
  parent-dir-prefix packing, the per-project archive-log that is the SOURCE OF
  TRUTH for what is on tape (never a flaky live lfe query), the cub/json and
  consolidation keep rules, the pre-archive Mac-vs-pfe cross-check and rsync, the
  git checkpoint and the sensitive-project read-lock, and dmget staging. Load
  before archiving a project (or part of one) to lfe, running archive_to_lfe.sh,
  writing a <proj>_archive_notes.sh log, or deciding an archive-gated wipe.
  Companion to tape-extract (restore side) and pfe-nas (pfe/ssh/double-hop,
  deletion safety). lfe access: direct ssh lfe works from the Mac (and from pfe).
---

Writing project data to lfe (Lou) tape. This is the archive (write) side. For
RESTORE (pulling data back off tape) see [[tape-extract]]. For pfe/Athena compute,
ssh/double-hop mechanics, and deletion safety see [[pfe-nas]]. Deep reference:
`~/projects/lfe_archive.sh`. The generic tool is `~/bin/archive_to_lfe.sh` (deploy
to the lfe-visible `/nobackupp19/oalexan1/projects/` before running).

- **NEVER wipe or overwrite anything on lfe (CRITICAL).** Tape is the ultimate,
  irreplaceable original archive. Never delete, clobber, or overwrite any dataset on lfe.
- **lfe access: DIRECT `ssh lfe` works from the Mac (verified 2026-11-06, lands on lfe5),
  and also from pfe.** The `Host lfe` block in `~/.ssh/config` carries
  `ProxyCommand ssh sfe ssh-balance lfe`, so a single `ssh lfe '...'` from the Mac
  transparently proxies through the sfe gateway - no manual `ssh pfx` then `ssh lfe`
  double hop is needed. Suppress the CUI banner the usual way: `ssh lfe 'cmd 2>&1' 2>/dev/null`.
  PREFER this single-hop form - it sidesteps the double-hop quoting traps entirely (you
  can use `$VAR`, `awk $N`, `;`-chains, and markers in the remote body, within normal
  single-ssh quoting). The old `ssh pfx "ssh lfe '...'"` double hop still works and is the
  natural form WHEN YOU ARE ALREADY ON pfe (an inside hop `ssh pfe 'ssh lfe "..."'`), but
  from the Mac the direct `ssh lfe` is the default. For anything with heavy quoting, still
  write a small script and run it by absolute path (scp it to lfe first).
- **ONE TAR AT A TIME - NEVER run concurrent `archive_to_lfe.sh` writes to lfe (CRITICAL).**
  lfe is ACTUAL magnetic tape: a robotic library mounts a physical cartridge and streams the
  tar to it. Concurrent tar writes make the robot thrash between cartridges and strain the
  hardware. Launch ONE archive, poll its log for `END ... rc=0`, and only THEN launch the
  next - QUEUE multiple projects/parts sequentially, never fan them out. (Burned 2026-11-01:
  launched 4 tars at once - Oleg: "don't want to strain that poor thing, it does actual tape.")
  Verifying a FINISHED tar (`tar tf`) reads the still-resident on-disk copy and is fine; only
  the concurrent WRITES must be avoided. Detail: `~/projects/lfe_archive.sh`.
- **ANYTHING TAPE-RELATED = UTMOST SERIOUSNESS, and do NOT depend on a live lfe QUERY to decide a wipe
  (Oleg, 2026-11-01).** A live lfe listing can SILENTLY RETURN EMPTY (seen 2026-11-01 - two archive
  checks came back blank; the old Mac->pfx->lfe double hop was especially prone to this, which is one
  more reason to prefer the direct single-hop `ssh lfe` above). An empty result from a
  query is NOT evidence of "not archived" OR "archived" - it is NO information, and must NEVER greenlight a
  deletion (an empty listing could wrongly look like "safe to wipe" and destroy UNARCHIVED data). **The
  SOURCE OF TRUTH for what is on tape is the project's own tape-archive LOG**, which policy keeps precisely
  so we never depend on a live lfe query: the per-project archive-notes `.sh` (e.g. CaSSIS:
  `~/projects/cassis_asp/cassis_cleanup_plan.sh`) and the general `~/projects/lfe_archive.sh`. **RULE for an
  archive-gated wipe: only wipe a dir as "already archived" when the LOG positively records its `.tar` on
  lfe (named tarball + size/date). If the log is silent or ambiguous, do NOT wipe** - leave the dir, or only
  trim regenerable intermediates in place; archiving-then-wiping an unlogged dir needs an explicit, verified
  tape write first (and that is a deliberate, careful operation, never a side effect of a cleanup). When you
  do wipe an archived dir, record the recovery tar path in the wipe audit line.

- **`.cub` AND camera `.json` FILES ARE NEVER REGENERABLE BULK - across ANY mission,
  always KEEP + ARCHIVE them, never wipe them as "re-fetchable".** This is the standing
  default for every cleanup/archive pass, for any instrument (LRO NAC, OHRC, CTX, CaSSIS,
  HiRISE, any ISIS cube), not a per-project judgement call. A `.cub` is the product of real
  ingest work (download + `spiceinit` + calibration) and a camera `.json` (CSM model-state /
  ISD / `adjusted_state.json`) is the product of a bundle/jitter solve - both are small
  relative to run output and precious, so even when the raw source is technically
  re-orderable from a public archive, the cubes and cameras go INTO the tape tar and are the
  LAST things to wipe, never folded into the regenerable-intermediate pile. Burned 2026-11-01:
  an ohrc_lronac_align archive first EXCLUDED the 61 GB `lro_nac/` cubs as "re-fetchable NAC"
  while keeping the OHRC cubs - Oleg caught it ("but lro nac is also cub", then "going forward
  keep whatever cub files and json files by whatever mission"). Rule: if it ends in `.cub` or
  is a camera `.json`, it is kept and archived, full stop. Match files, `run-*.tif` stereo
  intermediates, mapproj eval tiles, and figures ARE regenerable and wipe freely - cubes and
  cameras are not.
- **The ban covers CONSOLIDATION / "distilling" a project down to its derived products, not
  just explicit `rm` passes.** A project is NEVER "fully kept" when its `.cub` / camera `.json`
  / input data are gone and only the DERIVED products (DEMs, aligned linescan, CTX ref,
  hillshades, eval tiles) remain. Derived products do NOT let you re-run the earlier stages:
  without the cubs you cannot rebuild the dense matches (stage 6), so you cannot re-run the
  stereo/bundle with a new recipe - you are forced into a full PSA re-fetch + re-ingest first.
  So when consolidating many sites into one deliverable tree, the cubs and camera `.json` go
  IN (or to tape), they are not "distilled away". NEVER write "nothing was lost" after a
  consolidation that dropped the inputs - inputs gone = the ability to reprocess is lost, which
  is a real loss even when every output is on disk. Burned 2026-11-02 -> bit 2026-11-05: the
  CaSSIS hunt sites were consolidated into `cassis_jay/<orbit>/` keeping cams+ctx+linescan+DEMs
  but the cubs were "distilled away" as re-fetchable, and the notes asserted "NOTHING was lost
  in the grand wipe". Days later a sweet-spot-recipe regression on those good sites needed a
  stage-6 dense rebuild, which needs the cubs, which were gone - forcing a from-scratch PSA
  re-fetch per site. Rule: keep `.cub` + camera `.json` + raw input data through EVERY
  consolidation and cleanup, on disk or on tape, full stop.
- **Archive/wipe hygiene (paper trail; see `~/projects/lfe_archive.sh`):** LOG EVERY
  archive AND every wipe in that project's own notes (what, when, and where it is
  recoverable - lfe tar path, or public source + re-fetch recipe). Per-project
  archive/cleanup scripts live IN the project dir (git-added+pushed), never loose at
  the nobackup `projects/` root; only the generic `archive_to_lfe.sh` tool sits at the
  root. Every project's data+scripts+logs live inside its own `projects/<proj>` dir -
  no stray top-level `/nobackupp19/oalexan1/<proj>` dirs. Logs are transient: after the
  archive/wipe verifies and the notes line is written, wipe the `*.log` (one literal
  path per rm) - the durable record is the notes line, not the log.
- **CLEAN UP YOUR OWN `.log` FILES WHEN DONE - do NOT leave transient logs scattered on lfe or
  pfe (Oleg, 2026-11-06: "you tend to leave .log all over").** Any `tar_*.log` / `dmget_*.log` /
  poll log a tar/stage/archive step writes is SCRATCH, not a deliverable. The moment that step
  has verified and its one-line outcome is recorded in the project notes, REMOVE the log
  (one literal absolute path per `rm`, no glob/`$VAR`). This is a mandatory finishing step of
  EVERY archive/tar/stage cycle, not an afterthought - treat a leftover `.log` the same as a
  leftover scratch file. At the end of a batch, also sweep for strays
  (`ls /u/oalexan1/*.log /u/oalexan1/projects/*.log 2>/dev/null`) and clean any you created.
- **PARENT-DIR-PREFIX packing policy (every tape tarball):** ALWAYS create the tar
  from the `projects` root with the PROJECT DIR as the leading path component, so
  `tar xf` recreates `<proj>/...`. Archive different PARTS of one project as SEPARATE
  tarballs (`<proj>_cubs.tar`, `<proj>_results.tar`, `<proj>_cameras.tar`), each named
  for its part but all containing the `<proj>/` prefix, so unpacking any or all of them
  in one local dir reassembles a single `<proj>/` tree with each part at its correct
  relative place. NEVER tar from inside a subdir (that drops the `<proj>/` prefix and
  the parts no longer reassemble). `archive_to_lfe.sh <proj> <proj>_<part> --subdir <rel>`
  does this; see [[sfs-results-verification]] / lfe_archive.sh.
- **Per-project ARCHIVE LOG (a dedicated, cross-linked notes file):** keep a timestamped
  running inventory of every tape archive in a project archive-notes file (e.g.
  `<proj>_archive_notes.sh`) cross-linked to the main project notes, with one row per
  tarball: tape path, contents, extract root, size, launch time, and a DONE+VERIFIED
  status. Verify each archive (`tar tf <tarball> | wc -l` / spot-list) and record it
  BEFORE wiping the on-disk copy. This is the trace you need when a later wipe goes wrong.
- **BEFORE a tape archive, CROSS-CHECK Mac vs pfe and rsync Mac -> pfe what belongs on tape
  (standing pre-archive step).** The Mac is authoritative for notes/lists/scripts/metadata, and
  some of it may exist ONLY on the Mac (burned 2026-11-06: sfs_south's `metadata/` with the site
  extents was Mac-only, absent on pfe, so a pfe-built tar would have missed it). So before kicking
  off the tar: (1) compare the Mac project dir with the pfe mirror, (2) rsync Mac -> pfe the text
  that should be captured - notes (`*.sh`), scripts (`*.py`), `lists/`, `metadata/`. For NOTES and
  SCRIPTS the Mac wins (plain `rsync -a`, overwrite pfe). For generated `lists/` the pfe set is the
  superset, so use `rsync -a --ignore-existing` to ADD Mac-only lists without clobbering pfe's
  newer ones. Only then build the tar, so tape gets the authoritative, complete text.
- **Also consider git add + push of the project's notes/lists/scripts (NOT data) around an
  archive** - the archive is a natural checkpoint. Add text only (one named path at a time, never a
  dir/recursive add, never a `.tif`/`.cub`/data or an HTML report), and WAIT for an explicit "push".
  Git mechanics live in [[git-repos]] / [[repo-sync]]. SENSITIVITY GATE: some projects are sensitive
  (e.g. sfs_south = Artemis south-pole landing-site work). For those, EXCLUDE the sensitive metadata
  (site extents/coordinates) from git entirely - keep it on tape + local only, never committed - and
  lock the TAPE and pfe copies so other users cannot read them (`chmod 600` the lfe `*.tar`,
  `chmod go-rwx` / `700` the pfe project dir). Lists and notes may still be git-added if the owner
  says so, but metadata with coordinates never is.
- **Staging a file off tape (`dmget`): launch it DETACHED, decoupled from the poll - burned TWICE (2026-08-20).** A large file (tens of GB) can take many minutes to stage. If `dmget` runs foreground inside a backgrounded Bash task or a poll/Monitor loop, and that task is later killed (session lifecycle, TaskStop, context compaction, a killed monitor), the `dmget` process dies with it and the file stalls half-staged at `(UNM)` - you then have to re-issue and wait all over again from scratch. RULE: on lfe, FIRST launch `dmget` as a detached `nohup ... &` writing to a log (so it survives everything), THEN poll `dmls -l` for `(DUL)`/`(REG)` in a SEPARATE task; killing the poller never touches the stage. E.g. `ssh lfe 'cd /u/oalexan1 && nohup dmget projects/foo.tgz > dmget_foo.log 2>&1 & echo pid $!'`, then in its own loop poll `dmls -l projects/foo.tgz` until it shows `(DUL)`/`(REG)`. If a poll finds it still `(UNM)` with no `dmget` running (`ps -u oalexan1 | grep -c dmget` == 0), the stage was orphaned - relaunch the detached `dmget`. (States: `OFL`/`DUL` = on tape, `UNM` = unmigrating/mid-stage, `REG`/`MIG` = on lfe disk / migrating.) READ-TRIGGERS-STAGE shortcut (often simplest): any READ of an `(OFL)` file auto-triggers DMF staging - so `scp lfe:/u/.../file.zip .` (pfx pulling to /nobackup) or starting the `unzip`/`tar xf` of the archive will itself block-then-stage, no explicit `dmget` needed. Do that pull/untar INSIDE a backgrounded Bash task (or nohup on lfe); if it dies mid-stage nothing is orphaned - just re-run the read and DMF resumes. For MANY/large files still prefer the explicit detached `dmget` above so the stage survives independently of the copy. Do NOT unzip in place on lfe (writes to tape space) - `scp` the archive to `/nobackupp19` first, then unzip there. Burned before by running `dmget` foreground in a killable task (2026-08-22): background it.
