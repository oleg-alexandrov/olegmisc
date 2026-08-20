---
name: project-workflow
description: How Oleg's projects are organized and worked - project context, the notes/paper-trail discipline (log commands, products, timestamps), resuming a project by reading notes not disk archeology, copying scripts, keeping inputs in data/ dirs with relative paths, and quick config debugging on pfe. Load when starting, resuming, or logging any project task under ~/projects.
---

## Project Context

- The StereoPipeline repository is at /home/oalexan1/projects/StereoPipeline
- The VisionWorkbench repository is at /home/oalexan1/projects/visionworkbench
- ASP stands for Ames Stereo Pipeline (refers to StereoPipeline)
- BB stands for BinaryBuilder
- BA stands for bundle_adjust (or bundle adjustment)
- The BinaryBuilder repository (`/home/oalexan1/projects/BinaryBuilder`) contains the ASP build toolset. Its `auto_build/` subdirectory has the nightly build and regression test infrastructure.
- For cutting VW point releases and keeping the conda-forge feedstock
  building (alpha → point release → repoint bot's PR branch → merge →
  restore alpha), see `~/projects/vw_conda_release.sh`.
- **ISAAC / Astrobee ISS panorama-mesh** (interesting project, worth
  revisiting): two Astrobee robots (bumble, queen), each with nav_cam +
  sci_cam + haz_cam, scanned the JEM/Kibo module from several bays,
  rotating in place. Fused into one registered, textured mesh via
  theia_sfm -> rig_calibrator -> depth fusion -> texrecon. Documented in
  ASP `docs/examples/sfm_iss.rst`. Work notes:
  `~/projects/20220608_Isaac9/isaac9_notes.sh`. The hard part is that
  panorama acquisition is rotation-only (near-zero baseline), so
  triangulation is near-degenerate. Flagged to reprocess with better
  fusion understanding, possibly without the noisy haz_cam.

## Notes & Paper Trail (CRITICAL)

Keep a per-project notes `.sh` in `~/projects/<subdir>/` and log to it as you go
- plan/approach/why up front, findings and surprises during, what worked/didn't
after. Don't rely on memory; this survives context compaction. Make work
REPRODUCIBLE (record exact commands/invocations so results can be redone) and
log the screw-ups and bad judgment too, not just the wins. Notes `.sh` are
comment-only - never `chmod +x`; `git -C ~/projects add` new files in subdirs.
Full conventions + the work-tracking file index (mpr_todo.sh, todo.sh,
ostfl_2026_notes.sh): `~/projects/notes_conventions.sh`.

**Cross-link notes files so none is an orphan.** When a project already has a
main notes `.sh` and new notes get written for a specific sub-task (a focused
experiment, a rationale, a one-off study), wire them together - suggest it or
just do it. The main notes gets a one-line POINTER to the sub-notes ("for the
distortion refit see `<name>.sh`"), and the sub-notes opens with a back-pointer
naming its parent so it is self-aware as part of a bigger picture. Same for two
peer notes that touch the same work - link both ways. The goal: from any notes
file you can navigate to the whole web, and the main notes stays the index of
what exists. A sub-notes file with no inbound or outbound link is a bug - fix it
when you notice it.

**Prompt to log done items to the progress trackers.** When a notable task
finishes - especially if it landed in `NEWS.rst` or as a PR to ISIS, ALE,
SpiceQL, USGSCSM, or other USGS repos - SUGGEST recording it in the right
progress/done log: `mpr_todo.sh` (Monthly Progress Report, all projects),
`csm_todo.sh` (CSM/ISIS work - the USGS PRs go here), `ostfl_2026_notes.sh` (OSTFL),
or `sli_fusion_todo.sh` (SLI fusion / GSFC geolocation). Just remind; don't
edit these without the user's go-ahead. These are user-facing reporting docs,
not the per-project working notes.

**Notes are the source of truth, the disk is not.** Reviews read the notes, never
re-derive from files (NO archeology) - dirs and log files get wiped, so the notes
alone must let anyone reconstruct the whole process later (wins, dead-ends, and
screw-ups alike) and condense it into a user doc. Log every script's EXACT
invocation - the qsub command, input AND output paths - and the rationale.
**TIMESTAMP everything you log - commands, results, stage START/DONE - with the
wall-clock time** (`run date`; the runner scripts already echo `START/DONE
$(date)`). Prefix note entries with the date/time. Being AWARE of how time
passes as work proceeds catches bugs: a step that finished suspiciously fast
(did it actually run, or no-op?), one that hung far too long, a job that died
minutes after submit. Without timestamps these are invisible. Run `date` when
you start a stage, when you check on it, and when you log an outcome.
**After each stage completes, record the PRODUCED OUTPUT FILES by name** (the
mosaicked DEMs, overlays, etc.), as an explicit list relative to the work dir, so
they are never re-derived or dug up later. Output files are part of the work log,
not an afterthought.

**LOG EVERY NOTABLE COMMAND AND EVERY NOTABLE PRODUCT so both can be found
later (CRITICAL).** The paper trail must let anyone re-locate what was run and
what came out, without disk archeology. Two halves:
- COMMANDS: log the exact, copy-pasteable invocation of every notable stage
  (the full command with all options and paths, the qsub line, the download
  command). This INCLUDES every plotting / figure-generation invocation - the
  exact `python <plot_script.py>` (or tool) line that produced each figure, its
  input rasters/CSVs, and the output image path - not just the compute/qsub runs.
  A notable command is any that produces or transforms a kept product, and a
  figure IS a kept product. Runs AND plots AND scripts all get their invocation
  logged; if a plot came from a script, that script must be git-tracked and named
  in the log so the figure can be rebuilt.
- PRODUCTS AND THEIR INPUTS: log them by NAME, scaled to how many there are.
  Few inputs (2 images, 1 camera, a reference DEM) - name each one explicitly.
  Many inputs (hundreds/thousands of images or cameras) - you cannot name each,
  so log the LIST FILE that enumerates them (path to the image-list / camera-list)
  plus the count. For a SINGLE notable output product (a mosaicked DEM, an aligned
  DEM, a geodiff) - name the file and its key diagnostic (e.g. the median tri-error,
  the NMAD vs reference). For MANY per-run outputs (a big run dir with countless
  per-pair sub-runs) - log the run DIR and the naming pattern, not each file. The
  litmus: months later, from the notes alone, could someone name the exact input
  images/cameras (or the list holding them), re-run the exact command, and find the
  exact output DEM and its quality number? If not, the log is incomplete. No need to note on-Mac vs on-pfe - that is figure-out-able. Each
experiment gets its OWN versioned peer dir (e.g. `dem2gcp_v7` -> `dem2gcp_transverse_v8`),
kept SEPARATE from `ref/` and `input/`, so experiments stay findable, comparable,
and wipeable. Hierarchical memory: this file is a condensed INDEX of triggers - a
task matching a pointer here is the cue to READ the deeper notes BEFORE acting.

**Healthy project layout (read at project start):** keep logic in reusable
SCRIPTS and specifics out of them (pass as args/env); keep logic OUT of notes -
notes hold only the minimal paper trail (invocation, choices, results). Three
layers: runner -> one generic launcher -> minimal notes. Full statement (cardinal
rules, layers, litmus test): `~/projects/qsub_convention.sh` section 1.

## Resuming a Project: Read and Adapt, Never Improvise (CRITICAL)

When picking up or extending an EXISTING project, the FIRST task - before
designing or writing anything - is to find and read what is already there:
the notes file(s), the precise scripts/runners, the sample and production
invocations, the qsub launch lines, the logs. These projects log nearly
everything: the exact workflow, parameters, tile sizes, node choices, gotchas.
Read and UNDERSTAND that existing workflow, then make the SMALLEST surgical
change that satisfies the request, reusing the existing scripts/invocation.
NEVER hand-roll a new parallel workflow from scratch - it wastes effort and, far
worse, produces results measured on the wrong setup, so diagnostics and numbers
have to be thrown out and redone. Only deviate where physically forced (e.g. a
node's RAM), and flag that as operational, not a recipe change. (Learned the
hard way on lunamaps SfS covariance, 2026-06: improvised a raw-`sfs` per-tile
pipeline instead of reading and adapting the existing `parallel_sfs` runner,
took several redirects to get on track, and had to redo the OOM/SBU diagnosis.)

**PREFER NOTES OVER DISK ARCHEOLOGY (CRITICAL).** When resuming, learn the
project state by READING THE LATEST NOTES - inputs, outputs, exact commands,
timestamps, the current winning result and how it was earned - NOT by digging
through whatever happens to be on disk. Disk digging is dangerous and yields
wrong, inconsistent conclusions: dirs get wiped, half-finished and REVERTED
attempts litter the tree, and file mtimes lie. The whole reason every stage logs
its inputs, outputs, commands, and timestamps is so the next session reads the
answer instead of re-deriving it - so read it. Only touch the disk to CONFIRM a
fact the notes already assert (does this named file still exist), never to
discover state the notes should have recorded. Keep the contract going: in your
OWN work, log everything (exact invocations, produced files by name, decisions,
dead-ends and reverts) as you go, for the next bot's traceability - not just for
yourself. If the notes were missing a fact you had to dig for, that is a notes
bug - fix the notes.
**When notes CONFLICT, the NEWER-TIMESTAMPED entry WINS (CRITICAL).** Notes accrete
dated entries over time and older ones get superseded but not always deleted, so a
grep can surface a stale claim and a current one side by side. NEVER act on the
first hit. When two statements disagree (e.g. "isd_generate is BROKEN for CaSSIS"
vs a later "isd_generate works"), find the LATEST-DATED statement on that exact
question and treat it as current; the older/undated one is history. Sort by date,
chase "SUPERSEDED/UPDATE" banners, and CROSS-CHECK against merged PRs and the
shipped user docs (RST) - those reflect the end state and outrank any note. This
is why every entry must be timestamped: an undated claim cannot be aged out. When
you find a stale recipe still being treated as live, MARK IT SUPERSEDED in the
notes (dated pointer to the current recipe) so it stops misleading the next bot.
(Burned 2026-07-20: old CaSSIS notes said isd_generate could not build an ISD and
needed a hand-cooked metakernel + isd_gen.py; PRs #720/#725 had since made bare
`isd_generate <cube>` work end to end, and I chased the dead recipe for several
turns before checking the merged PRs and cassis.rst.)
**THE TRIGGER (this is where the rule actually has to fire - a disposition is not
enough).** The failure is almost never "did not read notes at all"; it is hitting
a SPECIFIC factual sub-question mid-task (where does this file live? how was it
made? why does this camera/DEM have this value? what is its provenance?) and
reflexively answering it with a DISK PROBE - `find`/`ls`, inspecting a state
file, diffing files across dirs, comparing ECEF positions/timestamps, `cam_test`
- because disk feels like where precise answers live. STOP. Before ANY such probe
to answer a question about the project's OWN process, GREP THE NOTES for that fact
first. Disk is for CONFIRMING a NAMED fact the notes assert ("does file X still
exist", "is its value still Y"), NEVER for DISCOVERING/deriving process state the
notes should record. LITMUS: if you are inferring lineage, provenance, or "which
file is the real one" from timestamps, ECEF positions, distortion coefficients, or
by diffing files across directories, you are doing archeology - stop and read the
notes. And CHASE NOTE POINTERS: when a note references a deeper account ("see
~:934", "the S4 entry below", another notes file), follow it before deriving
anything from disk. (Burned on CaSSIS 2026-07-08: reverse-engineered the
refit-transverse camera lineage from ECEF positions and cam_test across stage2
dirs, when `cassis_reprocess.sh` documented the exact refit command, output path,
and cam_test result - and even had a `~:934` pointer straight to it.)
If the notes were missing a fact you had to dig for, that is a notes bug - fix it.

## Copying a Script for Custom Work: Read Both First

When making a copy of an existing script (or a new peer dir) for some custom or
one-off variant, first READ both the existing script(s) AND the destination you
are copying into. These often carry hard-won knowledge - a gotcha comment, a
tuned parameter, an env quirk, an ordering constraint - that is easy to lose if
you write the new version from scratch. Writing fresh every time silently drops
that accumulated wisdom. But do NOT imitate blindly either: understand WHY each
piece is there, keep what still applies, and drop or change what does not fit the
new task. Read, comprehend, adapt - never blank-slate, never blind copy.

## Project Data Lives in a data/ Dir, Not Run Dirs With Symlinks (CRITICAL)

Canonical project DATA (input images/cubs, reference DEMs, anything a run consumes
but does not produce) must be stored ONCE in a stable `data/` directory with honest
unique names, and every list/script must reference it THERE, directly. NEVER let a
list point at a SYMLINK ALIAS inside a RUN dir (per-run `imgs/` collections,
short-name `sl/L0.cub` aliases, etc.). Run dirs get wiped, and then the references
break even though the real data is untouched - this bit us on CaSSIS: the joint
image list pointed at `stage2/<site>_mid2/imgs/*.cub` symlinks (a run dir) instead of
the canonical `cassis_asp/data/<site>/<obsID>/.../cas_cal_sc_...cub`, so a wiped/absent
run dir showed every image MISSING. If symlink trickery is used for TEMPORARY
expediency (e.g. short names a tool wants), CORRECT it when feasible - point the lists
at the canonical `data/` path. Data in ONE place, honest names, no run-dir indirection,
no eternal per-run copies. INSPECTION/PREVIEW files count too: colorized PNGs and
geodiff/DEM copies pulled over for viewing go in the experiment's REGULAR dir
(mirror the honest pfe layout), NEVER a throw-away `eyeball`/scratch/tmp dir with
renamed copies - each experiment's outputs live in its OWN dir, wipeable as one.
(Bit us on CaSSIS: an `eyeball/` dir of renamed geodiff copies; wiped, remirrored.)

SYMLINK / PATH-REWRITE TRICKERY EACH RUN IS A SMELL: if you find yourself resolving
symlinks, or rewriting image-name paths inside a GCP / match / list file every run to make
things match, the data is NOT well organized - stop and put the slow-changing inputs (GCP,
cubs, match files) in ONE stable, honest, separate location (e.g. a `gcp/` dir in the work
dir) built ONCE, so every run references it directly with no per-run trickery. Data that
changes rarely deserves a good permanent home, not run-dir symlinks re-derived each time.
(CaSSIS 2026-07-07: the joint GCP stored `stage2/*/imgs/` symlink paths; moving the image
list to `data/` forced a 300k-line GCP path-rewrite mid-launch - exactly the smell.)

## Debug Config on pfe With a Quick Kill, Not a Full qsub Round-Trip

For fast CONFIG checks (does the GCP load? do image names match? does an option parse?) run
the tool briefly ON the pfe head node - it reaches "Loaded N GCP" / the error in seconds -
then KILL it before it starts heavy compute. Far faster than a qsub round-trip per iteration.
ALWAYS ensure the kill (Ctrl-C / kill the PID): heavy compute must NEVER linger on the head
node. Only for quick startup/config validation, never a real run.

## Relative Paths in a Project Work Dir

In a project work dir, all paths (in scripts and when presenting to the user)
must be RELATIVE to that work dir. Use absolute paths only for external files
outside it.

**ALWAYS operate FROM the work dir and keep everything relative to it (CRITICAL,
recurring).** Pick one work dir, stay in it, and write every path in scripts,
commands, and chat RELATIVE to it (`data/cub/x.cub`, `quartet_v1/ba`), never
absolute (`/Users/...`, `$HOME/projects/...`). A script assumes it is run from the
work dir and uses relative paths; the ONLY absolute paths allowed are external
tooling outside the project (conda env / ISISROOT / a reference DEM elsewhere) and
a single literal absolute path in a destructive `rm` (the safety exception). Do
NOT hardcode `$HOME/projects/<proj>/...` into project scripts. When showing a
command to the user, show it relative too. (Bit us on the Viking quartet: a BA
runner hardcoded `$HOME/projects/viking_orbiter/data/cub` instead of `data/cub`.)

**Keep slow-changing INPUTS in a `data/` dir (or similar) that OUTLIVES wiping the
outputs.** Inputs (cubs, reference DEMs, images) live in `data/`; each experiment's
outputs live in their own peer run dir that can be wiped wholesale without touching
`data/`. So a `rm -rf <run_dir>` never destroys an input, and re-running is cheap.
