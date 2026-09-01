---
name: autonomous-ops
description: Running Claude autonomously or overnight - the don't-stall rule, in-session CronCreate heartbeat plus OS-level watchdog resurrector, per-bot namespacing under ~/.claude/autorun, submit-job-then-arm-cron discipline, and dropping the cron when done. Load whenever asked to run overnight, run autonomously, monitor a long/PBS job, or set up a recurring heartbeat.
---

## Overnight / Autonomous Runs and Self-Wakeup

**Overnight / autonomous + self-wakeup (full detail: `~/projects/claude_overnight_notes.sh`):**
- DON'T STALL when told to run overnight and the parts are already logged. If the
  prior notes contain the recipe (exact scripts, invocations, params, source paths),
  KEEP GOING through the steps until done - do NOT sit in monitor mode waiting. There
  was nothing to invent; following preexisting steps is the job. "Cautiously" means
  READ CAREFULLY and follow the notes precisely, NOT stop. "Read and adapt, don't
  improvise" is satisfied BY executing the documented recipe - it is never a license
  to idle. Stop only for a real SHOW-STOPPER (a dead host, a wiped input, a genuine
  decision the notes do not answer) - and log that blocker. (Burned 2026-07-09: held
  ~7h before a fully-documented CaSSIS S2 step, calling it "risky/needs a focused
  effort" when the notes had the whole pipeline. That was idling, not caution.)
- Working alone, take initiative on simple fixes (symlink, missing lib, resubmit
  failed job); test small first; log what you did. No sweeping refactors, no
  external commits unprompted.
- DEFER / SKIP CLEANUP WHEN UNATTENDED - a permission-gated command does not fail,
  it FREEZES on the gate (CRITICAL). A destructive Bash command (`rm`, even a single
  literal absolute path; also `find -delete`, `mv` over a file, etc.) can trip the
  sandbox permission prompt, and when the user is away NOBODY is there to approve it:
  the call HANGS indefinitely, silently stalling the whole run until they return and
  click. It does not error, it does not time out - it just sits there. Confirmed
  2026-09-01: an `rm -rf <literal scratch dir>` after a test blocked and only completed
  after Oleg manually approved it; it had frozen the session. YOU CANNOT DETECT THIS
  FROM INSIDE: a call hung on the permission gate is INDISTINGUISHABLE from a slow one -
  no error, no timeout, no signal - only the user can see the stall. So there is no
  "notice it and recover"; PREVENTION is the only defense. So when running
  unattended, or ANY time storage is not the constraint (it rarely is): do NOT delete
  throwaway/scratch/temp output at all. Leave it - the scratchpad auto-cleans, disk is
  cheap, and a skipped cleanup costs nothing while a gated `rm` costs the whole run.
  Reach for a delete only when disk genuinely IS the blocker; then use one literal
  absolute path per `rm` (never a glob/`$VAR`/`cd &&`), and prefer handing the user an
  `! <command>` to run themselves over risking the gate mid-flow. Tidiness is never
  worth a stall.
- DEFAULT for ANY repeating autonomous monitoring/pipeline: reach for CronCreate
  FIRST, not ScheduleWakeup. Set up the independent recurring cron
  (off-round-marks, e.g. "9,29,49 * * * *") at the START, don't re-arm one-shots.
- THE MOMENT a qsub/PBS job (or any long remote job) is submitted, IMMEDIATELY
  CronCreate the recurring monitor in the SAME turn. Do NOT offer ("want me to set
  up a cron?") and wait for a yes - that is the exact failure that "falls asleep on
  the job": the job dies and no one is watching. Setting the cron is not optional and
  needs no permission. Submit job -> set cron -> report, always in one turn. A job
  with no watching cron is a bug.
- For any multi-stage autonomous pipeline, use an INDEPENDENT RECURRING timer that
  paces itself and PERSISTS no matter what until you explicitly kill it: CronCreate
  (recurring:true, e.g. "8,28,48 * * * *" off the round marks) whose prompt is an
  IDEMPOTENT check-and-advance (only launch a stage if its predecessor is done and it
  is not already running). It keeps firing across user messages and idle; CronDelete
  it ONLY when the work is fully done and nothing is running. Do NOT pace long
  autonomous work with single-shot ScheduleWakeup that you re-arm each turn - that is
  FRAGILE: a wakeup is one-shot and a user message supersedes it, so it silently
  lapses the moment a back-and-forth distracts you (this stalled a pipeline once).
  ScheduleWakeup is fine only for a true one-off wait. NEVER count on a task-completion
  notification (it can be missed). Interval tuned to the work: ~15-30 min for stereo/PBS.
- A ONE-SHOT BACKGROUND WAIT IS NOT A HEARTBEAT. Spawning a `run_in_background` Bash
  monitor that sleeps-then-checks-once (or any single-fire wait) to "watch a job" is
  the SAME trap as single-shot ScheduleWakeup: it fires ONCE and stops, and the long
  job it was watching keeps running with NO pulse advancing it - you fall asleep on the
  job. WHENEVER any long/unattended job is in flight, the PERSISTENT CronCreate
  heartbeat MUST be armed. Deleting the heartbeat is correct ONLY when nothing is
  running; the instant new long work launches, re-arm it in the SAME turn. Use one-shot
  background waits only as a SHORT convenience ON TOP OF an already-armed heartbeat,
  never as the pulse. (CaSSIS 2026-07-08: deleted the heartbeat when idle, then launched
  stereo jobs and leaned on run_in_background monitors - the watched job would have
  fallen asleep with no pulse advancing it. Re-arm the heartbeat immediately.)
- CREATE THE CRON ONCE, KEEP IT STABLE, NEVER CHURN IT. The cron is a LOCAL HEARTBEAT
  whose only job is to keep the session ticking so you stay awake - it is INDEPENDENT
  of what runs on remote nodes. Its prompt must be CONTENT-FREE: it points at the
  project notes for ALL changing state (which stage/job is running, which cluster,
  job IDs, next step) and says "read the notes and advance". When the work moves
  (e.g. sky_ele -> Athena, new job IDs), update the NOTES, NEVER delete-and-recreate
  the cron. Baking node/job specifics into the cron prompt is exactly what tempts a
  churn on every change. Delete the cron ONLY when absolutely, totally done.
  (Burned 2026-07-07: churned the cron on a node switch; it fired once, never
  re-fired, and the pipeline sat idle ~11h after the BA finished. The BA was
  fine - the monitor died.)
- STANDING POLICY - TWO HEARTBEAT LAYERS FOR ALL AUTONOMOUS WORK (set 2026-07-08).
  The session-only vs OS-level distinction is the crux, so respect both layers.
  For ANY unattended/auto session or long pipeline, ALWAYS arm BOTH:
  (1) IN-SESSION heartbeat = CronCreate. Pick the interval to fit the work - roughly
      every 20-40 min (tighter for fast-moving stages, looser for long jobs). Its prompt
      is content-free, points at the project notes, touches
      `~/.claude/autorun/heartbeat_<tag>` each firing, and advances the work. This is the
      normal pulse WHILE the harness is alive.
  (2) OS-LEVEL cron = emergency resurrector, on the local machine(s). This is the layer
      that survives an OUTAGE. It relaunches `claude -c -p` only when the heartbeat file
      is stale (harness presumed dead), else stands down; atomic-lock guarded so runs
      never overlap; self-heals across a still-down service (cron keeps re-firing and
      catches the moment it returns).
  WHY BOTH (the thing I got wrong before): CronCreate is SESSION-ONLY - it lives inside
  the running Claude session and DIES WITH IT, so a "service unavailable" outage that
  kills the harness ALSO kills the CronCreate heartbeat and nothing re-arms it. Only an
  OS-level cron, independent of the harness, can bring Claude back. The old blanket "no
  OS-level crontab" rule predated this understanding and is RETIRED. OS cron is now
  REQUIRED for durable auto work, on LOCAL machines only, NEVER on pfe.
  PER-BOT NAMESPACING (REQUIRED - a single shared watchdog/heartbeat/sentinel is LOSSY
  with 2+ concurrent auto bots: a survivor keeps the shared heartbeat fresh so a dead bot
  is never resurrected, and the first `.auto_done` disarms everyone). So EACH concurrent
  auto bot gets its OWN fully independent set, tagged by a short name `<tag>`. ALL of the
  apparatus files live UNDER `~/.claude/autorun/`, NEVER loose in the home dir (`~`) or
  `~/bin` - the home dir stays clean. Make it once (`mkdir -p ~/.claude/autorun`). The set:
    - heartbeat  `~/.claude/autorun/heartbeat_<tag>`   (the bot touches ONLY this, every turn)
    - watchdog   `~/.claude/autorun/watchdog_<tag>.sh`
    - lock       `~/.claude/autorun/watchdog_<tag>.lockdir`   (own lock - watchdogs never collide)
    - log        `~/.claude/autorun/watchdog_<tag>.log`
    - sentinel   `<project>/.auto_done_<tag>`   (in the PROJECT dir, not home; disarms ONLY this bot)
    - crontab    its own line at STAGGERED minutes (e.g. "11,26,41,56" vs another's "9,24,39,54")
  Each watchdog checks ONLY its own heartbeat and resurrects ONLY its own session, by
  `cd`-ing into that bot's PROJECT DIR before `claude -c -p "<resume prompt>"` so `-c`
  grabs the right session - different bots MUST run in different project dirs (else use
  explicit session IDs). A bot touches ONLY its own heartbeat and disarms ONLY its own
  sentinel; it NEVER touches another bot's files. The same tagging applies to any l1
  backup watchdog (`~/.claude/autorun/watchdog_<tag>_l1.sh`, sshes mac_arm). Retire a bot's
  watchdog when ITS work is done: touch that bot's `<project>/.auto_done_<tag>` (and drop
  its crontab line) AND remove that bot's files under `~/.claude/autorun/`. Example: the
  Olympus CTX-pair bot = `~/.claude/autorun/heartbeat_ctxpairs` +
  `~/.claude/autorun/watchdog_ctxpairs.sh` (crontab "11,26,41,56") + project
  cassis_olympus_mons + sentinel `cassis_olympus_mons/.auto_done_ctxpairs`. Never let any
  heartbeat/watchdog/lock/log file sit loose in `~` or `~/bin` - they all belong under
  `~/.claude/autorun/`. Detail: `~/projects/claude_overnight_notes.sh`.
- MUST DROP THE OS-LEVEL CRON (and the in-session CronCreate heartbeat) THE MOMENT ALL
  WORK IS FULLY DONE. The OS cron exists ONLY as a safeguard to resurrect the session if
  it DIES MID-WORK. Once the work is complete there is nothing left to resurrect or
  advance, so a still-armed cron just cycles for no good reason (and can pointlessly
  relaunch a finished session). Dropping it is the FINAL action of any auto job: remove
  the crontab line(s) / touch the `.auto_done` sentinel AND CronDelete the in-session
  heartbeat. Arm the cron for the duration of the work, drop it when done - never leave it
  idling past completion.
- On every wakeup, FIRST run `date` to re-orient - long runs leave you stale.

## Notes Discipline + Reread-On-Resurrection (MANDATORY for any auto/overnight run)

The durable NOTES FILE (in `~/projects/<proj>/`), not this chat context, is the single
source of truth for an autonomous run. Context gets COMPACTED and the session can be
KILLED and RESURRECTED by the OS watchdog - both wipe your working memory. The notes
survive; treat them as the handoff to your future self.
- **On EVERY resurrection (cron said "came back from dead") AND every time you notice a
  compaction/summary just happened: STOP and re-bootstrap before doing anything.** In
  order: (1) `date`; (2) re-read the project notes file top-to-bottom, especially the
  WORK LOG (newest entry last) and the PLAN; (3) re-read the parent/hub notes + any
  status tracker + the relevant skill; (4) check which PRODUCTS already exist on disk
  before redoing a step (a half-finished step is common); (5) resume from the last
  logged step. Never resume from stale chat memory - it may be a different reality than
  the notes.
- **Put a RESURRECTION/COMPACTION POLICY header at the TOP of the project notes' WORK
  LOG** spelling out the reread order above, so a cold-started session self-orients.
- **Log COPIOUSLY and IMMEDIATELY, as you go, never batched at the end** (the end may
  never come - you might die first). Each meaningful step gets a timestamped WORK LOG
  entry with: the exact command run, the product path it wrote, the key stats/metrics,
  what you EYEBALLED (and the PNG path), any gotcha learned, and a one-line "next step".
  Also record inferences, dead ends (so you don't repeat them), and pointers to docs/code
  (`file:line`). Density beats polish - a future cold session must be able to continue
  from the log alone.
- **The cron/watchdog resume prompt must be CONTENT-FREE**: it points at the notes and
  says "re-read and advance", never bakes in changing state (job IDs, current stage).
  All state lives in the notes; you update the notes, never churn the cron.
- **Commit the notes early and often** (`git -C ~/projects add <file> && commit`), so the
  paper trail is safe even if the disk copy is lost. Push only per the repo's push rules.

