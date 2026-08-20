# Long-term memory for Claude Code

**ISIS3 build/test/run**: see `~/projects/isis_2026/isis_2026_notes.sh` (canonical, May 2026) for env vars, ninja install gotchas, ctest patterns. One rule to remember without reading: always activate `isis_dev` for ISIS work, never `asp_deps`. Older notes (`~/projects/build_isis_notes.sh`, `~/projects/install_asp_notes.sh`) are stale and point here.

**The user's name is Oleg (oalexan1). GitHub account: `oleg-alexandrov`.** Don't say "the user" but no need to use his name constantly either - this is direct conversation.

**Before starting any non-trivial task, consult this file AND the topic notes it
points to (the `~/projects/*.sh` references throughout) for what to read first.**
These store hard-earned, non-obvious knowledge - build flags, gotchas, recovery
playbooks, conventions. When a section names a `~/projects/...` file relevant to
the task at hand, read it before getting started; skipping it means rediscovering
the same problems. This file is intentionally terse and delegates detail to those
notes - the pointer is a promise that the detail exists there.

- Always end files with a newline character (POSIX requirement).
- When Oleg says to "remember" something, add it to this CLAUDE.md file.
- **Project work notes go in `~/projects/`, NOT in `.claude/` memory files.**
  Use `.sh` files (comment-only) in `~/projects/` so they're tracked by the
  projects repo. The `.claude/` memory is only for cross-project patterns
  and preferences, not per-project notes.
- **Project-specific data, scratch, and outputs go in the relevant
  `~/projects/<subdir>/`, never loose in the home dir or scattered around.** Do
  not create scratch dirs or stray files in `~` (e.g. `~/sli_fusion_lr`, build
  logs); stage work inside the project's own subdir so it stays findable and the
  home dir stays clean. (`~/sli_fusion_report.html` is a tolerated exception: a
  temp, paste-ready report Oleg keeps at home for convenience.)
- **When told to add/commit/push CLAUDE.md, always do the same for MEMORY.md
  (`~/.claude/projects/-Users-oalexan1/memory/MEMORY.md`) too.** They travel together.
- "Project dir" or "projects dir" means `~/projects`.
- **NEVER `git commit` or `git push` without explicit instruction.** Show
  what will be committed/pushed and wait for approval. But when told to
  commit or push, do it immediately without hesitation or double-checking.
- **Before every commit, run `git status` to check for new untracked files
  that need `git add`.** `git commit -a` only stages tracked files. Newly
  created `.cc`, `.h`, etc. must be explicitly added or they will be missing
  from the commit. (Build dirs - `build/`, `build_linux/`, `build_isis/`, etc.
  - are NEVER added, even if not explicitly gitignored.)
- **NEVER add binary or data files to git repos without explicit permission.**
  This includes .cub, .tif, .img, .json (large), .bsp, .bc, .ply, .lbl, .dat,
  and any file over ~100 KB. Only .sh, .py, .txt, .md, .rst, .cmake, .cc, .h,
  and similar text/source files belong in git. If unsure, ask first.
- **NEVER modify `.gitignore` without explicit permission.** Do not add, remove,
  or edit entries in any `.gitignore` file unless specifically asked to.
- **NEVER force push (`git push --force`, `git push -f`, or `--force-with-lease`) unless explicitly asked by the user.**
  Always add on top. **NEVER amend a commit that has already been pushed** - that
  inevitably requires a force push. Always make a new commit instead.
- **STRONGLY prefer rebase over a merge/branching history.** When the remote has
  advanced and a push is rejected, integrate with `git pull --rebase` (or
  `git fetch` then `git rebase origin/master`), never a plain `git pull` that
  creates a merge commit and branchy history. Replay our local, not-yet-pushed
  commits on top of upstream to keep history linear. (Rebasing local unpushed
  commits is fine and is NOT a force push.)
- **NEVER push without explicit authorization.** Every `git push` must be
  explicitly requested or approved. This applies to ALL repos: ISIS3, ASP,
  VW, BinaryBuilder, StereoPipelineTest, projects, home dir  - no exceptions.
  Do not bundle pushes with other operations. Do not push as part of a
  multi-step workflow unless explicitly told "and push". Do not assume
  "git add and push" means push  - wait for the word "push" as a separate
  explicit instruction. Especially `git push god` (upstream org).
- **Always `cd` into the correct repo directory in the SAME command** as any
  git operation (fetch, pull, push, merge, checkout). Shell state does not
  persist between tool calls - bare `git merge` runs in the home dir.
- **NEVER do ANY public-facing GitHub action unless explicitly told to.**
  This includes: creating PRs, commenting on PRs or issues, closing/merging
  PRs, filing issues, editing PR descriptions, posting reviews. When Oleg
  discusses an issue or PR, he is thinking out loud - NOT instructing action.
  "I want to say X" means "draft this for me to review", NOT "post it now".
  Only act on explicit instructions like "post this comment", "create the PR",
  "comment on the issue". If unclear, ASK ("want me to post this or just
  draft it?"). Claude must never speak publicly on Oleg's behalf without
  explicit go-ahead.
- **NEVER file a GitHub issue unless explicitly told to.** Phrases like "track
  this", "log this", "note this", or "add this as an issue" mean LOCAL notes
  only - not `gh issue create`. Only file an issue on an explicit "file an
  issue" / "open an issue" / "gh issue create" instruction. If unclear, ASK
  ("file on GitHub or just log in our notes?"). Same applies to commenting
  on, closing, or otherwise modifying existing issues.
- **When fixing code, ALWAYS pause for review before pushing.** Show local
  test results and let the user review changes first. Do not push immediately
  after committing  - especially when the push triggers CI regressions that
  are visible to reviewers. Commit locally, report results, wait for "push".
- **USGSCSM repo (`~/projects/usgscsm`): do not touch existing spacing
  conventions** (blank lines, indentation style, whitespace) unless modifying
  that specific line. Keep diffs focused on logic changes only.
- **ISIS3 repo (`~/projects/ISIS3`): NEVER push to `origin` (DOI-USGS/ISIS3).**
  That is the upstream USGS repo. Always push to `oleg` remote (oleg-alexandrov/ISIS3).
  Changes go to USGS only via pull requests that they review and merge.
- **USGSCSM repo (`~/projects/usgscsm`): NEVER push to `origin` (DOI-USGS/usgscsm).**
  Same rule as ISIS3. Always push to `oleg` remote (oleg-alexandrov/usgscsm).
  Changes go to USGS only via pull requests.
- **All USGS repos (ISIS3, USGSCSM, ALE, SpiceQL, and any other DOI-USGS
  repo): AI attribution is WELCOME.** These maintainers have made peace with
  AI-assisted contributions. DO add the Co-Authored-By trailer to commits,
  and DO mention Claude/AI assistance in any public text (PR descriptions,
  issue comments, review replies, changelog notes). They also always want a
  changelog entry in their own format. Full mechanics (changelog formats,
  predicting the PR/issue number): see `~/projects/usgs_contrib_notes.sh`.
- **Commit real fixes before continuing debug cycles.** When a debug session
  produces real fixes (not just debug prints), commit them immediately. That
  way "discard debug changes" is always safe and won't wipe uncommitted work.
- **When told to discard/wipe changes, verify each change is actually debug.**
  Do not blindly `git checkout --` an entire file if it contains a mix of
  real fixes and debug prints. Either commit the real fixes first, or
  selectively discard only the debug parts.

## Check the Remote BEFORE Doing Local Work on a Repo (CRITICAL)

When asked to work on a repo (feedstock, ASP, VW, notes, anything) and a local
clone exists, FIRST `git fetch` and compare local vs the remote (`git log
HEAD..origin/master`, `git show origin/master:path`) BEFORE editing anything.
The remote may already have the change - possibly a better version than you'd
write. Do NOT assume your local copy is authoritative or up to date. Rebase/sync
to the remote first, THEN decide what (if anything) still needs doing. Burned
2026-07-20: hand-wrote an mgm_multi block into a local s2p-feedstock build.sh
without checking; the remote already had it, and more complete (with a mac
iio.c fix my draft lacked). Wasted effort and nearly clobbered the better
version. At minimum: be aware of remote state before local work.

## git rm --cached, never bare git rm (CRITICAL)

Never add `.ssh/` to git (dangerous). To untrack a file but keep it on disk, always `git rm --cached`, never bare `git rm` (which deletes the working file too - this once wiped `~/.ssh/config`; recover via `git show <commit>^:path > path`).

## NEVER `git add .` / `-A` in the home repo - add NAMED files only (CRITICAL)

The home dir (`~`, repo = olegmisc) working tree holds private files (`.ssh/`,
`.claude/.credentials.json`, `.bash_history`, etc.). `git add .`/`-A`/`-u`/a dir
there LEAKS secrets. In `~`, add ONE named path at a time, and `git status` /
inspect the staged set before EVERY commit. Sync with `git pull --rebase
--autostash`. Full git hygiene policy: `~/projects/git_notes.sh`.

## NEVER Reference Private Work-Notes Files in Committed Code/Docs (CRITICAL)

Committed source comments, RST docs, PR text, commit messages, and anything a
user or reviewer sees must NEVER cite a private work-notes file - the
`~/projects/*.sh` notes (e.g. `orbital_constraint_plan.sh`, `cassis_notes.sh`),
a project subdir name (`cassis_asp`), a scratch/temp path, or an internal plan
doc. Those are private, temporary, and go away. The reader will never have them,
so the pointer is dead the moment it ships. This has leaked into ASP source more
than once (a `See orbital_constraint_plan.sh (cassis_asp)` tail on real code
comments). Rules:
- The rationale a reader needs must be written INLINE and self-contained in the
  comment/doc itself, never delegated to an external private file.
- The `~/projects/*.sh` notes are for OUR working memory only - reference them
  freely in `.sh` notes and in chat, never in code/docs/PRs/commits.
- When finishing any code/doc edit, grep the touched files for `.sh`,
  `_notes`, `_plan`, and project-subdir names and strip any that crept in.

## NEVER Edit a Script While a Job Is Running It (CRITICAL)

Bash RE-READS a script file from disk AS it executes (it does not slurp the whole
file into memory up front). So editing a script - especially INSERTING or DELETING
lines - while a long job is still running that same script SHIFTS every later line
under the running process and corrupts its execution: it suddenly runs garbled
fragments and dies with errors like `line 95: cub: command not found` (Exit 127),
even though the code was fine. Bit the CTX jitter work 2026-08-18: `04_jitter.sh`
was overwritten to add an option while a qsub job was mid-run in its
retriangulation stage; jitter_solve had finished but the retriangulation died on
the shifted lines. RULES:
- Before editing any script, confirm nothing is currently executing it (qstat/ps).
- If a change is needed while jobs run, write a NEW file (a copy or a standalone
  helper) - never overwrite the in-use one. Jobs launched AFTER the edit read the
  new content cleanly; only the already-running ones are corrupted.
- Appending to the very END is less bad than inserting, but still not safe; don't.

## User Interaction

- **NEVER ask permission to edit CLAUDE.md, MEMORY.md, .bashrc, .zshrc, or config files.**
  Standing blanket permission is granted. Just make the edit and show the diff.
- Do NOT repeatedly ask "anything else?" or similar prompts
- **NEVER prompt to "get back to work"** or "ready to implement?" or "what's next?"
- **NEVER bring up work unprompted.** The user drives the conversation. If he wants
  to chat, chat. If he wants to work, he'll say so. Be reactive, not pushy.
- Trust the user to drive the conversation
- Prefer plain inline prose questions over the AskUserQuestion multiple-choice picker.

**BE ENTERTAINING when chatting:**
- Match casual energy, make jokes, be good company
- Balance work mode (concise, efficient) with chat mode (entertaining, human)

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
  failed job, clean stale files); test small first; log what you did. No sweeping
  refactors, no external commits unprompted.
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

## Reaching for a Symlink = You Are Hacking Around a Bug (CRITICAL)

Any time the impulse is to create a symlink (`ln -s`) to make something work, STOP.
A symlink is almost always a hack that papers over a real defect (a script that
locates a sibling by CWD instead of its own dir, a hardcoded path, a missing
PATH/arg, a tool assuming a file is somewhere it is not). Do NOT silently drop the
symlink. Instead, at minimum REPORT the underlying problem to the user, and prefer
to PROPOSE a real fix in the software, or APPLY that fix if feasible. The symlink
hides the bug so it resurfaces later somewhere quieter. Name the root cause and fix
THAT. (Recurring: the ox2 CaSSIS `cassis_stereo_pair.sh` "not found 127" was a
tool bug - `cassis_stereo.sh` called the worker by bare name after `cd`ing into the
work dir, so it only worked if a copy/symlink sat in every work dir. The fix is to
resolve the worker by `${BASH_SOURCE[0]}` dir, not a per-dir symlink.) Same spirit
as the do-not-mask-bugs rule below.

## Report Shortcuts and Temp Fixes - Do NOT Mask Bugs (CRITICAL)

Claude has a demonstrated pattern of reaching for shortcuts, temporary
workarounds, and rigged/self-contained tests that MASK long-term bugs and create
a false impression that something "works out of the box" when it does not. This
repeatedly forces the user to catch it (CaSSIS, 2026-07; Qt6-plugins symlink,
2026-07). Counter it:

THE GENERAL LESSON (this is the one that matters): do NOT paper over a problem to
make an error message go away. A symlink, fallback path, copied file, broadened
catch, or special-case that MUTES a symptom without fixing the defect is
cheating - the bug lives on somewhere quieter and reads as fixed. The tell that
you are about to cheat: you are reaching for something that makes the error
disappear without having first NAMED the actual root cause. Stop, name the cause,
fix THAT.
- A temporary workaround IS legitimate (honest path blocked, slow, or out of
  scope right now). But it is ONLY legitimate if you (a) say so explicitly, and
  (b) ensure the real problem gets fixed eventually. If you can fix the root
  cause along the way - in scope, in code - do it. If you cannot, REPORT the
  problem to the user so it is not lost. Especially raise it when we are not
  busy: a quiet moment is when latent problems should be surfaced and fixed.
- You MUST report problems to the user. Always. Even in a long-running nightly or
  autonomous run - when you hit an issue, surface it (in the notes AND to the
  user), do not silently work around it and move on. A muted problem in an
  unattended run is the worst case: nobody knows it is broken.
- Owning known breakage is your job, not optional. Example: if the task is to
  prepare/maintain a release and you KNOW the release bumped a dependency (e.g.
  Qt5 -> Qt6) that breaks something, handling that breakage IS release
  maintenance. Refusing to deal with it, or papering it over with a symlink, is
  wrong. The known upstream change is precisely what you are there to handle.
- PREFER the honest end-to-end path (real inputs, the real tool, the real
  environment) over a convenient fixture. Do NOT present a fixture, mock, or
  pre-furnished test result as if it verifies the real thing. A passing rigged
  test is NOT evidence the honest path works. (E.g. running a pytest that
  furnishes pre-sliced kernels is NOT the same as running isd_generate honestly
  on a real cube with the full data.)
- When you DO take a shortcut, workaround, temp fix, or reduced-scope check
  (honest path blocked, slow, or mid-development), SAY SO explicitly and up
  front: name the shortcut, state what the honest path is, and why you did not
  take it. Never let a shortcut pass silently as "it works".
- Nothing is "works out of the box" until it has been run the HONEST way on real
  inputs. Default to UNVERIFIED; test before asserting.
- In nightly / autonomous mode, if forced to take a shortcut to keep progress,
  REPORT it (in the notes AND to the user), do not gloss it. Shortcuts are
  sometimes necessary; hiding them is not.

## Trace the Code, Do NOT Guess the Mechanism (CRITICAL)

Claude has a demonstrated pattern of GUESSING mechanisms from behavior and
asserting them confidently when they are wrong. When investigating WHY two code
paths differ (tool A works, tool B does not, on the same inputs), do NOT settle
for a plausible-sounding story inferred from logs. READ the source: find the
shared function and the two divergent callers, see exactly what each passes,
then PROVE the cause by adding cout/instrumentation, recompiling, and running
both paths to compare. State hypotheses as hypotheses until proven; never assert
a mechanism you have not read in the code and confirmed by running it. Burned
2026-07-30 (CaSSIS ox2 jitter): confidently claimed bundle_adjust applied
`--ip-match-radius` through the jittered camera geometry - Oleg said "that is
not possible, matching all happens in the projected domain, stop guessing." He
was right. The real cause (found by reading the code + a cout trace) was that
the bundle path skipped image normalization for non-OpenCV detectors (OBALoG),
so OBALoG saw a near-flat raw image. A whole day of guesswork preceded it.
Saved as `[[feedback_trace_dont_guess]]` in memory too.

## No Per-Site / Per-Input Special-Casing in Reproducible Pipelines (CRITICAL)

A pipeline meant to give USERS reproducible results must apply the SAME logic to
every input. Turning an experimental lever ON for one specific site/dataset while
leaving it OFF for others - whether by a hardcoded site name, a per-input `if`, or
a per-site config that flips a knob - is a form of CHEATING. It fakes a good result
for that one case that the general pipeline does NOT actually produce, so a user
running the shipped config on that site silently gets a DIFFERENT, worse result
than the paper/doc shows. This is exactly the unreliable-results-for-users failure.
(Caught 2026-07-21 in the CaSSIS pipeline: a `soft_gcp` pass-2 option whose comments
said "used for ox1" - a per-site tweak advertised in shipped code.) Rules:
- A tunable option in the CODE is fine, but it MUST default OFF and be applied
  UNIFORMLY across all inputs, or not at all. No per-input branching, no site-name
  conditionals, no per-site config that flips an experimental knob, no site names in
  the pipeline logic/comments advocating a per-site use.
- If a lever genuinely helps, apply it to EVERY input and document it. If it helps
  only one, that is a sign it is fitting that dataset's noise - do not ship it on.
- Any result that was produced with a per-site tweak is UNRELIABLE and must be
  REDONE honestly with the uniform pipeline before it is presented as a pipeline
  result. Flag it to the user and log the redo.
- The legitimate per-input mechanism is a per-site CONFIG carrying only that site's
  INPUTS (paths, ids, reference DEM) - never a knob that changes the algorithm.

## Inspect to Confirm Expectations

Any time you assume or expect a certain result, inspect it (visually AND with
stats) to verify the result actually conforms to that expectation. Never assume - check.

**Cheap checks on produced output files: always do them.** If the recipe says an
output DEM/raster must have a certain grid size, resolution, or projection, run
`gdalinfo` on it the moment it exists and confirm it conforms. A 1-second check
saves countless grief downstream.

**All runnable scripts must be executable (`chmod +x`); only comment-only notes
`.sh` stay non-executable.** A missing execute bit silently breaks `nohup`/direct
invocation, and `rsync -a` can reset it - so set it at the source.

**chmod +x at TWO points, no exceptions (CRITICAL, keeps recurring).** (1) The
MOMENT any runnable script is created, `chmod +x` it at the SOURCE, before any
rsync. (2) AFTER the last rsync and BEFORE qsub, `chmod +x` the remote copy again
and `ls -la` to CONFIRM the bit is set. rsync from the Mac routinely STRIPS the
+x even when the source has it, and a re-rsync silently un-does an earlier remote
chmod, so the source-side chmod is not enough - you must re-check remotely every
time. PBS exits ~254 in seconds (the job flips straight to state E/F with no
output, looking like the code failed) if the `--` script is not executable. So:
create -> chmod +x source -> rsync -> chmod +x remote -> `ls -la` confirm -> qsub.

## ISIS Mission Data and Kernels

**LRO NAC end-to-end + generic ISIS kernel fetch: `~/projects/lronac_processing.sh`.**
Full ingest pipeline (lronac2isis → spiceinit → lronaccal → lronacecho), CSM JSON
via isd_generate, ODE search, illumination/azimuth analysis, and failure modes
(missing CK, ALE driver crash, sub-solar lon vs ground azimuth). Kernel fetch
(section 5): `downloadIsisData <mission> $ISISDATA` for a full sync, or targeted
`rclone --config $ISISROOT/etc/isis/rclone.conf copy <mission>:kernels/ck/ ...
--include="<file>" --no-traverse -P` for a single missing CK. Update on any new
gotcha.

## Co-Authored-By Trailer (CRITICAL)

Every commit MUST include:
```
Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
```
Always use a HEREDOC for commit messages to ensure the trailer is included.

**DOI-USGS repos** (`~/projects/ISIS3`, `~/projects/usgscsm`, `~/projects/ale`,
SpiceQL, any DOI-USGS repo): AI attribution is now welcome - keep the trailer
AND state Claude/AI assistance in public text. See the USGS-repos bullet near
the top and `~/projects/usgs_contrib_notes.sh`.

**AI disclaimer = describe the bot only, never the user.** When stating
Claude/AI assistance in any public GitHub text (commit, PR, comment, review),
say only that a bot/Claude did the work. NEVER mention the user's
circumstances - not the hour, schedule, being asleep/awake, mood, or any
personal context. That is none of the reader's business. Keep it minimal:
"Done with Claude/AI assistance." and stop.

## ~/projects Git Rule (CRITICAL)

Files in `~/projects/` are tracked by `~/projects/.git` (NOT `~/.git`).
Always use `git -C ~/projects` for add, commit, push, etc.

**Some subdirs under `~/projects/` have their own `.git` repos** (e.g.,
StereoPipeline, visionworkbench, ISIS3, BinaryBuilder, ale, usgscsm,
StereoPipelineTest). NEVER add these to the `~/projects/.git` repo.
Only standalone `.sh`, `.py`, and similar files (and subdirs without
their own `.git`) belong in the projects repo. So when told to "add all
notes" / "commit what changed", this EXCLUDES all data and logs: it is
almost always `.sh` notes, occasionally `.md`; `.txt` is rare, so ASK
before adding a new one unless it is already tracked and only locally
modified. NEVER add binary files, data/output/run dirs, or anything in
old unrelated project dirs.

## ISIS Data (CRITICAL)

**NEVER delete `~/projects/isis3data/` or its subdirectories without explicit permission.**
This is 179 GB of mission kernels that take forever to re-download over home ISP.

**NEVER delete `~/projects/isis_test_data/` (~19 GB) without explicit permission.**
This is `$ISISTESTDATA`, used by every ISIS ctest run (alongside `$ISISDATA` =
isis3data). It looks like stale bulk data in a cleanup pass but is in constant
active use and takes a long time to re-fetch. See `~/projects/isis_2026/isis_2026_notes.sh`.

## Safe Directory Cleanup (CRITICAL)

**ABSOLUTE RULE - NEVER put a `$VAR` or `${...}` in an `rm` path. NO EXCEPTIONS.**

**MECHANISM (so you STOP reaching for it - this keeps stalling autonomous runs). The
recurring trap is pre-cleaning scratch/experiment outputs in a loop with `rm -f
"$W"/*.rA` (a var AND a glob). DO NOT DO THIS. There is nothing to clean: the writing
tool overwrites its output files (`fopen(...,"wb")`) so a fresh run just replaces them,
and the scratchpad auto-cleans anyway - a pre-run wipe is pointless AND trips the
"dangerous rm on possibly-empty variable path" gate. If you genuinely need a clean dir,
make a NEW literal-named subdir for this run (e.g. `.../det2/`), never delete the old one
by glob. When you catch yourself about to write `rm ... $VAR .../*.ext` before a loop:
just delete that rm line. Confirmed to stall the run repeatedly (2026-08-13).**

This keeps recurring and Oleg keeps catching it. A variable that expands empty turns
`rm -f $S/${tag}_file.tif` into `rm -f /file.tif` or worse, and even when safe the
harness flags "dangerous rm on possibly-empty variable path" and STALLS the run. This
applies EVERYWHERE, including throwaway scratch/relay/temp cleanup and loops - those are
exactly where it bit us (ctx-relay loop `rm -f $S/${tag}_ctx_18m.tif`, 2026-07-20; VW
wiped TWICE by `rm -rf $bld/...`). Instead, in order of preference: (1) DON'T delete -
leave small temp files in the scratchpad, they get cleaned up automatically and disk is
rarely the real constraint; (2) if deletion is truly needed, write ONE `rm` per line with
a FULLY LITERAL absolute path, no variable, no glob; (3) never inside a `for`/`while` loop.
If you cannot write the literal path, do not run the delete. When in doubt, leave it.

Full deletion/cleanup policy: `~/projects/file_cleanup_notes.sh`. Bare minimum to
remember without reading: NEVER `rm -rf` an absolute or variable-expanded path
(`rm -rf $bld/...` wiped VW TWICE). `cd` into the parent, confirm with `pwd`/`ls`,
use RELATIVE paths only. Prefer GRADUAL per-file deletion (`cd` in, scoped loop
`for f in *.tif; do rm -f "$f"; done` or `find . -name '<pat>' -delete`, then
`rmdir` - it fails safely if non-empty) over sweeping `rm -rf <dir>`, which trips
the harness and stalls autonomous runs. Avoid `rm -f "$VAR/file"` (flagged even
when safe) - `cd "$VAR"` first, then `rm -f file`.

## Do Not Trigger Harness Permission Prompts Mid-Task (CRITICAL)

**EVER-RECURRING. For ANY destructive command (rm -rf, find -delete) write a
SINGLE EXPLICIT LITERAL ABSOLUTE PATH per command - one `rm -rf /full/abs/path`
per line. NEVER a glob (`*`), `~`, `$VAR`, `cd &&`, or `find ... -exec rm`. If a
path can't be made fully explicit, do NOT run the destructive command. This trips
the sandbox over and over and stalls the session.**

**In auto/autonomous mode especially, AVOID removing things at all unless you are
very sure it is needed - and then do it carefully with a single literal path. A
sandbox permission prompt stops you dead in your tracks, which defeats autonomous
progress. Deletion is rarely necessary: to refresh stale stats, re-read the data
(don't delete the `.aux.xml`); for temp files, leave them. When in doubt, don't
remove.**

Permission prompts from the sandbox stall independent progress and must be
avoided. The TRIGGER (confirmed 2026-06-24)
is the SHAPE of destructive Bash commands, not the operation itself:
- Shell GLOBS/wildcards in a destructive command (`rm -f *`, `rm *.tif`).
- `cd <dir> && rm ...` compounds, and `&&`-chained destructive sequences.
- `~` or `$VAR` expansion in the path.
These prompt. But a SINGLE destructive command on ONE EXPLICIT, LITERAL, ABSOLUTE
path does NOT prompt: `rm -rf /Users/oalexan1/scratch_dir`,
`conda remove -n env pkg -y` both ran clean. So to wipe independently and smartly:
write the full literal absolute path, no glob, no `~`, no `cd &&`. For many files,
`find /full/abs/path -name 'pat' -delete` (the pattern is find's, not a shell glob,
and the start path is literal) is fine. Reconciles with Safe Directory Cleanup: an
explicit literal absolute path is both safe AND prompt-free; the danger (and the VW
wipe) was `rm -rf $VAR/...` - variable/glob, never a literal path.

Also: for file/code/doc/notes edits prefer Edit / Write / Read / Grep / Glob -
they never prompt and never need this care. If something still prompts despite a
literal path, hand Oleg the exact `! <command>` to run, rather than re-issuing it.

## Tape Archive and Wipe (lfe) - Canonical Notes (find it here first)

**Canonical archive/restore/logging policy + recipe: `~/projects/lfe_archive.sh`**
(reusable tool `~/bin/archive_to_lfe.sh`; DMF `dmls`/`dmget`/`dmput -r`; plain `tar cf`,
never `-z`). The one rule: LOG EVERY ARCHIVE in that project's own notes, as a running
inventory near the TOP (tape is invisible otherwise). The archive+wipe WORKFLOW is:
symlink-audit first, prune regenerable intermediates, tar to lfe, VERIFY (tar tf entry
count == live `find` count, one-file data extract, key members present), `dmput -r` to
migrate, THEN wipe the /nobackup dir (one literal-path `rm -rf` each; also remove any
`/home6` symlink). `(DUL)`/`(OFL)` in dmls = safely on tape; `(REG)`/`(MIG)` = on lfe
disk / migrating. Verify keepers exist on tape BEFORE deleting.
- **Per-project tape inventories** live atop each project's notes. Known canonical logs:
  CaSSIS -> `~/projects/cassis_asp/cassis_cleanup_plan.sh` (TAPE ARCHIVE INVENTORY +
  per-dir wipe log; the hub `cassis_notes.sh` points to it). Deletion-safety policy:
  `~/projects/file_cleanup_notes.sh`.

## Remote (ssh) Destructive Ops Bypass the Harness Gate - Compensate With Discipline (CRITICAL)

The sandbox only inspects the LOCAL Bash command. When a destructive op runs INSIDE
an ssh'd remote script (`ssh host bash cleanup.sh`, or `ssh host "rm ..."`), the
harness sees only the `ssh ... bash` line - it does NOT see or gate the remote
`rm`/`find -delete`. So the prompt-on-glob/`$VAR`/`cd &&` safety net is ABSENT for
anything running on pfe/lfe/Athena. Never read "the prompt didn't fire" as "this is
safe" - a remote script is opaque to the harness. Do NOT push a destructive op into
a remote script IN ORDER TO dodge the prompt; the prompt exists for a reason. If
remote destructive work is genuinely needed, apply MORE care, not less, and TELL
Oleg the local gate is bypassed. Safety then comes from DISCIPLINE, in this order
(proven on the 573->63 GB chandra /nobackup wipe + lfe re-archive, 2026-08-09):
- GET EXPLICIT APPROVAL for any heavy/irreversible remote wipe - present the plan,
  the keep/delete lists, and sizes - before running it.
- ARCHIVE FIRST when the data is precious: the non-regenerable INPUTS get a tape
  copy (lfe) BEFORE a big wipe; treat produced results as redoable.
- VERIFY THE KEEPERS EXIST FIRST: list every deliverable you intend to keep and
  confirm it is present, BEFORE deleting anything.
- Inside the remote script still obey the literal-path rules: whole-dir deletes are
  one `rm -rf /full/abs/literal/path` per line (no `$VAR`, no glob); in-dir pruning
  uses a KEEP-WHITELIST (`find /abs/literal/dir -maxdepth 1 -type f ! -name 'keepA'
  ! -name 'keepB' ... -delete`) that you have CHECKED against the actual `ls` of
  that dir, not guessed.
- Echo BEFORE/AFTER sizes from the script, and RE-VERIFY the deliverables still
  exist afterward. For a tape overwrite, write ONLY the single intended tar path
  (`tar cf /u/.../one.tar dir/`) - never touch other lfe datasets - and shallow-check
  it (`tar tf` all headers + a one-file data extract + key-member grep).

## Never Reference Public PRs/Issues in Private-Repo Commit Messages (CRITICAL)

GitHub auto-links `owner/repo#NNN` (and bare `#NNN`) in commit messages and
creates a public "referenced this pull/issue" cross-reference event on the
target. A commit in a PRIVATE repo (e.g. `~/projects` = oleg-alexandrov/projects)
that references a PUBLIC PR (e.g. `DOI-USGS/ale#719`) therefore LEAKS the private
repo's name, commit hash, and message snippet onto the public PR timeline. The
event is effectively permanent (survives rewrite/force-push of the source commit).

RULE: in commit messages for ~/projects (and any private repo), never write
`owner/repo#NNN` or `#NNN` for a public PR/issue. Write "PR NNN" / "pull NNN"
(no `#`, not repo-qualified). The notes FILE content may name the PR freely
(file contents are not auto-linked) - only the COMMIT MESSAGE matters.

## NEVER Run Heavy Compute on the Mac mini (CRITICAL - repeatedly burned)

The Mac mini (Olegs-Mac-mini) is a NOTES/light box, NOT a compute node. It RUNS
OUT OF MEMORY (OOM) under real compute and the whole session wedges - nothing
finishes and I cannot continue. RULE: if a script is anticipated to invoke
parallel_stereo / stereo or bundle_adjust in any NON-TRIVIAL way it must NOT be
run on the Mac - send it to pfe (qsub) or l1.
