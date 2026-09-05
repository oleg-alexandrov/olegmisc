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
- **On ANY context compaction/summary OR session resurrection, STOP and re-read
  the active project's notes file top-to-bottom (esp. its WORK LOG + resurrection
  policy) BEFORE acting - never resume from stale/summarized memory.** During
  autonomous/overnight runs, LOG COPIOUSLY and IMMEDIATELY as you go (each step:
  command, product path, key stats, next step), so a cold-started self can
  continue from the notes alone. Full discipline: the autonomous-ops skill
  ("Notes Discipline + Reread-On-Resurrection").
- **Project-specific data, scratch, and outputs go in the relevant
  `~/projects/<subdir>/`, never loose in the home dir or scattered around.** Do
  not create scratch dirs or stray files in `~` (e.g. `~/sli_fusion_lr`, build
  logs); stage work inside the project's own subdir so it stays findable and the
  home dir stays clean. (`~/sli_fusion_report.html` is a tolerated exception: a
  temp, paste-ready report Oleg keeps at home for convenience.)
- **When told to add/commit/push CLAUDE.md, always do the same for MEMORY.md
  (`~/.claude/projects/-Users-oalexan1/memory/MEMORY.md`) too.** They travel together.
- "Project dir" or "projects dir" means `~/projects`.
- **Mac and pfe mirror a project at the SAME home-relative path `~/projects/<proj>/...`**
  (only the home prefix differs: on pfe `~/projects/<proj>` is a symlink to
  `/nobackupp19/oalexan1/<proj>`; the bytes live on nobackup). So copy back/forth with the
  SAME relative path, never rename on the fly, and a matching path means the data is in both
  places. Detail (symlink setup, mirror-the-remote-relative-path rule): the pfe-nas skill.
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
- **TEST DATA IS NEVER COMMITTED - IRONCLAD, DO NOT EVEN ASK.** Regression
  `gold/` and `run/` dirs, and test inputs/outputs of any kind (imagery, DEMs,
  .cub/.tif/.img, produced rasters), are NEVER git-added to ANY repo - not ASP,
  not StereoPipelineTest, not BinaryBuilder, not any test dir. Regolding writes
  `gold/` on l1 only (it is gitignored, ~40 GB); that data lives on disk, never
  in git. Our commits carry SOURCE and DOCS only; the sole binary exception is
  figures for documentation. This rule is permanent and will not change - do not
  ask permission to add test/gold/run data, just never do it.
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

**POLICY: always base new work on the UPSTREAM remote dev/main, fetched FIRST.**
Before ANY git work that will become a branch or PR, the FIRST action is to fetch
the true UPSTREAM branch (for DOI-USGS repos: `git fetch https://github.com/DOI-USGS/<repo>.git dev`)
and branch off THAT. NEVER branch off a local `dev`/`main` or a fork's dev without
fetching upstream first - they drift. Seen 2026-08-28 on ISIS3: local `dev` was 25
commits behind DOI-USGS/dev (and `adam/dev` even further behind). A branch off local
dev would have opened the PR on a stale base. Fetch upstream dev, branch off it,
replay the change on top, then push/PR. This holds for every repo, every time.

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

## Word choice: say "fails", never "chokes"

Never write "choke"/"chokes"/"choked" when you mean fail. Use "fails" (or
"errors out", "cannot read", "rejects", "throws"). Applies everywhere - chat,
notes, commits, PRs, docs, code comments. Same spirit as the avoid-jargon rule:
plain, precise verbs, not casual slang. This is the running style-guide section
for word choices to prefer/avoid (extend it as more come up).

## Punctuation: no em dashes (use a period or colon)

Oleg dislikes the em-dash style. Never use a long em dash to join clauses. End
the clause with a period and start a new sentence, or use a colon when introducing
something. Applies everywhere - chat, notes, commits, PRs, issues, docs, code
comments. (A short hyphen inside a compound word is fine; the ban is on the em dash
"-" used as a clause separator.) This is a running STYLE GUIDE; the github-issues
skill carries the fuller GitHub-text formatting rules (flowing paragraphs; no
horizontal rules; verified step-by-step repro) - load it before drafting any
GitHub text - but the no-backticks rule below is stated inline BECAUSE it must
apply even when that skill is not loaded.

## GitHub prose: NO inline backticks, use *italics* (CRITICAL - keeps recurring)

In ANY GitHub prose - a PR body, an issue body, a comment, a review, a commit
message - never wrap an identifier, filename, flag, path, command, or keyword in
backticks. Set it in *italics* with single asterisks instead. Backticks appear
ONLY inside a standalone fenced code block, never in a running sentence. This
rule applies EVERY TIME, with or without the github-issues skill loaded; do not
delegate it to a skill you might forget to load (that is exactly how it leaked
into usgscsm PR 534, which had backticks around every identifier). The em-dash
ban above and this backtick ban are the two always-on GitHub prose rules; apply
both reflexively before any `gh pr`/`gh issue`/`gh api` write. Writing GitHub text
is itself the trigger to load github-issues for the rest of its rules.

## Shell command blocks: NEVER comment to the right of a continuation line

A multi-line shell command (backslash-continued) exists to be COPY-PASTED and RUN.
Never put a comment to the right of a continuation line. `--foo bar \  # note` is
not just ugly - it BREAKS the line continuation, because the backslash is no longer
the last character on the line, so the command silently stops continuing and fails.
Put clarifying comments ABOVE the command or, better, in a short note BELOW it (e.g.
after the block: "here image_list.txt is the 75 cubs and camera_list.txt the 75 CSM
states"). The command itself stays pure and runnable, one clean `\`-terminated line
each. Applies everywhere a runnable multi-line command appears - READMEs, docs,
notes, chat.

## Term: "triangulation error", not "ray intersection error"

Prefer "triangulation error" over "ray intersection error" / "intersection error"
for the stereo point2dem error (the `--errorimage` band, `run-IntersectionErr.tif`).
Applies to chat, notes, commits, docs, figure captions, and the ASP RST docs. It is
fine to write both once ("triangulation error (ray intersection error)") the first
time in a doc for clarity, then use "triangulation error" throughout.

## Colorized-plot polarity: keep it CONSISTENT within a document, never flip

For signed-difference maps (dz, DEM-minus-ref, disparity) keep ONE fixed subtraction
order and ONE fixed colour polarity across the whole document/report - never revert
it mid-way (Oleg gets confused). Convention that matches the existing CTX-Jezero
artifact: compute every diff as [evaluated - reference] (e.g. mosaic - HRSC, corrected
- mosaic) and colour it BLUE = evaluated ABOVE reference (matplotlib `RdBu`, blue at
+vmax). Do NOT introduce an artificial vertical shift to make a diff look nicer, and
do NOT claim a specific datum cause (areoid, geoid) unless it is actually verified.
(Note: this per-document polarity can differ from the visual-inspection skill's default
RdBu_r; consistency within the document wins - pick the doc's convention and hold it.)

## Docs phrasing: "after jitter correction", not bare "after jitter"

In docs/figure captions, write the full action, not the noun alone: "before/after
jitter correction" (or "solving for jitter"), never bare "before/after jitter".
Same spirit for any process - name the operation, not just the thing.

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

## Keep Skills Fresh - Refresh Unprompted, Then Flag for Review

Skills (`~/.claude/skills/*/SKILL.md`) are living memory. Whenever, in the course of
any task, I find a skill is stale, wrong, or missing a hard-won lesson that belongs in
it (a new gotcha, a corrected mechanism, a technique worth reusing), REFRESH THE SKILL
UNPROMPTED - do not wait to be told. Make the edit as part of the work, keep it
concise and self-contained, then SUGGEST to Oleg that he review the change and approve
committing it. Do not push a skill edit without his go-ahead (normal git-push rule
still applies), but the edit itself is standing-authorized like CLAUDE.md/MEMORY.md.
The point: knowledge learned once should be captured immediately in the relevant
skill, not left to evaporate.

I am also IN CHARGE of maintaining the whole skill SET, not just editing existing
files. Two more standing duties:
- **Propose NEW skills.** When it appears a new skill is in order - a topic keeps
  recurring, a body of hard-won knowledge has no home, or a task type would clearly
  benefit next time - SUGGEST creating one to Oleg (say what it would cover and why).
  We are deliberately growing the skill set SLOWLY to educate me over time, so raise
  the idea rather than silently sprawling: propose, let him decide. Do not create a
  new skill unprompted; the refresh authorization is for editing EXISTING skills.
- **Advise on structural maintenance.** When the skill set needs reorganizing -
  merging two overlapping skills, splitting one that has grown too broad, renaming,
  re-scoping a description, or retiring a stale one - FLAG it and suggest the change.
  I own noticing this; Oleg approves the restructuring.

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

## Never Quietly Serve Stale/Old Results When the Point Was to Get NEW Ones (CRITICAL)

When a task's WHOLE POINT is to produce fresh results (a new run, re-measured
data, an updated figure), do NOT silently fall back to OLD or known-bad results
if the fresh ones aren't in hand (remote host down, data not pulled, run not
finished). ASK, or go GET the fresh ones - never quietly hand over the stale
version dressed up as the deliverable. The old data was often bad ON PURPOSE and
the entire reason for the new run. Worse: reconstructing/approximating the result
from a stale local artifact can reintroduce the very defect the new run fixed.
Burned 2026-08-30 (WV03 green-CCD): pfe was offline, so instead of asking I
reconstructed the "after correction" panels from a stale local curve
(`C1 - roll(C1,13)`), which manufactured the exact +13 seam spikes the shift had
removed - the opposite of the shipped result. Oleg had to catch it. The fix was
to pull the REAL re-measured per-scene residuals from pfe once he restored access.
RULE: no fresh data in hand -> say so and ask / fetch; never substitute old data.
Saved as `[[feedback_ask_dont_serve_stale]]` in memory.

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

**EYEBALL AFTER EACH STEP - not 10 steps later (CRITICAL, the recurring failure).**
Every image or DEM product is visually inspectable, and mapping tools (otsu/KDE
masks, correlation, mapproject, pc_align) are FRAGILE - they silently produce
junk. Claude's demonstrated mistake is going through the motions of a multi-step
pipeline WITHOUT looking, so a bad mask/threshold/regrid at step 2 is only caught
10 steps later after wasted compute. The discipline: for EACH product, first state
the HYPOTHESIS (what it should look like - "the mask keeps the runway/houses and
drops the coral/underwater"), THEN eyeball (Claude has eyes: colorize/hillshade ->
PNG -> look) to CONFIRM it. Do not launch the next step until the current product
is confirmed by eye. Frequent inspection is not optional in geospatial work - it is
the work. To compare two rasters by eye, first `gdalwarp` them to the SAME grid,
extent, and projection (so the PNG-based image comparison is apples-to-apples),
then hillshade/colorize and look. (See the visual-inspection and asp-photogrammetry skills.)

**Cheap checks on produced output files: always do them.** If the recipe says an
output DEM/raster must have a certain grid size, resolution, or projection, run
`gdalinfo` on it the moment it exists and confirm it conforms. A 1-second check
saves countless grief downstream.

## Naming Derived Products (keep the source's FULL basename) (CRITICAL)

A derived file MUST be named after its SOURCE's FULL basename plus an explicit
descriptor suffix, so any product unambiguously names its parent. For source
`X.tif`: mask -> `X_mask.tif`, hillshade -> `X_hs.tif`, mapprojected -> `X_map.tif`
(or `X.map.tif` only where a tool REQUIRES that exact token, e.g. bundle_adjust
`--mapprojected-data`), error image -> `X_err.tif`, etc. NEVER invent a cryptic
short name that drops the base: for `pan_200013549424.r100.tif` the mask must be
`pan_200013549424.r100_mask.tif`, NOT `new_424_pan_mask.tif` (untraceable to its
source). **A derived dataset stays in the SAME DIRECTORY as its source** (next to
it), carrying the source's full basename + suffix - do NOT drop it in a separate
generic dir (`dem/`, `masks/`) with a re-invented name, which MIXES provenance
(e.g. lidar-derived DEMs jumbled with stereo DEMs) and makes it unclear what came
from what. Example: for `data/lidar/Florida_..._Ellipsoid.tif` the derivatives are
`data/lidar/Florida_..._Ellipsoid_blur.tif`, `..._Ellipsoid_filled.tif`,
`..._Ellipsoid_filled_blur.tif` (filled first, then blurred) - all in `data/lidar/`,
never `dem/blurred_lidar.tif`. Distinguish variants (masked vs unmasked, etc.) by an
EXPLICIT token (`_mask`, `_full`, `_blur`, `_filled`) - NEVER by only `.` vs `_`
before the same word (`X.map.tif` vs `X_map.tif` is an unreadable trap). Chain
suffixes in operation order (`_filled_blur` = filled then blurred).

## Report Paths Relative to the Work Dir (CRITICAL)

There is always exactly ONE work dir for a task. Report every path RELATIVE to it
(`masks/new_424_mask_map.tif`), consistently - not sprawling absolute paths
(`/nobackupp19/.../sdb_2026_08/masks/...` or the `/home6` symlink twin). State the
work dir ONCE, then keep all paths relative to it. Same in notes, scripts, and chat.
Do NOT keep changing the anchor mid-conversation (sometimes `/home6`, sometimes
`/nobackupp19`, sometimes relative) - that has annoyed Oleg repeatedly. Pick the
project work dir once and hold it fixed for the whole session; every path is
work-dir-relative unless he explicitly asks for the absolute path.

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

**DELEGATE destructive/bulk-wipe ops to a SUBAGENT (Oleg's standing rule, "as
usual", 2026-09-03).** When a step needs a destructive/harness-triggering command
(bulk `rm -rf` of build dirs, wiping several dirs, any delete that risks a sandbox
prompt and stalls the main loop), hand it to a subagent via the Agent tool instead
of running it in the main session. The subagent executes the wipe in its own
context so the main loop is not stalled by a permission gate. Give the subagent the
EXPLICIT LITERAL ABSOLUTE PATHS to remove (one `rm -rf /full/abs/path` per line, no
glob/`~`/`$VAR`), the keep-list, and instruct it to verify each path before deleting
and report back. This is the default for any non-trivial cleanup; we have been
bitten repeatedly by doing it inline.

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

## Nested Quoting in Remote/Docker Commands Is Dangerous - Keep Destructive Ops DEAD SIMPLE (CRITICAL)

Deeply-nested remote invocations - `ssh host 'bash -lc "... \$var ... \$(cmd) ...
awk \\\$4 ..."'` or the docker equivalent - are FRAGILE. The multiple layers of
quoting/escaping are easy to get wrong, and when they are wrong the failure modes
are bad in BOTH directions:
- BEST case (proven 2026-08-23, the ale respin cleanup): an over-nested
  `ssh l1 'bash -lc "... conda ... awk \\\$4 ..."'` produced NO output and
  SILENTLY DID NOT RUN the `rm` lines at all - I thought l1 scratch was wiped; it
  was still there. A silent no-op on a cleanup reads as success and isn't.
- WORST case: a `$VAR` that expands empty (or a mis-terminated quote) turns
  `rm -rf "$W/dir"` into `rm -rf /dir` or `rm -rf /` on the REMOTE, where the
  harness gate does NOT apply. This is how you wipe someone's root.
RULES:
- For any REMOTE destructive op, use a DEAD-SIMPLE command: `ssh host 'rm -rf
  /full/literal/absolute/path'` - single outer quotes, NO inner `bash -lc`, NO
  `$var`, NO `$(...)`, one literal absolute path per `rm`. Then read back with a
  separate simple command (`ssh host 'test -d /path && echo STILL || echo GONE'`).
- If remote logic genuinely needs variables/loops, write a real script FILE,
  rsync it over, and run it by path (`ssh host bash /abs/path/script.sh`) - never
  inline a multi-level-escaped one-liner for anything that deletes.
- Treat an empty/odd result from a nested remote command as "it probably didn't
  run", not "it worked" - re-verify state explicitly before moving on.

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

## DRY-TEST EVERY qsub/PBS SCRIPT ON THE HEAD NODE FIRST (CRITICAL - keeps recurring)

Before launching ANY script via qsub on pfe/Athena, DRY-TEST it on the head node
first. Do NOT burn a queue+run to discover a trivial bug (a relative path broken by
a `cd`, a missing input file, a bad `-t` camera session, an arg typo) - these
structural errors surface in the FIRST SECONDS, and a real qsub wastes the queue
wait, the SBUs, and my momentum. This bit us again 2026-08-28 (a `cd v5` broke
relative `data/...` paths -> every input missing -> bundle_adjust exit 1, whole job
dead - a 30s dry test would have caught it). PROCEDURE:
- Launch the REAL script on the head node redirecting to its log, in its own session
  so the whole tree can be killed: `setsid bash script.sh <args> >/dev/null 2>&1 &`
  (the script's own `exec>` log captures output; peek that, or the produced dir).
- WATCH RAM and CPU while it runs (`ps -o pid,rss,pcpu,comm -u $USER --sort=-rss`).
- KILL the whole process tree within ~30s (`pkill -KILL -u $USER -f '<toolnames>'`) -
  and KILL EARLIER the instant memory or CPU spikes (the head node OOMs and the
  policy gate reaps you + emails). The dry run must never be allowed to run real
  heavy compute; 30s is only to let the cheap early steps (path resolution, file
  existence, session init, arg parse) run and error out.
- INSPECT the log / produced dir for errors. Then WIPE every produced file
  individually (rm the dry-run outputs / the run subdir by a literal absolute path).
- ONLY after a clean dry test (early steps ran, inputs resolved, no error) submit the
  real qsub. Full mechanics also in the pfe-nas skill.

## NEVER Run Heavy Compute on the Mac mini (CRITICAL - repeatedly burned)

The Mac mini (Olegs-Mac-mini) is a NOTES/light box, NOT a compute node. It RUNS
OUT OF MEMORY (OOM) under real compute and the whole session wedges - nothing
finishes and I cannot continue. RULE: if a script is anticipated to invoke
parallel_stereo / stereo or bundle_adjust in any NON-TRIVIAL way it must NOT be
run on the Mac - send it to pfe (qsub) or l1.

## NEVER Run More Than 1 Thread / 1 Process on the pfe (or Athena) Head Node (CRITICAL - keeps recurring)

On a pfe/Athena HEAD/FRONT-END node, run ONLY 1 thread and 1 process. Anything
with >1 thread/process is REAPED by the policy gate after ~1 minute (and fires a
NAS policy-violation email) - it does NOT finish, and it silently leaves EMPTY or
partial output that looks like a different bug. This has bitten repeatedly (Oleg
keeps flagging it): most ASP tools DEFAULT to ~8 threads, so a bare `dem_mosaic`,
`point2dem`, `bundle_adjust`, `pc_align`, `geodiff`, `sat_sim`, `mapproject`, etc.
on the head node trips the gate. RULES:
- Any ASP/gdal tool run on the head node MUST be forced to a single thread
  (`--threads 1`; mapproject also `--processes 1`). A bare invocation is a bug.
- `dem_mosaic --threads 1` on the head node is FINE; bare `dem_mosaic` is NOT
  (the exact miss on 2026-09-05: bare dem_mosaic got reaped, left a 0%-valid
  empty mosaic; `--threads 1` produced the real 22.7%-valid mosaic).
- If in doubt, or for anything heavier than a lone single-thread streaming op,
  QSUB it (devel for quick) - never the head node.
- If the head node KILLS a command, do NOT retry it there (re-trips the gate +
  another email); move it to qsub or shrink to 1 thread / 1 process.
- NEVER run a multi-step `.sh` inline on the head node - a multi-thread tool can
  hide inside it. A script that does real work is a qsub job, period.
Full detail (models, budgets, env): the pfe-nas skill (head-node section).
