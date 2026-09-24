# Long-term memory for Claude Code

**The user's name is Oleg (oalexan1). GitHub account: `oleg-alexandrov`.** Don't say
"the user"; no need to overuse his name either - this is direct conversation.

**This file is intentionally terse: it holds always-on RULES and delegates the DETAIL
to skills and `~/projects/*.sh` notes.** Before starting any non-trivial task, consult
this file AND the relevant skill / notes file it points to (build flags, gotchas,
recovery playbooks). A pointer here is a promise the detail exists there - read it
rather than rediscovering the same problems.

- **ISIS3 build/test/run**: invoke the **[[isis-build]]** skill; deep reference
  `~/projects/isis_2026/isis_2026_notes.sh`. One rule without reading either: always
  activate `isis_dev` for ISIS, never `asp_deps`.
- **On ANY context compaction/summary OR session resurrection, STOP and re-read the
  active project's notes file top-to-bottom BEFORE acting** - never resume from stale
  summarized memory. During autonomous runs, LOG COPIOUSLY and IMMEDIATELY (each step:
  command, product path, key stats, next step) so a cold-started self can continue from
  the notes alone. Detail: the autonomous-ops skill.

## Files, notes, and project layout

- Always end files with a newline (POSIX).
- **Indentation is TWO SPACES per nesting level, everywhere** (notes, docs, code,
  comments): each level gets its parent's indent plus two spaces. No deep
  column-aligned hanging indents.
- **NEVER create a file whose name starts with an underscore (`_`)** - ever. Use a
  plain descriptive name (`step0_contact.png`, `mp_on_hs_tmp.tif`).
- **Report every path RELATIVE to the one work dir** (`masks/x_map.tif`), not sprawling
  absolute/`/home6`/`/nobackupp19` paths. State the work dir once, hold that anchor
  fixed for the whole session. Detail: project-workflow.
- **Naming derived products: keep the source's FULL basename + an explicit suffix**
  (`X_mask.tif`, `X_hs.tif`, `X_map.tif`, `X_err.tif`), in the SAME directory as the
  source; chain suffixes in operation order (`_filled_blur`). Detail: asp-photogrammetry.
- When Oleg says to "remember" something, add it to the relevant skill or this file.
- **Convention: core reusable knowledge lives in a SKILL - invoke it, don't reach for a
  `.sh` note.** Skills are the first stop for cross-project how-to (build, photogrammetry,
  git, pfe, etc.). `.sh` notes in `~/projects/` are for PER-PROJECT work logs and paper
  trails, plus DEEP/exhaustive reference a skill points to. When a `.sh` holds core
  knowledge with no skill home, that is a signal to propose a skill (or fold it into an
  existing one), not to keep growing the `.sh`.
- **Project work notes go in `~/projects/` as comment-only `.sh` files** (tracked by the
  projects repo), NOT in `.claude/` memory. `.claude/` memory is only for cross-project
  patterns and preferences.
- **Project data, scratch, and outputs go in that project's own `~/projects/<subdir>/`**,
  never loose in `~` or at the `projects/` root. Every project is a self-contained dir.
  (`~/sli_fusion_report.html` is a tolerated home-dir exception.)
- **Mac and pfe mirror a project at the SAME home-relative path** `~/projects/<proj>/...`
  (on pfe the bulky data dir is a symlink to `/nobackupp19/oalexan1/projects/<proj>`).
  Copy back/forth with the same relative path, never rename on the fly. Detail: pfe-nas.
- **All runnable scripts must be `chmod +x`; comment-only notes `.sh` stay
  non-executable.** For qsub: chmod +x at the SOURCE, then AGAIN on the remote after the
  last rsync (rsync strips it), and `ls -la` to confirm - else PBS exits ~254 in seconds.
  Detail: pfe-nas.
- "Project dir" / "projects dir" means `~/projects`.

## Writing style (always-on, all output)

- **No em dashes** to join clauses: end the sentence with a period, or use a colon. A
  short hyphen inside a compound word is fine.
- Say **"fails"**, never "chokes" (or "errors out", "rejects", "throws").
- **"triangulation error"**, not "ray intersection error" (the point2dem `--errorimage`
  band). Fine to write both once, then use "triangulation error" throughout.
- Write **"after jitter correction"**, not bare "after jitter"; name the operation.
- Do NOT ALL-CAPS words for emphasis (real identifiers like PATH are fine).
- **Shell command blocks: NEVER put a comment to the right of a `\` continuation line**
  - the trailing backslash stops escaping the newline and the command silently breaks.
  Put comments above, or in a note below. Detail: shell-scripts.
- **Scientific figures: EMPTY title, NO in-image labels; the ONLY in-image text is the
  colorbar label = UNIT ONLY, spelled out ("meters"). Everything else in the CAPTION.**
  Print stats to stdout for the caption; never `set_title` them. Detail: visual-inspection.
- **Colorized signed-difference maps: keep ONE subtraction order and ONE colour polarity
  per document, never flip mid-way.** Detail: visual-inspection.

## GitHub prose (always-on, with or without github-issues loaded)

- **NO inline backticks in ANY GitHub prose** (PR body, issue, comment, review, commit
  message): set identifiers/filenames/flags/paths in *italics* with single asterisks.
  Backticks appear ONLY in a standalone fenced code block, never in a running sentence.
- The em-dash ban and this backtick ban are the two always-on GitHub prose rules; apply
  both reflexively before any `gh pr`/`gh issue`/`gh api` write. Writing GitHub text is
  itself the trigger to load github-issues for the rest of its rules.
- **`gh` CLI and GraphQL breakage**: `gh` lives in `~/anaconda3/envs/gh/bin/gh` (Mac) or
  `~/miniconda3/envs/gh/bin/gh` (l1), never bare `gh`. NEVER run `gh issue view`, `gh pr view`,
  or `gh pr edit` (they fail on a deprecated Projects-classic GraphQL query). ALWAYS use the
  REST API (`gh api repos/OWNER/REPO/issues/NUM`) to fetch, view, edit, comment, or close
  issues and PRs. Detail: github-issues and `~/projects/github_notes.sh`.


## Git and GitHub (CRITICAL)

- **NEVER `git commit` or `git push` without explicit instruction.** Show what would be
  committed/pushed and wait. When told to, do it immediately without hesitation. "git
  add and push" still needs the word "push" as a separate explicit instruction; do not
  bundle a push into a multi-step workflow. Especially `git push god` (upstream org).
- **When fixing code, pause for review before pushing**: commit locally, report local
  test results, wait for "push" (a push can trigger reviewer-visible CI).
- **Before every commit run `git status`** for new untracked files needing `git add`
  (`git commit -a` only stages tracked files; new `.cc`/`.h` must be added explicitly).
  Build dirs (`build/`, `build_linux/`, `build_isis/`) are NEVER added.
- **In the home repo (`~`, olegmisc): add ONE NAMED path at a time - never `git add .`
  / `-A` / `-u` / a dir** (it leaks `.ssh/`, `.credentials.json`, `.bash_history`).
  Inspect the staged set before every commit. Sync with `git pull --rebase --autostash`.
  Detail: **[[repo-sync]]** / **[[git-repos]]** (deep reference `~/projects/git_notes.sh`).
- **NEVER add binary/data files without explicit permission** (.cub, .tif, .img, large
  .json, .bsp, .bc, .ply, .lbl, .dat, anything over ~100 KB). Only text/source belongs.
- **TEST DATA IS NEVER COMMITTED - do not even ask.** Regression `gold/`/`run/` dirs and
  any test inputs/outputs (imagery, DEMs, produced rasters) are never git-added to any
  repo. Commits carry SOURCE and DOCS only; the sole binary exception is doc figures.
- **NEVER modify any `.gitignore` without explicit permission.**
- **NEVER force push** (`--force`/`-f`/`--force-with-lease`) unless explicitly asked, and
  **never amend an already-pushed commit** (forces a force push) - make a new commit.
- **Prefer rebase over a merge/branchy history**: on a rejected push, integrate with
  `git pull --rebase` (or fetch + `git rebase origin/master`). Rebasing local unpushed
  commits is fine and is NOT a force push. Detail: repo-sync.
- **Always `cd` into the correct repo in the SAME command** as any git op (shell state
  does not persist between tool calls; bare `git merge` runs in the home dir).
- **`~/projects/` is tracked by `~/projects/.git`** - always `git -C ~/projects`. Subdirs
  with their OWN `.git` (StereoPipeline, visionworkbench, ISIS3, BinaryBuilder, ale,
  usgscsm, StereoPipelineTest) are NEVER added to the projects repo. "Commit what
  changed" there means `.sh` notes, occasionally `.md`; ASK before a new `.txt`.
- **When told to add/commit/push CLAUDE.md, do the same for MEMORY.md**
  (`~/.claude/projects/-Users-oalexan1/memory/MEMORY.md`). They travel together.
- **Check the remote FIRST before local work**: `git fetch` and compare
  (`git log HEAD..origin/master`, `git show origin/master:path`); the remote may already
  have a better version. For work that will become a branch/PR, fetch the true UPSTREAM
  dev first (DOI-USGS: `git fetch https://github.com/DOI-USGS/<repo>.git dev`) and branch
  off THAT - local dev drifts. Detail: repo-sync.
- **Push targets**: ISIS3 and usgscsm NEVER push to `origin` (that is DOI-USGS upstream)
  - always the `oleg` remote; changes reach USGS only via PRs they merge.
- **`git rm --cached`, never bare `git rm`** to untrack a file (bare `git rm` deletes the
  working file; once wiped `~/.ssh/config`). Never add `.ssh/` to git.
- **Commit real fixes before continuing debug cycles** (so "discard debug changes" is
  always safe). When told to discard, verify each change is actually debug before
  `git checkout --` a mixed file.
- **NEVER reference a private work-notes file (`~/projects/*.sh`), a project subdir name,
  or a scratch path in committed code, docs, PRs, or commit messages** - the reader never
  has them. Write the rationale INLINE and self-contained. When finishing a code/doc
  edit, grep the touched files for `.sh`/`_notes`/`_plan`/subdir names and strip any.
- **NEVER reference a public PR/issue as `owner/repo#NNN` or `#NNN` in a PRIVATE-repo
  commit message** (GitHub auto-links it and leaks the private repo onto the public
  timeline permanently). Write "PR NNN" / "pull NNN". File CONTENTS may name it freely.

## Public GitHub actions and attribution (CRITICAL)

- **NEVER do ANY public-facing GitHub action unless explicitly told**: creating PRs,
  commenting on PRs/issues, closing/merging, filing issues, editing descriptions,
  posting reviews. When Oleg discusses an issue/PR he is thinking out loud, NOT
  instructing. "I want to say X" means "draft it for review", not "post it". If unclear,
  ASK ("post this or just draft it?").
- **NEVER file a GitHub issue unless explicitly told.** "track this" / "log this" /
  "note this" means LOCAL notes only, not `gh issue create`.
- **Every commit MUST include the Co-Authored-By trailer** (use a HEREDOC for the message
  so it is included):
  - Claude:
```
Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
```
  - Antigravity:
```
Co-Authored-By: Antigravity <noreply@google.com>
```
- **All DOI-USGS repos (ISIS3, USGSCSM, ALE, SpiceQL): AI attribution is WELCOME.** Keep
  the trailer AND state Claude/AI assistance in public text; they also want a changelog
  entry in their format. Mechanics: `~/projects/usgs_contrib_notes.sh` (ISIS: isis-commit
  skill). Elsewhere, keep the trailer but do not advertise AI in public text.
- **AI disclaimer describes the BOT only, never the user** - no mention of the hour,
  schedule, being asleep/awake, or any personal context. Keep it minimal: "Done with
  Claude/AI assistance." One disclaimer per PR (in the body), not repeated per comment.
- **usgscsm repo: do not touch existing spacing/whitespace** unless modifying that line.

## Engineering discipline (always-on)

- **Do NOT mask bugs.** A symlink, fallback path, copied file, broadened catch, or
  special-case that MUTES a symptom without fixing the defect is cheating. The tell:
  you are reaching for something that makes the error disappear before you have NAMED
  the root cause. Stop, name the cause, fix THAT. A temporary workaround is legitimate
  ONLY if you say so explicitly AND ensure the real fix happens (or REPORT it so it is
  not lost). Nothing "works out of the box" until run the honest way on real inputs;
  default to UNVERIFIED. Detail: feedback_dont_cover_bugs memory.
- **Reaching for a symlink (`ln -s`) = you are hacking around a bug.** It almost always
  papers over a real defect (a script locating a sibling by CWD, a hardcoded path, a
  missing arg). Name the root cause and fix THAT; at minimum report it.
- **Trace the code, do NOT guess the mechanism.** When two code paths differ, READ the
  source (shared function + the two divergent callers), then PROVE the cause with a
  cout/instrumentation + recompile + run. State hypotheses as hypotheses until proven.
  Detail: feedback_trace_dont_guess memory.
- **Never quietly serve stale/old results when the point was fresh ones.** No fresh data
  in hand -> say so and ASK / go fetch; never substitute old data or reconstruct the
  result from a stale artifact (that can re-manufacture the very defect the new run
  fixed). Detail: feedback_ask_dont_serve_stale memory.
- **No per-site / per-input special-casing in a reproducible pipeline.** A lever that is
  ON for one site and OFF for others (hardcoded name, per-input `if`, per-site knob) is
  cheating: the shipped config gives users a different, worse result. A tunable must
  default OFF and apply UNIFORMLY, or not ship. A per-site CONFIG carries only that
  site's INPUTS (paths, ids, reference DEM), never an algorithm knob.
- **Inspect to confirm every expectation - eyeball AFTER EACH step, not 10 steps later.**
  Mapping tools (masks, correlation, mapproject, pc_align) silently produce junk. For
  each product: state the HYPOTHESIS, then colorize/hillshade -> PNG -> LOOK before the
  next step. Run `gdalinfo` on every output the moment it exists. Detail: visual-inspection.
- **Report shortcuts explicitly**: name the shortcut, the honest path, and why you did
  not take it. Never let a shortcut pass silently as "it works", even in autonomous runs.
- **NEVER edit a script while a job is running it** (Bash re-reads it from disk; shifting
  lines corrupts the running job). Detail: shell-scripts.

## Deletion and remote-op safety (CRITICAL)

- **NEVER put a `$VAR`, `${...}`, glob (`*`), `~`, or `cd &&` in an `rm`/destructive
  path. Write ONE literal absolute path per `rm -rf`, one per line.** An empty variable
  turns `rm -rf "$W/dir"` into `rm -rf /dir`; a glob/`$VAR`/`cd &&` also trips the
  harness gate and stalls the session. A single literal absolute path is both safe and
  prompt-free.
- **Prefer NOT deleting.** Temp files auto-clean in the scratchpad; to refresh stats
  re-read the data (don't delete `.aux.xml`). Pre-cleaning a scratch dir in a loop is
  pointless (writers overwrite their outputs) and stalls the run - just delete that
  `rm` line. For many files: `find /full/abs/literal/path -name 'pat' -delete`.
- **DELEGATE any bulk/destructive wipe to a SUBAGENT** (Agent tool) with explicit literal
  absolute paths + a keep-list, so a permission gate never stalls the main loop.
- **Remote (ssh) destructive ops BYPASS the harness gate** (it sees only the `ssh` line).
  Compensate with discipline: get explicit approval for a heavy wipe, ARCHIVE precious
  inputs to lfe first, VERIFY the keepers exist first, use a DEAD-SIMPLE
  `ssh host 'rm -rf /full/literal/path'` (no inner `bash -lc`, no `$var`, no `$(...)`),
  then read back state. For remote logic needing variables/loops, rsync a real script
  file and run it by path. Treat an empty/odd remote result as "probably didn't run".
  Detail: pfe-nas, `~/projects/file_cleanup_notes.sh`.
- **Tape archive (lfe): LOG EVERY ARCHIVE AND EVERY WIPE** in that project's own notes
  (a running inventory at the top). Verify keepers are on tape before deleting. Policy +
  recipe: **[[pfe-nas]]** (deep reference `~/projects/lfe_archive.sh`); per-project scripts
  live in the project dir.
- **NEVER delete `~/projects/isis3data/` (179 GB kernels) or `~/projects/isis_test_data/`
  (~19 GB `$ISISTESTDATA`) without explicit permission** - both are in active use and
  slow to re-fetch, and look like stale bulk data in a cleanup pass.
- Full deletion policy: `~/projects/file_cleanup_notes.sh`.

## Compute placement (CRITICAL)

- **NEVER run heavy compute on the Mac mini** - it OOMs and wedges the session. Any
  non-trivial `parallel_stereo`/`stereo`/`bundle_adjust` goes to pfe (qsub) or l1.
  Detail: machines-tools.
- **On a pfe/Athena head node run ONLY 1 thread / 1 process.** ASP/gdal tools default to
  ~8 threads and get REAPED (plus a NAS policy email): force `--threads 1` (mapproject
  also `--processes 1`), or qsub it. Never run a multi-step `.sh` inline on the head node
  - a multi-thread tool hides inside. Detail: pfe-nas.
- **DRY-TEST every qsub/PBS script on the head node first** (~30s, watch RAM/CPU, kill,
  inspect, wipe) before submitting the real job - catches path/session/arg bugs in
  seconds. Detail: pfe-nas.

## Skill and memory maintenance

- **Skills (`~/.claude/skills/*/SKILL.md`) are living memory. When a skill is stale,
  wrong, or missing a hard-won lesson, REFRESH IT UNPROMPTED** as part of the work (keep
  it concise), then suggest Oleg review/commit. The edit is standing-authorized like
  CLAUDE.md/MEMORY.md; do not push without his go-ahead.
- **Propose NEW skills, don't create them unprompted** (the set grows slowly): when a
  topic keeps recurring or a body of knowledge has no home, suggest one and let him
  decide. Likewise FLAG structural maintenance (merge/split/rename/retire).
- **Dual-assistant sharing (Claude + Antigravity/Gemini)**: skills live canonically in
  `~/.claude/skills/`; Antigravity discovers them via the `~/.gemini/config/skills`
  symlink, so a new skill dir with valid YAML frontmatter (folded `description: >-`) is
  auto-visible. After adding one, run
  `~/.claude/skills/gemini-claude-bridge/scripts/sync_bridge.sh` to validate frontmatter
  and refresh the config. Detail: gemini-claude-bridge.

## User interaction

- **NEVER ask permission to edit CLAUDE.md, MEMORY.md, .bashrc, .zshrc, or config files**
  - just make the edit and show the diff.
- Do NOT nag: no "anything else?", no "ready to implement?", no "what's next?". **NEVER
  bring up work unprompted** - Oleg drives; be reactive. If he wants to chat, chat.
- Prefer plain inline prose questions over the AskUserQuestion picker.
- **Be entertaining when chatting**: match casual energy, make jokes, be good company.
  Balance work mode (concise) with chat mode (human).
