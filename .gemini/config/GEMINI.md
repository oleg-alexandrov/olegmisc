# User & Project Directives (Antigravity & Claude Memory)

> Canonical memory source: `~/.claude/CLAUDE.md`. Consult it and the relevant `~/projects/*.sh` notes before starting any non-trivial task.

## Identity & Core Working Style
- **User**: Oleg (`oalexan1`), GitHub account: `oleg-alexandrov`.
- **Indentation**: Exactly TWO SPACES per nesting level, everywhere (notes, text docs, code, comments). No deep column-aligned hanging indents.
- **File naming**: NEVER create a file starting with an underscore (`_`). Not scratch figures, not temp rasters, ever.
- **File formatting**: Always end files with a newline character (POSIX requirement).
- **Punctuation & Prose**:
  - No em dashes (use a period or colon).
  - Word choice: say "fails", never "chokes".
  - Term: "triangulation error", not "ray intersection error".
  - Docs phrasing: "after jitter correction", not bare "after jitter".
  - GitHub prose: NO inline backticks, use *italics*.
  - Plain text paths (NO clickable links): NEVER return clickable paths or markdown links (`[text](file://...)`). Always return explicit file paths (full or relative as requested) in plain text so they can be copied and pasted in the terminal without being truncated or hidden.
- **Shell commands**: NEVER comment to the right of a continuation line (`\`).
- **Notes discipline**:
  - Project work notes go in `~/projects/` as `.sh` comment-only files (e.g. `~/projects/*.sh`), NOT loose in home dir or scattered.
  - Project scratch and outputs go in `~/projects/<subdir>/`, never in `~`.
  - On any context compaction or session resumption: re-read the active project's notes file top-to-bottom before acting.

## Git & Repository Discipline (CRITICAL)
- **Check Remote First**: Check remote before doing local work on a repo (`git fetch`, inspect remote changes).
- **Never `git add .` or `-A`**: Add NAMED files only.
- **`git rm --cached`**: Never use bare `git rm`.
- **Private Notes**: NEVER reference private work-notes files (`*.sh` notes) in committed code, public PRs, or public docs.
- **Commit Messages**: Never reference public PRs/issues in private-repo commit messages. Include Co-Authored-By trailer: `Co-Authored-By: Antigravity <noreply@google.com>`.

## Execution & Engineering Discipline (CRITICAL)
- **Trace the Code**: Trace the code, do NOT guess the mechanism.
- **No Masking Bugs**: Report shortcuts and temporary fixes; do not mask bugs or hack around bugs with ad-hoc symlinks.
- **No Stale Results**: Never quietly serve old/cached results when the user requested a fresh run.
- **No Special-Casing**: No per-site or per-input special casing in reproducible pipelines.
- **Scripts in Flight**: NEVER edit a script while a running job is executing it.
- **HPC & Compute Limits**:
  - NEVER run heavy compute on the Mac mini.
  - NEVER run more than 1 thread / 1 process on the pfe (or Athena) head node.
  - Dry-test every qsub/PBS script on the head node first.
- **Report Paths**: Report paths relative to the current work directory.

## Automatic Skills Integration
All skills located in `~/.claude/skills/` are mounted and available in Antigravity via `~/.gemini/config/skills/` and `~/.gemini/config/skills.json`. Whenever a task matches a skill's purpose (e.g. `asp-photogrammetry`, `bundle-adjust`, `csm-models`, `pfe-nas`, `gdal-rasters`, etc.), consult and follow that skill's instructions.
