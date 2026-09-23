#!/usr/bin/env bash
# Sync and maintain the global bridge between Claude and Antigravity (Gemini).
# Idempotent setup for ~/.gemini/config using ~/.claude as the canonical source.

set -euo pipefail

CLAUDE_DIR="${HOME}/.claude"
GEMINI_CONFIG="${HOME}/.gemini/config"

echo "Checking canonical Claude paths..."
if [ ! -d "${CLAUDE_DIR}/skills" ]; then
  echo "Error: ${CLAUDE_DIR}/skills does not exist." >&2
  exit 1
fi
if [ ! -f "${CLAUDE_DIR}/CLAUDE.md" ]; then
  echo "Error: ${CLAUDE_DIR}/CLAUDE.md does not exist." >&2
  exit 1
fi

mkdir -p "${GEMINI_CONFIG}"

echo "Linking skills directory..."
ln -sf "${CLAUDE_DIR}/skills" "${GEMINI_CONFIG}/skills"

echo "Writing skills.json..."
cat << 'EOF' > "${GEMINI_CONFIG}/skills.json"
{
  "entries": [
    {
      "path": "~/.claude/skills"
    }
  ]
}
EOF

echo "Setting up global GEMINI.md rules..."
cat << 'EOF' > "${GEMINI_CONFIG}/GEMINI.md"
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
- **Commit Messages**: Never reference public PRs/issues in private-repo commit messages. Include Co-Authored-By trailer when requested.

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
EOF

ln -sf "${GEMINI_CONFIG}/GEMINI.md" "${GEMINI_CONFIG}/AGENTS.md"

echo "Validating YAML frontmatter for all skills..."
python3 -c '
import glob, os, yaml

skills = glob.glob(os.path.expanduser("~/.claude/skills/*/SKILL.md"))
errors = 0
for s in sorted(skills):
    with open(s) as f:
        content = f.read()
    if not content.startswith("---"):
        print(f"Warning: {s} missing frontmatter delimiter")
        errors += 1
        continue
    parts = content.split("---", 2)
    try:
        fm = yaml.safe_load(parts[1])
        if not (isinstance(fm, dict) and "name" in fm and "description" in fm):
            print(f"Warning: {s} missing name or description")
            errors += 1
    except Exception as e:
        print(f"YAML error in {s}: {e}")
        errors += 1

if errors == 0:
    print(f"All {len(skills)} skills have valid frontmatter.")
else:
    print(f"Encountered {errors} errors during validation.")
'

echo "Sync complete. All skills and rules mapped to ${GEMINI_CONFIG}."
