---
name: gemini-claude-bridge
description: >-
  How Claude Code and Antigravity (Gemini) skills, memory, and rules are mapped and synchronized - global ~/.gemini/config/ setup, ~/.claude/skills/ integration, CLAUDE.md to GEMINI.md bridging, and the sync_bridge.sh repair script. Load whenever the user asks how we mapped Claude to Gemini, asks to check or fix the Claude/Gemini bridge, syncs skills between assistants, or configures Antigravity on a new machine.
---

# Gemini - Claude Bridge and Customizations Sync

This skill documents how Claude Code and Antigravity (Gemini) share skills and long-term memory, ensuring both assistants have access to the exact same runbooks and rules without polluting project repositories.

## Architecture

1. Skills:
  - Canonical location: `~/.claude/skills/<skill-name>/SKILL.md`.
  - Global link in Antigravity: `~/.gemini/config/skills` (symlink) and `~/.gemini/config/skills.json`.
  - Both assistants discover and load skills dynamically via progressive disclosure.
2. Rules and Memory:
  - Canonical location: `~/.claude/CLAUDE.md`.
  - Global link in Antigravity: `~/.gemini/config/GEMINI.md` and `~/.gemini/config/AGENTS.md`.
  - The Gemini rules file enforces core invariants and sets `CLAUDE.md` as the authoritative source.
3. Clean Repos Rule:
  - Never put `.agents/` or `GEMINI.md` inside project repositories (such as `StereoPipeline`).
  - Everything stays global in `~/.gemini/config/` to avoid git noise and broken checkout links.

## Idempotent Sync / Repair Script

To verify or repair the bridge on this machine or a new machine, run:
```bash
~/.claude/skills/gemini-claude-bridge/scripts/sync_bridge.sh
```

## Operational Workflow: Adding or Modifying Skills

Both Claude Code and Antigravity (Gemini) share these skills. To keep them synchronized and avoid broken configs:

1. Modifying an existing skill:
  - Edit the file directly in `~/.claude/skills/<skill-name>/SKILL.md`.
  - Because `~/.gemini/config/skills` is a filesystem symlink pointing directly to `~/.claude/skills`, modifications take effect immediately for both assistants.
  - No re-indexing or script run is required for simple edits, but ensure YAML frontmatter remains valid.

2. Creating a new skill:
  - Always create the directory and file in `~/.claude/skills/<new-skill>/SKILL.md`, never in `~/.gemini/config/skills/`.
  - YAML frontmatter must include `name:` and `description: >-` (folded block scalar).
  - Immediately run `~/.claude/skills/gemini-claude-bridge/scripts/sync_bridge.sh`. This script:
    - Validates frontmatter syntax for all skills.
    - Updates `~/.gemini/config/skills.json` so Antigravity registers the new skill.
    - Verifies the global symlinks.

## Frontmatter Requirements for Skills

All `SKILL.md` files must have valid YAML frontmatter:
- Avoid unquoted colons in single-line descriptions (e.g. `:ref:` or `gotchas:`).
- Use folded block scalars (`description: >-`) for multi-line or colon-containing descriptions.

