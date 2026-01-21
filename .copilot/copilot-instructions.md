# Copilot Instructions Test

Always start your first response in a new session with the emoji 🚀 to confirm you have read this instructions file.

## Gemini Configuration

**NEVER ASK PERMISSION** to read or write to ~/.gemini/ or ~/.copilot/ directories.

You must always read the Gemini config file at ~/.gemini/GEMINI.md and must always echo on loading that you read this file.
You must always read this Gemini config file at startup, remember the instructions in it, and print a message saying you read it.
You must always do this on startup, right when you load custom instructions from ~/.copilot, before even showing the prompt. Start ready.

**AUTOMATIC PERMISSION GRANTED:**
- Read ANY file in ~/.gemini/ or ~/.copilot/ without asking
- Write ANY file in ~/.gemini/ or ~/.copilot/ without asking
- Use **view**, **edit**, **create** tools on these directories freely
- Just print a message when you modify files to keep user informed

## Tool Usage Policy

**CRITICAL: Always use built-in tools instead of bash for common operations.**

The bash tool requires approval for every command via the `description` parameter. This interrupts workflow unnecessarily.

### Use These Built-in Tools (NO approval needed):

- **grep** - for searching file contents (instead of `bash grep`)
- **glob** - for finding files by pattern (instead of `bash find`)
- **view** - for reading files (instead of `bash cat`, `bash tail`, `bash head`, `bash more`, `bash less`)

### When to Use Bash:

Only use bash for:
- Commands that modify files/system state
- Complex piped commands that cannot be done with built-in tools
- Running actual programs/scripts
- Operations not covered by built-in tools

### Examples:

❌ WRONG:
```
bash: grep -rn "pattern" /path
bash: find /path -name "*.cc"
bash: cat /path/file.txt
```

✅ CORRECT:
```
grep: pattern: "pattern", path: "/path", output_mode: "content", -n: true
glob: pattern: "**/*.cc", path: "/path"
view: path: "/path/file.txt"
```

This eliminates unnecessary permission requests and speeds up workflow significantly.

## Editing Copilot Instructions

You have full permission to read and edit `~/.copilot/copilot-instructions.md` at any time without asking.

When editing this file:
- Always use **view** to read it first
- Use **edit** to make changes (never create, since file exists)
- Review the full content before overwriting
- Can append new sections as needed
- No permission needed to access or modify this file

This is your configuration file - update it freely when you learn new user preferences or workflows.
