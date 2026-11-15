#!/usr/bin/env bash
# PreToolUse/Bash guard: deny any deletion that is not one literal absolute path. The logic
# lives in rm_guard.py (reads the hook JSON on stdin, prints a deny decision or nothing).
exec python3 "$HOME/.claude/hooks/rm_guard.py"
