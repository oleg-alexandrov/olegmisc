#!/usr/bin/env bash
# PreToolUse/Bash guard: block ANY git command (read or write) that does not
# explicitly target a repo, either with a leading "cd /absolute/path &&" or with
# "git -C /absolute/path". Shell state does not persist between tool calls, so a
# bare git op runs in the home dir: a wrong-repo push reports a misleading
# "Everything up-to-date", and a wrong-repo read (status/log/rev-list/tag) returns
# a confident but wrong answer. Forcing an explicit absolute cwd kills both.

set -euo pipefail

input=$(cat)
cmd=$(printf '%s' "$input" | jq -r '.tool_input.command // ""')

# Any git invocation at all? ("git " as a command word, not a substring.)
if ! printf '%s' "$cmd" | grep -Eq '(^|[;&|[:space:](])git[[:space:]]'; then
  exit 0
fi

# Exempt truly cwd-independent git commands (they do not depend on which repo).
if printf '%s' "$cmd" | grep -Eq '(^|[;&|[:space:](])git[[:space:]]+(--version|version|--help|help|config[[:space:]]+--(global|system))'; then
  exit 0
fi

# Explicitly targeted and therefore safe:
#   git -C /absolute ...            (git names the repo itself)
#   cd /absolute ... && / ; git ... (an absolute cwd is set first)
if printf '%s' "$cmd" | grep -Eq 'git[[:space:]]+-C[[:space:]]+/'; then
  exit 0
fi
if printf '%s' "$cmd" | grep -Eq '(^|[;&|[:space:]])cd[[:space:]]+/[^;&|]*(&&|;)'; then
  exit 0
fi

reason='git command with no explicit repo target. Shell state does not persist between tool calls, so a bare git op runs in the home dir: a wrong-repo push reports a misleading "Everything up-to-date", and a wrong-repo read (status/log/rev-list/tag) returns a confident but wrong answer. Rewrite the command to start with "cd /absolute/path/to/repo &&", or use "git -C /absolute/path/to/repo".'
jq -nc --arg r "$reason" '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"deny",permissionDecisionReason:$r}}'
exit 0
