#!/usr/bin/env bash
# PreToolUse/Bash guard: block any git repo-mutating / push command that does not
# explicitly target a repo, either with a leading "cd /absolute/path &&" or with
# "git -C /absolute/path". Shell state does not persist between tool calls, so a
# bare "git push" silently runs in the home dir and reports a misleading
# "Everything up-to-date". Forcing an explicit absolute cwd kills that class of bug.

set -euo pipefail

input=$(cat)
cmd=$(printf '%s' "$input" | jq -r '.tool_input.command // ""')

# Does the command invoke a git subcommand that mutates the repo or pushes?
write_op_re='(^|[;&|[:space:](])git([[:space:]]+-{1,2}[^[:space:]]+)*[[:space:]]+(push|commit|merge|add|rm|reset|cherry-pick|rebase|revert|am|stash|checkout|restore|tag)([[:space:]]|$)'
if ! printf '%s' "$cmd" | grep -Eq "$write_op_re"; then
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

reason='git repo-mutating/push command with no explicit repo target. Shell state does not persist between tool calls, so a bare git op runs in the home dir (and a wrong-repo push reports a misleading "Everything up-to-date"). Rewrite the command to start with "cd /absolute/path/to/repo &&", or use "git -C /absolute/path/to/repo".'
jq -nc --arg r "$reason" '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"deny",permissionDecisionReason:$r}}'
exit 0
