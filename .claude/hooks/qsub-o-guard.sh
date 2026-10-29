#!/usr/bin/env bash
# PreToolUse/Bash guard: block an inline qsub submission that forgets -o, so PBS
# cannot drop a stray <jobname>.o<jobid> spool into /home6/oalexan1. The offender
# is always the ad-hoc one-off typed over ssh (carries -N but no -o); the framework
# scripts call qsub INSIDE the script (the Bash command line shows only
# "QSUB_BIN=.../qsub bash foo.sh", no -N), so they are not matched. Keying on
# "has -N but lacks -o" is the precise signal. See the pfe-nas skill, the "PBS spool
# output placement" rule.

set -euo pipefail

input=$(cat)
cmd=$(printf '%s' "$input" | jq -r '.tool_input.command // ""')

# A real qsub submission? Require a qsub invocation AND a -N job name on the command
# line. QSUB_BIN=.../qsub assignments and bare "bash foo.sh" (qsub inside) have no -N.
if ! printf '%s' "$cmd" | grep -Eq 'qsub[[:space:]]'; then
  exit 0
fi
if ! printf '%s' "$cmd" | grep -Eq '(^|[[:space:]])-N[[:space:]]'; then
  exit 0
fi

# Already targeted and therefore safe: an explicit -o, or a #PBS -o directive inline.
if printf '%s' "$cmd" | grep -Eq '(^|[[:space:]])-o[[:space:]/"'"'"']'; then
  exit 0
fi
if printf '%s' "$cmd" | grep -Eq '#PBS[[:space:]]+-o'; then
  exit 0
fi

reason='qsub submission with no -o. Over a non-interactive ssh the qsub cwd is /home6/oalexan1, so PBS drops its <jobname>.o<jobid> accounting spool straight into the home dir and litters it. Add -o "$W/" -j oe to the qsub (name the work dir, trailing slash), and cd "$W" before submitting. This holds for every qsub including a quick one-off - the worker doing its own exec > log does NOT exempt it, PBS still writes a separate spool. See the pfe-nas skill.'
jq -nc --arg r "$reason" '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"deny",permissionDecisionReason:$r}}'
exit 0
