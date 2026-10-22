#!/usr/bin/env bash
# PreToolUse hook (matcher Bash, wired in ~/.claude/settings.json).
# When a Bash command touches pfe/Pleiades/Athena/lfe (an ssh/scp/rsync to a
# pfe/pfx/pfeNN/athfe/athena/lfe host, or a qsub/qstat/qdel), inject a reminder
# to load the pfe-nas skill FIRST - there is no trivial exception, even a lone
# ldd/ls/find/objdump probe is pfe work. Non-blocking: it only nudges, never
# denies (a wrong pfe command does not corrupt a repo the way a bare git op
# does, so a hard block is not warranted). Hands over the few facts that keep
# getting rediscovered by hand so a blind hunt is avoided.
# Reads the tool call as JSON on stdin; prints hook JSON only on a match.
cmd=$(jq -r '.tool_input.command // empty')

# ssh/scp/rsync aimed at a NAS host, or a PBS command.
if printf '%s' "$cmd" | grep -Eq '(^|[;&|[:space:]])(ssh|scp|rsync)[[:space:]].*(pfx|pfe[0-9]*|athfe[0-9]*|athena|lfe)([^a-zA-Z0-9]|$)' \
   || printf '%s' "$cmd" | grep -Eq '(^|[;&|[:space:]/])(qsub|qstat|qdel)([^a-zA-Z0-9]|$)'; then
  jq -n '{hookSpecificOutput:{hookEventName:"PreToolUse",additionalContext:"pfe/Pleiades/Athena/lfe work detected. Load the pfe-nas skill BEFORE continuing - no trivial exception, a lone ssh probe (ldd/ls/find/objdump) is still pfe work. Facts that keep getting dug for by hand: the working ASP build is ~/projects/BinaryBuilder/StereoPipeline (real tools in libexec/, reached via a SYMLINK a bare find will not descend, so a blind find for parallel_stereo misses it); the runtime env needs ISISROOT=/swbuild/oalexan1/miniconda3/envs/isis10asp plus $ISISROOT/lib on LD_LIBRARY_PATH; there is NO live asp_deps env (only asp_deps_stale_nfs; ISIS is isis10asp); head/front-end node = 1 thread / 1 process always."}}'
fi
exit 0
