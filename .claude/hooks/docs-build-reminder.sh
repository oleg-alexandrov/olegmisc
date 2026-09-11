#!/usr/bin/env bash
# PreToolUse hook (matcher Bash, wired in ~/.claude/settings.json).
# When a Bash command looks like an ASP docs build (sphinx-build, or a
# make html/latexpdf in a docs tree), inject the canonical build recipe so
# the sphinx env path and the standard output dir are not rediscovered by
# trial and error. Full conventions live in the docs-writing skill; this
# just hands over the command that keeps getting dug for.
# Reads the tool call as JSON on stdin; prints hook JSON only on a match.
cmd=$(jq -r '.tool_input.command // empty')
case "$cmd" in
  *sphinx-build*|*"make html"*|*"make "*"html"*|*"make latexpdf"*)
    jq -n '{hookSpecificOutput:{hookEventName:"PreToolUse",additionalContext:"ASP docs build detected. The base/anaconda3 sphinx lacks sphinxcontrib.bibtex; use the sphinx env and the standard output dir (docs-writing skill): from within docs/, run  ~/anaconda3/envs/sphinx/bin/sphinx-build -b html . _build/html  then grep the log for  WARNING|ERROR:|undefined label|citation not found  (empty = clean). Build into _build/html, not a custom dir. PDF/latexpdf is unsupported; HTML only."}}'
    ;;
esac
