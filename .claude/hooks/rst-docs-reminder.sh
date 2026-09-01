#!/usr/bin/env bash
# PreToolUse hook (matcher Edit|Write, wired in ~/.claude/settings.json).
# When an RST doc is about to be edited, inject a reminder to load the
# docs-writing skill, so the ASP :numref: label schema is not forgotten:
# labels are defined across many files, including the base-dir INSTALLGUIDE.rst
# and NEWS.rst (included into docs/ via stubs), not only under docs/.
# Reads the tool call as JSON on stdin; prints hook JSON only for .rst files.
f=$(jq -r '.tool_input.file_path // empty')
case "$f" in
  *.rst)
    jq -n '{hookSpecificOutput:{hookEventName:"PreToolUse",additionalContext:"You are editing an RST doc. Load the docs-writing skill. In ASP, :numref: labels are defined across many files, including the base-dir INSTALLGUIDE.rst and NEWS.rst, not only under docs/. Before calling a :numref: reference broken, grep the whole repo for its label."}}'
    ;;
esac
