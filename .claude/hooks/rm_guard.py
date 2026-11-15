#!/usr/bin/env python3
# rm_guard.py - PreToolUse/Bash guard: deny any deletion that is not one literal absolute path.
# Covers rm, rmdir, unlink, git rm (except --cached), find -delete / -exec rm, xargs rm, and
# Python deletions run from the shell (shutil.rmtree, os.remove, os.unlink, os.rmdir, unlink()),
# locally, inside ssh quotes, or written into a heredoc script. Every path argument must start
# with "/" and hold no $, backtick, glob, ~ or brace: an empty or wrong variable in a delete path
# removes the wrong tree, and a reviewer cannot verify such a command at a glance.
import json
import re
import sys

cmd = json.load(sys.stdin).get("tool_input", {}).get("command", "") or ""
bad_chars = re.compile(r"[$`*?~{}\[\]]")
sep = re.compile(r"[;&|\n]")
# A command word starts a shell segment: line start, after ; & | ( or a quote/backtick (ssh,
# $(...)), optionally after sudo/nohup/exec/time/command. Prose words mid-sentence do not match.
word_start = r"(?:^|(?<=[;&|(`'\"\n]))[ \t]*(?:(?:sudo|nohup|exec|time|command)[ \t]+)*"
problems = []

# Python deletions run from the shell cannot be checked for literal paths.
if re.search(r"(?:^|[;&|(\s`'\"/])python[0-9.]*\s", cmd) and \
   re.search(r"shutil\.rmtree|os\.remove|os\.unlink|os\.rmdir|os\.removedirs|\.unlink\(|send2trash", cmd):
  problems.append("a Python deletion (shutil.rmtree / os.remove / os.unlink / unlink())")

# xargs feeds unknown paths to rm.
if re.search(r"\|[ \t]*xargs\b[^;&|\n]*\b(rm|rmdir|unlink)\b", cmd):
  problems.append("xargs rm (paths not visible)")

# rm / rmdir / unlink, or git [-C dir] rm, as a command word.
for m in re.finditer(word_start + r"(?:git[ \t]+(?:-C[ \t]+\S+[ \t]+)?)?(rm|rmdir|unlink)(?=\s)", cmd):
  seg = sep.split(cmd[m.start(1):], maxsplit=1)[0]
  seg = seg.replace("'", " ").replace('"', " ")
  toks = seg.split()
  if "git" in cmd[m.start():m.start(1)] and "--cached" in toks:
    continue
  if bad_chars.search(seg):
    problems.append("'%s' with a variable, glob, ~ or brace" % seg.strip())
    continue
  args = [t for t in toks[1:] if not t.startswith("-")]
  rel = [a for a in args if not a.startswith("/")]
  if rel:
    problems.append("'%s' with a relative path (%s)" % (seg.strip(), " ".join(rel)))

# find ... -delete / -exec rm: the search roots must be literal absolute paths.
for m in re.finditer(word_start + r"(find)\s", cmd):
  seg = sep.split(cmd[m.start(1):], maxsplit=1)[0]
  if not re.search(r"-delete\b|-exec(dir)?\s+(rm|rmdir|unlink)\b", seg):
    continue
  toks = seg.replace("'", " ").replace('"', " ").split()[1:]
  roots = []
  for t in toks:
    if t.startswith("-") or t in ("(", "!"):
      break
    roots.append(t)
  bad = [r for r in roots if not r.startswith("/") or bad_chars.search(r)]
  if not roots or bad:
    problems.append("find delete with a non-literal or relative root (%s)" % (" ".join(roots) or "none"))

if problems:
  reason = ("BLOCKED by rm-guard: " + "; ".join(problems) + ". A delete must name ONE literal "
            "absolute path per rm (no $VAR, glob, ~, brace, cd, relative path, xargs or Python "
            "deletion). Prefer not deleting at all (the scratchpad cleans itself). If a wipe is "
            "really needed, hand a subagent the literal absolute paths. This is a HARD STOP: do "
            "not retry a reworded variant, report it to Oleg.")
  print(json.dumps({"hookSpecificOutput": {"hookEventName": "PreToolUse",
                                           "permissionDecision": "deny",
                                           "permissionDecisionReason": reason}}))
