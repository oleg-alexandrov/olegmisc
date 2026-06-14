---
name: Inlining commands through nested ssh mangles $vars and quotes
description: Multi-hop ssh (pfe21 -> athfe01) eats $N awk fields and shell metachars in inline commands; use a heredoc-written script file instead.
type: feedback
originSessionId: 37b98be3-16e5-4752-af1b-25aa07b6585d
---
When a command must run on a doubly-hopped host (e.g.
`ssh pfe21 'ssh athfe01 "..."'`), do NOT inline anything that uses `$N`
awk fields, shell vars that should not expand on the outer host, or
nested quoted args. The outer shell strips them before the inner shell
ever sees them. Symptom: awk reports `^ syntax error` because `$4`
became empty; geodiff prints its full `--help` because flag args got
mangled.

**Why:** lost two rounds today (geodiff vs LOLA + awk stats) before
giving up on inline form. Each retry over nested ssh is ~10 sec of
banner overhead.

**How to apply:** for any non-trivial command on a doubly-hopped host,
write the script to `/tmp/x.sh` via a heredoc with **quoted** terminator
(`<<"EOF"` or `<<'EOF'`) so `$` is preserved, then run `bash /tmp/x.sh`
in a single ssh hop. Example:

```bash
ssh pfe21 'cat > /tmp/run_$$.sh <<"EOF"
#!/bin/bash
cd /some/dir
awk -F, "NR>1 {s += $4} END {print s}" file.csv
EOF
bash /tmp/run_$$.sh; rm -f /tmp/run_$$.sh'
```

Trivial single commands (`ls`, `qstat`) inline through nested ssh just
fine. Rule of thumb: if it has `$N`, awk, or escaped quotes, write it
to a file.
