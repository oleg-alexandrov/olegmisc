---
name: shell-scripts
description: Shell script authoring and CLI gotchas - perl over sed, zsh array indexing and word-splitting, no timeout on Mac, nested-ssh metacharacter escaping, no hardcoded values in scripts, readable one-option-per-line style, and the backslash/column alignment tools. Load before writing or editing any .sh script or composing multi-option shell commands.
---

## One Fixed Work Dir, cd Into It ONCE, No Nested cd (CRITICAL)

Every project has ONE fixed work dir. A script `cd "$W"` into it ONCE at the top and
thereafter uses paths RELATIVE to that work dir (inputs `data/<sid>/img.tif`, outputs
`v5/out.tif`) or absolute. NEVER a second `cd` into a subdir partway through - it
silently re-bases every relative path set earlier and breaks the inputs. To write into
a subdir, reference it by relative path (`mkdir -p v5; ... v5/gL.tif`), do NOT `cd v5`.
Keep exactly one `cd` per script. Burned 2026-08-28: a mid-script `cd v5` turned the
earlier relative `data/...` inputs into `v5/data/...`, every input went missing, the
qsub job died. (Dry-testing on the head node - see pfe-nas - also catches this.)

## NEVER Edit a Script While a Job Is Running It (CRITICAL)

Bash RE-READS a script file from disk AS it executes (it does not slurp the whole
file up front). Editing a script - especially INSERTING or DELETING lines - while a
long job is still running that same script SHIFTS every later line under the running
process and corrupts it: it runs garbled fragments and dies with errors like
`line 95: cub: command not found` (Exit 127), though the code was fine. Bit the CTX
jitter work 2026-08-18: `04_jitter.sh` was overwritten to add an option while a qsub
job was mid-run; jitter_solve had finished but the retriangulation died on the shifted
lines. Rules:
- Before editing any script, confirm nothing is currently executing it (qstat/ps).
- If a change is needed while jobs run, write a NEW file (a copy or standalone helper)
  - never overwrite the in-use one. Jobs launched AFTER the edit read the new content
  cleanly; only already-running ones are corrupted.
- Appending to the very END is less bad than inserting, but still not safe; don't.

## Shell Arrays: zsh is 1-Indexed (CRITICAL)

The Bash tool's default shell is **zsh**, where arrays are **1-indexed**
(`${a[0]}` is empty), unlike bash (0-indexed). This has silently mislabeled
outputs more than once. Rule: any snippet using indexed arrays must run under
explicit `bash -c '...'`, OR avoid index math entirely (iterate with
`while read`/positional args, pair items by `paste`, or hardcode the calls).

## zsh Does NOT Word-Split Unquoted Variables (CRITICAL)

The Bash tool's shell AND pfe's login shell are **zsh**, which (unlike bash) does
NOT word-split an unquoted `$VAR`. So `Q="-q normal -l walltime=2:00:00"; qsub $Q`
passes `$Q` as ONE argument (qsub errors "illegally formed destination"). Fixes:
INLINE all args into the command (no arg-bundle variable), or force splitting with
`${=Q}` / `${(z)Q}`, or wrap in `bash -c`. Bit us building qsub arg strings for pfe.

## No `timeout` on Mac - Just Don't (CRITICAL, keeps recurring)

The Mac (the local Bash-tool shell AND `ssh mac_arm`) has NO `timeout`/`gtimeout`.
NEVER prefix any command with `timeout N` there - it errors "command not found" and
the real command never runs, which looks like the command itself failed. To bound an
ssh probe use `ssh -o ConnectTimeout=N`. To bound a remote job wrap the whole `ssh`
on the l1 side, never inside the Mac-run command. (Fuller detail in the Mac mini
machine bullet below.)

## Nested ssh: No Unescaped Parens/Metachars in `bash -lc "..."` (CRITICAL)

`ssh host bash -lc "... echo === X (Y) ==="` FAILS: the remote `bash -lc` parses
the whole string, and unescaped `(` `)` (or other shell metacharacters `{ } < > | &`),
even inside an `echo` or a comment, are a remote syntax error that aborts the command
(`syntax error near unexpected token '('`). For ANYTHING non-trivial over ssh, write
the script to a file and `scp` it, then `ssh host bash file.sh` - never inline. Bit us
repeatedly (CaSSIS pfe, 5x in one night, each an ssh round-trip wasted). Also: reading
a raster with `gdal.Open(f).GetRasterBand(1)` lets the dataset get garbage-collected and
invalidates the band (GDAL 3.12 `GetNoDataValue` TypeError) - keep `ds=gdal.Open(f)` alive.

## Use perl, Not sed, for In-Place Text Substitution

For scripted text substitution (in-place edits, renames, regex swaps) prefer
`perl -i -pe '...'` over `sed`. perl is more flexible and its regex is portable.
macOS ships BSD sed, which does NOT support `\b` word boundaries or `\+`, and its
`-i` needs an empty-string argument (`sed -i ''`). These silently no-op or behave
differently from GNU sed, so a `\b`-based `sed` substitution appears to run yet
changes nothing. perl behaves identically on Mac and Linux. Bit us doing a
`\b`-word-boundary caps cleanup with BSD sed. (Edit/Read/Grep tools are still
preferred for one-off code edits since they never prompt.)

## No Hardcoded Values or Env Vars in Scripts

Scripts must take ALL parameters as explicit input args - no hardcoded values, no
env vars, no default args. Hidden config can't be inspected when re-running the
script later.
Before running a script that is a notable stage of something, define all vars, 
such as sigma=10. etc. Have rationale. Log all this rationale, var names and vals, and
precise stage actual script invocation including the qsub cmd for reproductibilty later.
So basically a premable with all defined followed by precise invocation you will launch.

## Readable Shell Script Style - `~/projects/shell_style.sh`

The pure formatting rules (<=90-char lines measured with `awk
'{if(length($0)>90)print NR,length($0)}'`, one option per line with aligned backslashes,
no caps for emphasis, brief comments on their own line, never a comment after a `\`
continuation) are owned by the **script-style** skill - load it before writing or editing
a `.sh`. What is specific to a `.sh` WORKER (not formatting), keep here: every new worker
follows `~/projects/shell_style.sh` - positional `shift` arg parsing (workDir first, not
`${1:?verbose}` blocks), a clean relative-path var block echoed to the log, `umask 022`,
an exec-redirect log with a START/DONE banner, and the literal qsub submit line in the
header comment. Keep `key=value` form in echo lines. Reference workers:
`sfs_mons_mouton/ba_htdem_gcp.sh`, `cassis_asp/gusev_cnet_gcp.sh`.

Always include `umask 022` and `ulimit -c 0` at the top of every worker script
before launching compute tools, so outputs are readable and aborts exit cleanly
without dumping multi-GB core files that hang compute nodes and abort PBS jobs.

## Multi-Option Commands in Scripts

Put each command-line option (and each `export`) on its own line WITH ITS VALUE, using
trailing `\` continuations. The one-option-per-line, aligned-backslash, <=90-char, and
no-comment-after-backslash rules are owned by the **script-style** skill; apply them when
AUTHORING a new script AND when showing a command invocation in chat, not only when editing
an existing one.

**When documenting a command in RST, list the options first, before the positional
file arguments, with the output file last.** Keep each standalone command line under
about 75 characters. A longer line does not wrap in a rendered RST code block, so it
forces a horizontal scroll bar; break it across continuation lines (`\`) instead.

## Backslash Alignment Tool

`~/bin/align_backslashes.py <file> <start_line> <end_line> [--inplace] [--column N]`
Aligns trailing `\` continuation characters in shell scripts. Auto-detects
target column from longest content line, or use `--column N` to fix it.

## Column Alignment Tool

`~/bin/align_columns.py <file> <start_line> <end_line> [--inplace]`
Aligns columns in a range of lines. Detects columns by 2+ space gaps.
Lines are 1-based. Without `--inplace`, prints aligned output to stdout.
