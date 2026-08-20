---
name: shell-scripts
description: Shell script authoring and CLI gotchas - perl over sed, zsh array indexing and word-splitting, no timeout on Mac, nested-ssh metacharacter escaping, no hardcoded values in scripts, readable one-option-per-line style, and the backslash/column alignment tools. Load before writing or editing any .sh script or composing multi-option shell commands.
---

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

## Readable Shell Script Style - `~/projects/shell_style.sh` (CRITICAL, no reminders needed)

**When rewriting or revisiting ANY script, OFFER to bring it to the preferred
style below** (do not silently leave it ugly). The single worst offense: a
lengthy comment placed AFTER code on the same line, spilling across continuation
lines. NEVER do that. A comment is BRIEF and goes on its OWN line(s) BEFORE the
code; a tiny same-line note on a var (`nprocs=$1; shift  # per node`) is fine.
Keep `key=value` form in echo lines (easy to read); otherwise avoid verbosity.
The preferred style in one line: positional `shift` args (workDir first), clean
relative-path var block echoed to the log, `umask 022`, exec-redirect log with
START/DONE banner, one option per line with aligned backslashes, the literal qsub
line in the header, no line over 90 chars, brief up-front comments.

EVERY new `.sh` worker follows the readable style in `~/projects/shell_style.sh`:
positional `shift` arg parsing (not `${1:?verbose}` blocks), a clean relative-path
var block echoed to the log, `umask 022`, exec-redirect log, one option per line
with aligned backslashes, and the literal qsub submit line in the header comment.
HARD rules Oleg keeps reminding on (just do them): NO line over 90 chars, code or
comment - measure with `awk '{if(length($0)>90)print NR,length($0)}'`, never
eyeball, and break the long ones (split a long multi-var `export` into separate
lines). A big comment goes on its OWN line(s) BEFORE the code, NEVER as a trailing
right-side comment that wraps across many lines (a short single-line trailing note
on a var is fine). Readable, human, not verbose/ugly. Reference workers:
`sfs_mons_mouton/ba_htdem_gcp.sh`, `cassis_asp/gusev_cnet_gcp.sh`.

## Multi-Option Commands in Scripts

In shell scripts, put each command-line option on its own line, WITH ITS VALUE
on that same line: `--option val \`. One option per line, never several options
on one line, and never split an option from its value. Same for each `export`.
Use trailing `\` continuation backslashes (single space before the `\`, matching
the surrounding scripts; or align to one column with the backslash alignment tool
below where that reads tidier). This applies when AUTHORING a new script and when
showing a command invocation in chat, not only when editing an existing script -
it recurred (a proposed bundle_adjust block bunched options onto one line), so
default to one-option-per-line for every multi-option command, everywhere.

**Comment lines in scripts never exceed 90 characters.** Wrap a longer comment
onto continuation comment lines. Measure line length with a tool (e.g. `awk
'{if(length($0)>90)print NR,length($0)}'`), never eyeball it.

**NEVER put a comment after a `\` line-continuation** (`cmd \  # note`): the `\`
escapes the trailing space, the `#...` is a comment, and the command ENDS there
(continuation broken). This applies to scripts AND to paste-able commands shown
to Oleg. Keep comments on their own lines, or omit them.

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
