---
name: script-style
description: Formatting for shell and Python scripts we own (line length, no ASCII separators, no caps for emphasis, concise comments, no AI mentions). Load before writing or editing any .sh or .py script or ASP python tool.
---

# Script style

Rules for readable scripts (shell, Python, ASP python tools) and their comments
and printed or logged messages. Dense, decorated, shouty, over-explained code is
a pain to read. Lay the code out so it can be scanned (aligned, room to breathe),
and keep the comments short.

## Line length: at most 90 characters

Every line, comments included. Wrap a long comment or a long argument-default
message onto more lines rather than running past 90. This is the rule that most
improves readability, so hold it strictly.

Do not stuff a long explanation into a bash `${var:?message}` default. Put the
explanation in a short wrapped comment above and keep the message terse:

    # arg 13: num_matches_from_disp. 0 = DEM mode (build the DEM from existing
    # matches); >0 = dense mode (compute the pairing, emit that many matches per
    # pair, no DEM).
    num_matches_from_disp=${13:?num_matches_from_disp required}

## Align continuation lines

When a command is continued with trailing `\`, put every backslash in one column,
exactly one space past the longest content line. Do not pad out to some arbitrary
far column, and do not leave the backslashes ragged:

    multi_stereo                     \
      --mode dem_mosaic              \
      --image-list  images.txt       \
      --camera-list cameras.txt      \
      --output-prefix stereo/run

If one argument value would push that column far out (a long --stereo-options
string, say), assign it to a variable first and pass the variable, so the block's
lines stay short and the alignment stays tight.

Same idea for Python help strings built with `+ \`: put the `+ \` one space past the
longest fragment, and start each fragment at the same indent.

## No decorative separators

Never use runs of `-`, `=`, `#`, or `*` as a rule between paragraphs or sections,
in comments or in messages. Do not write `# ===== stage 6 =====` or
`echo "---- done ----"`. Use a single blank line between blocks, and a plain
short heading comment when a label helps:

    # stage 6: dense matches

For output, write a plain line: `echo "done"`.

## No capitals for emphasis

Do not upper-case words to stress them, in comments, messages, or log lines.
Write "dense mode", not "DENSE mode"; "emit", not "EMIT". Real identifiers and
acronyms keep their normal casing (PATH, DEM, CSM, NED).

## Comments: short, once

Say each thing once, briefly. Do not narrate reasoning or restate what the code
plainly shows. Cut a comment that repeats the line below it. Prefer one tight
sentence over three.

## No AI or internal-monologue in scripts

Never mention Claude, AI assistance, or the reasoning behind a choice in script
comments. That is internal monologue and does not belong in the file. This holds
for internal pipelines such as CassisPipeline. (Commit-message attribution is a
separate matter, governed by the repo's own rules.)

## Scope

Applies to scripts we own and edit: shell (.sh), Python, and the ASP python
tools, plus their comments and printed or logged output. It does not change
program option names or output filenames. For RST and other user-facing docs,
the docs-writing skill governs; this skill is for code.
