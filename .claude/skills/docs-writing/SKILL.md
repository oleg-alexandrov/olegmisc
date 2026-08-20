---
name: docs-writing
description: Documentation and prose writing - building ASP docs, RST formatting and :ref: vs :numref:, citing papers, NEWS.rst conventions, the say-once/short-sentence/words-to-avoid writing style, commit-message style, and how to show diffs. Load before writing or editing RST docs, NEWS entries, commit messages, PR/issue text, or any prose.
---

## Displaying Diffs and Changes

**ALWAYS show what you changed** - never make silent edits!

Use markdown diff blocks:
```diff
- old line
+ new line
```

## Building ASP Docs

`conda activate sphinx; make -C ~/projects/StereoPipeline/docs html` (output in
`docs/_build/html/`). Full build/cmake mechanics: `~/projects/cmake_build_notes.sh`.

## RST Documentation Formatting

**Documentation file locations:** check both `docs/` subdirectories and repository root level.
Cross-reference labels (`.. _foo:` targeted by `:numref:`foo``) OFTEN live in root-level
`.rst` files (ASP: `INSTALLGUIDE.rst`, `NEWS.rst`, `README.rst`, `install/INSTALLGUIDE.rst`),
NOT under `docs/`. So before calling a `:numref:` broken, grep the WHOLE repo for its label
(`git grep '^.. _foo:'`), not just `docs/`. Example: `:numref:`release`` resolves to
`INSTALLGUIDE.rst` at the repo root - it is NOT missing.

**Style:** Be concise - users are expert researchers. Give hints and pointers, not tutorials.

**Formatting rules:**
- Section underlines must be exactly the same length as heading text
  - **CRITICAL: Always count characters carefully - prone to off-by-one errors**
- Heading levels: `=` top, `-` subsection, `~` sub-sub, `^` sub-sub-sub
- **`:ref:` vs `:numref:` - name tools with `:ref:`, not `:numref:` (I keep getting this
  wrong).** `:numref:`geodiff`` renders "Section 16.26" (a NUMBER); `:ref:`geodiff``
  renders "geodiff" (the NAME). So to name a tool inline, use `:ref:` - "made with
  :ref:`point2dem`" reads "made with point2dem". NEVER "made with :numref:`point2dem`"
  (that reads "made with Section 16.56", which is nonsense in a sentence). Use `:numref:`
  ONLY to cite a section by its number ("see :numref:`cassis_ba`" -> "see Section 12.3"),
  or as a trailing parenthetical AFTER the plain word ("geodiff (:numref:`geodiff`)" ->
  "geodiff (Section 16.26)"). Verified by rendering the built HTML, 2026-07-17. Litmus:
  read the sentence with the ref replaced by "Section N" - if it reads wrong, use `:ref:`.

## Citing Papers in ASP Docs

ASP docs cite papers via `sphinxcontrib.bibtex` (configured in `docs/conf.py`,
`bibtex_bibfiles`). To add a citation:
1. Add a BibTeX entry to `docs/bibliography.bib` (the general reference bib).
   `docs/papersusingasp.bib` is ONLY for papers that USE ASP - do not put a
   cited work there. Use a short lowercase key (e.g. `alrousan98`). Brace proper
   nouns/acronyms so BibTeX keeps their case: `{DEM}`, `{SPOT}`.
2. Cite inline with ``:cite:`key` `` (renders a numbered, linked reference). It
   reads well right after the author names - "assessed by Al-Rousan and Petrie
   :cite:`alrousan98`", NOT "by (1998)" and NOT a bare "[1]" mid-sentence.
3. The reference list renders automatically from `docs/zzreferences.rst`
   (`.. bibliography:: bibliography.bib`); no per-doc bibliography directive is
   needed. Existing `:cite:` uses (e.g. in `bundle_adjustment.rst`) are the model.
Verify by building the docs: a missing entry warns "citation not found".

## NEWS.rst Conventions

**Release notes live in `NEWS.rst` at the repo root** (included by `docs/news.rst`).

- New items go in the **first section** ("Changes since last release"), never
  in older release sections below it.
- **CRITICAL: grep for all `^RELEASE` headers first** to find where the top
  section ends. Do NOT assume a large line number is still in the top section.
  The file has many `RELEASE X.Y.Z` headers and the top section may be short.
- Entries are grouped by tool name (e.g., `stereo_gui (:numref:`stereo_gui`):`)
  with bullet points underneath. Create a new tool group if one doesn't exist
  yet in the current section, or append a bullet to an existing group.
- The `Misc:` group always comes last in a section, after all tool entries.
- Keep bullets concise - one or two sentences with a numref link.

## Writing Style

**Say each thing once, in the one place it belongs, then point (CRITICAL,
recurring).** My drafts over-produce: a concept stated in an intro and again in
its own section, two cross-references where one does the job, a narrated outcome
("the crater should snap together", "the height difference is centered on zero"),
a doubled word ("residual disparity" for just "disparity"). None of these is
wrong. Each is one layer too many, and for an expert reader the value is in what
is cut. So: state each idea ONCE, in the section that owns it, then cross-ref -
do NOT foreshadow it earlier, restate it later, cite the same reference twice, or
describe what the reader is about to see. "Be concise" does not self-execute for
me (my sense of "enough" runs high), so run a deliberate SUBTRACTIVE pass before
showing any doc, comment, notes entry, commit, or PR: does this repeat something
above? is this cross-ref already made? am I narrating an expected result? Cut
what survives those questions. Applies everywhere - RST docs, code comments,
notes, commits, chat.

Never write "TL;DR" anywhere (notes, docs, chat, commits) - it is an ugly
macro-hack abbreviation. Use plain English: "Summary" (or just write the
summary). Keep summaries brief and to the point.

- Write SHORT sentences. Never join two sentences with a dash, an em dash, or a
  semicolon. Use a period and start a new sentence. Break long sentences up.
  Hyphens INSIDE a word are fine (model-based, cross-sensor). Applies everywhere:
  docs, notes, commits, chat (per Google/Microsoft/Apple style guides).
- Do not write three dots (an ellipsis). Use "etc." or just end the sentence.
- Do not use capital letters, underscores, asterisks, or any other markup to
  emphasize words (applies everywhere: code comments, docs, notes, commits,
  chat). Emphasis markup reads as shouting and clutters the text. Write plain
  prose and let word choice carry the emphasis. Real identifiers (PATH, NED,
  CSM, DOF) keep their normal casing.

**Words to avoid** (everywhere: code, comments, docs, notes, commits, chat). Use
plain English instead:
- "downweight" / "upweight" -> "give less weight" / "give more weight"
- "drape" / "draping" (a DEM used for mapprojection) -> "DEM for mapprojection",
  "mapprojection DEM", "mapproject onto the DEM". This is the universal ASP
  convention. Avoid "drape" everywhere - docs, code, and even our own notes. (When
  adding this rule, existing notes were left as-is by request; just do not write it
  going forward.)
- "tailable" -> "a log you can follow with tail" (not a real word)
- "downcase" / "upcase" -> "lowercase" / "uppercase"
- "honor" -> "respect", "obey", "use"
- "TL;DR" -> "Summary"
- "special casing" / "special-case" (as a verb) -> "handling as a special case",
  "a special case", "special-case handling" (noun). "special casing" is terrible
  English. And do NOT invent other analogous noun-to-verb coinages of the same
  shape (they read just as badly) - write the plain phrase instead.
When one of these is added here, also grep the projects `.sh` files and the ASP
and VW source and docs for it and fix existing occurrences.

**Informal INTERNAL terms and tools must NOT bleed into EXTERNAL / user-facing
docs (RST, published docs, PR and issue text). They stay valuable for our own
work; they just never ship.**
- "eyeball" -> "inspect" in external docs. Between you and me, "eyeball" is fine.
- "nuke" (as in remove/zero-out pixels) -> "remove", "mask out", "set to nodata"
  in external docs. Fine in our notes and chat, too casual for a shipped doc.
- Do NOT mention a "red/green overlay" (or "red/green hillshade overlay") in
  external docs. Describe the visual check plainly. The red/green (or red/blue,
  whatever helps you see the shift) overlay is a valuable INTERNAL inspection
  tool - keep using it for our work, just never name it in a shipped doc.
The rule is scoped: keep external docs professional; internal notes stay casual.

## Commit Message Style

Write like a human, not a robot. Short title; skip the body for trivial
changes. Avoid pedantic precision in the title:
- no quoted exact wording, no full function signatures, no `(file:foo.cc:123)`
- no parameter syntax with equals sign - "add csm parameter" not "add CSM= parameter"
- no `(#1234)` issue/PR number in the title - the body or PR cross-reference handles linking
- "fix changelog wording" beats `replace "plumbing" with "approach"`
- "added the csm parameter to campt" beats `Add CSM= parameter to campt (#6035)`
