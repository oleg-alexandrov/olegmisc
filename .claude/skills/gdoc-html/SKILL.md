---
name: gdoc-html
description: Writing a plain, self-contained HTML report for EXTERNAL consumption that Oleg pastes into a Google Doc - goal/method/verification/results write-ups with tables and figures. Load when asked for a "pasteable"/"external"/"Google Doc" report, or a plain HTML (no stylesheet) as opposed to the styled Artifact reports. NOT for the visual Artifact docs (those use full CSS + the Artifact tool).
---

## What this is (and is not)

Oleg periodically wants a **plain HTML write-up for external consumption** - goal,
method, verification, results, plans - that he **pastes into a Google Doc** himself
(he mirrors it; you do NOT edit Google Docs). This is the OPPOSITE of the styled,
CSS-heavy Artifact reports: here the constraint is Google-Docs paste fidelity, so
keep it PLAIN. Deliver a local `.html` file and `open` it in the browser for him to
copy-paste. Prior examples: `sdb_2026_08/wvgreen_ccd_report_plain.html`.

## The one rule that governs everything: Google Docs paste ignores CSS

When you select-all in a browser and paste into Google Docs, Docs keeps ONLY
semantic structure + **inline** style attributes. It **discards `<style>` blocks and
`class=` entirely**. So:
- Use semantic tags: `<h1>/<h2>/<h3>` (Docs maps them to Heading 1/2/3), `<p>`,
  `<b>`/`<strong>`, `<ul>/<li>`, `<table>`.
- Put any formatting you need as **inline `style="..."`** (e.g. `font-size` on a
  heading, `color:#777` to grey out a row). NEVER rely on a `<style>` block or classes.
- Wrap the body in one `<div style="font-family:Arial,Helvetica,sans-serif;font-size:11pt">`.
- Tables: `<table border="1" cellpadding="6" style="border-collapse:collapse;font-size:10.5pt">`
  with `<th align="left">`. The `border` attribute + inline `border-collapse` are what
  make the grid survive the paste.
- Figures: `<img src="data:image/jpeg;base64,...." style="width:100%;max-width:820px;height:auto">`.
  Base64 images **DO** transfer when copy-pasting the rendered page from a browser
  (Docs re-embeds them). Use JPEG (q~72-88, `sips -s format jpeg -Z 1500`) to keep size
  down - a figure-heavy report is many MB otherwise.

## How to build it

For a figure-heavy report, assemble with a small python script that base64-embeds the
JPEGs (cleaner than hand-writing giant `<img>` tags). Structure: `<div style=font...>`
wrapper, `<h1>` title, then sections. Put the KEY conclusion up front (Oleg: "say
upfront the strongest/most effective result"), then Method, Verification, Results
(tables + figures, all variants so he can pick), then Excluded/Future. Write real
flowing prose from his dictation, clean it up, be honest about caveats.

Then `open file.html` so it renders in his browser; he copies into the Google Doc.
No `<!doctype>`/`<head>` needed for paste, but a plain standalone file is fine.

## Gotchas
- Do NOT use the Artifact tool for these - that publishes a styled web page; this is a
  paste-into-Docs file. (The two are different deliverables; ask if unsure which.)
- No em dashes, no backticks-as-inline-code in the prose (Oleg style; use plain words
  or *italics* only where it helps).
- Keep one figure size cap (`max-width`) so the Doc doesn't get giant images.
- If he says "put all N results", show every variant/figure and let him choose which
  to keep - don't pre-trim.
