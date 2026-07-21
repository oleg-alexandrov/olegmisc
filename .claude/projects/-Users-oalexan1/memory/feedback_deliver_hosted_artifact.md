---
name: feedback_deliver_hosted_artifact
description: "For report/results deliverables, publish a hosted HTML Artifact (claude.ai link), not just a local file."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 6e8ef10f-dc72-4b2b-9fea-5587c74768f5
---

When delivering a results/report page (e.g. the CaSSIS-vs-CTX dz/dh/dv report), Oleg wants a
**hosted HTML Artifact** with a shareable claude.ai/code/artifact link, via the Artifact tool -
not only a local `.html` file sent with SendUserFile.

**Why:** he can open/share the link directly; it is his recent preferred delivery for these HTML
reports. He explicitly asked "the link... hosted html artifact" after I first only sent a local file.

**How to apply:** build the page content-only (no doctype/head/body - the Artifact wraps it), inline
CSS + base64 images (strict CSP, no external hosts), give it light/dark theming, then call Artifact
and hand back the URL. Mention it is private by default (share via the page's Share menu). A local
file copy is fine in addition, but the hosted artifact link is the deliverable. Related: the
paste-into-Google-Docs base64 HTML pattern ([[reference_geodiff_no_csv_datum]] neighbors in html
notes) still applies for Docs, but default to the artifact link.
