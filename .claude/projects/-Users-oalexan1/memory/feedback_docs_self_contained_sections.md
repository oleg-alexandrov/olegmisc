---
name: feedback_docs_self_contained_sections
description: "In ASP/RST docs, make each section self-contained; repeat key info rather than relying on cross-refs."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: e1543a16-d620-4b39-9869-65c2aee48914
---

When writing ASP docs (RST), each section must stand on its own. Repeat key
warnings/rules (e.g. the `--max-displacement` "somewhat larger than expected
remaining displacement" point) in full in every relevant section, AND add a
`:numref:` citation to the canonical section for the deeper version. Do NOT
slim repetition down to a bare cross-reference.

**Why:** The reader does not have a "bot mind" holding the whole doc in context.
They jump straight to the one section matching their problem and read it in
isolation, in a hurry. If that single section alone doesn't answer them, the doc
"doesn't work" for them - a "see above" that assumes they read an earlier section
fails.

**How to apply:** Judgment call by size. For a short key point (a sentence or
two), repeat it in full in each section + add the citation - repetition across
unrelated sections is good, not redundant. Only when the repeated block grows
into a large/giant cloned paragraph does it pay to keep the detail in one
canonical section and have the others state the key point briefly and refer to it
for the full detail. Default to repeating; factor out only when the clone is big.
