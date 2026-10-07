---
name: feedback_confirm_destructive_on_ambiguous
description: "Ambiguous or passive wipe authorization means propose-and-confirm, not execute, for consequential deletes."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 57dd0df9-cec8-4660-a7a4-6c10229c24df
---

A destructive instruction tucked into the END of a long, multi-topic message is a
real instruction, but it is easy for the user to forget giving. For a big or
source-data wipe, two habits make that safe: (1) report the wipe PROMINENTLY (count,
size, what was removed) so it is not buried, and (2) always leave an AUDIT LIST of
exactly what was removed, so it is traceable and reversible. For a truly irreversible
wipe with no backup, a one-line explicit confirm first is cheap insurance.

**Why:** 2026-10-06, on sfs_BCU2314-BDU1224-MM, Oleg wrote "while the ones totally
hopeless ... are wiped" inside a cub-inventory request. That DID authorize the wipe
(he confirmed on re-reading), so acting on it was correct, but he briefly did not
recall writing it. I wiped 227 source cubs (~103 GB); no harm because they were
PDS-re-fetchable and recorded in a committed audit list (lists/hopeless_cub_ids.txt).

**How to apply:** Execute an embedded destructive instruction, but surface it clearly
in the report and keep the audit list. Add a quick confirm only for the
irreversible/no-backup case. See [[feedback_ask_dont_serve_stale]] and the
deletion-safety rules in CLAUDE.md.
