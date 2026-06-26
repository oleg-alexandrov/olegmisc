---
name: qsub-script-umask-022
description: "Every NAS/PBS qsub payload script must set umask 022 near the top or outputs come out 0600 (owner-only) under PBS's stricter default"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: d01f48d0-7a85-4b9b-a062-0009c0e7c94e
---

Every NAS/PBS qsub payload script must set `umask 022` near the top (first line after `set -e`). PBS jobs inherit a stricter default umask (0077) than interactive shells, so without it all outputs come out `-rw-------` (0600, owner-only) and collaborators cannot read them.

**Why:** 2026-06-26 my new cassis runner scripts (`cassis_dem2gcp_exp.sh`, `cassis_dem2gcp_s1b.sh`) and a repurposed `frame_stereo_mosaic.sh` all lacked `umask 022`, so their BA/mosaic outputs were 0600. Oleg caught it. The rule is recorded in `~/projects/qsub_convention.sh:127` ("first line after #!/bin/bash is umask 022") and `~/projects/qsub_rules.sh:114` - I just didn't open the checklist when authoring from memory.

**How to apply:** when writing OR repurposing any qsub script, run the `qsub_convention.sh` pre-flight checklist. The two non-negotiables: `umask 022` AND an `exec>` redirect to a tailable work-dir log (see [[qsub scripts should tee to a log file for real-time tail]]). To repair existing 0600 outputs: `chmod -R a+rX <dir>` (additive, safe on live files). Recoverable, unlike a lost PBS `-o` spool.
