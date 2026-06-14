---
name: feedback-logs-at-workdir-top
description: "When writing shell helper scripts that produce a log file, place that log at the top of the work dir (not buried in a subdir) so Oleg can ls/tail it without hunting."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 55347783-26a6-473a-9089-64a9489355e5
---

When a helper script (sfs/bundle_adjust/sim_align/dem2gcp/etc) wraps a CLI tool and redirects output to a log file, the log path should be at the **top of the work dir**, not nested inside the tool's output subdir.

Pattern:
  out=output_${jobName}.txt       # e.g. output_ba_lola.txt, output_matches.txt
  rm -fv $out
  tool ... >> $out 2>&1

Examples that follow this (good):
  ~/projects/sfs/dem2gcp.sh        -> output_<gcpFile-basename>.txt
  ~/projects/sfs/trans_gcp.sh      -> output_<gcpFile-basename>.txt
  ~/projects/sfs/batch_sfs_sim.sh  -> output_batch_sfs_sim_<beg>_to_<end>.txt
  ~/projects/sfs_1414A/ba_lola.sh  -> output_ba_lola.txt

**Why:** Oleg watches progress by `ls + tail -f` in the work dir. Logs buried under `<simDir>/<id>/output_log.txt` are hard to find at-a-glance.

**How to apply:** any new helper script's `out=...` line goes at the top of currDir. The actual product files (DEMs, GCPs, BA outputs) can stay nested in their respective subdirs - it's just the LOG that should be top-level.
