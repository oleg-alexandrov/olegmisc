---
name: feedback_dry_test_qsub
description: Dry-test every qsub/PBS script on the pfe head node (~30s, monitor RAM/CPU, kill, wipe) before submitting the real job.
metadata:
  type: feedback
---

Before launching ANY script via qsub on pfe/Athena, DRY-TEST it on the head node
first - do not burn a queue+run to discover a trivial structural bug (a `cd` that
breaks relative paths, a missing input, a bad `-t` camera session, an arg typo).
These die in the first seconds.

**Why:** keeps recurring and wastes queue time + SBUs + momentum. Burned 2026-08-28:
a `cd v5` broke relative `data/...` paths, bundle_adjust exit 1, whole job dead - a
30s dry test would have caught it instantly.

**How to apply:** `setsid bash script.sh <args> >/dev/null 2>&1 &`, watch
`ps -o pid,rss,pcpu,comm -u $USER --sort=-rss` a couple times, KILL the whole tree
within ~30s (`pkill -KILL -u $USER -f '<toolnames>'`) - EARLIER if memory/CPU spikes
(head node OOMs + gate reaps you + emails). Read the script's `exec>` log (says what
it wrote) / `ls` the run dir. WIPE every produced file individually (rm run subdir by
literal absolute path). Only then submit the real qsub. In CLAUDE.md + [[pfe-nas]] skill.
