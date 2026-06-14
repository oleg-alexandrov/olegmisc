---
name: qsub scripts should tee to a log file for real-time tail
description: Redirect script output to a known log file path so progress can be tailed while the PBS job runs, instead of hiding it or waiting for PBS stdout at job end
type: feedback
originSessionId: 6edea641-7d58-4d85-a5e7-19cea61b1599
---
When writing qsub-submitted scripts that loop or parallelize, redirect stdout/stderr to a project-dir log file (not /dev/null, not only PBS stdout). This lets the user `tail -f` the log for live progress instead of waiting for job end.

**Why:** Oleg flagged 2026-04-15 that a parallelized geodiff job was opaque because per-command output was sent to /dev/null and PBS stdout only appears at job end. No visibility while the ~162 xargs tasks ran.

**How to apply:** In scripts submitted via qsub, write a known log path like `${currDir}/gdiff_all2.log`, then:

```bash
# inside the worker function
do_pair() {
    ...
    { geodiff ... && colormap ... ; } >> "$LOGFILE" 2>&1
    echo "done: $pref" >> "$LOGFILE"
}
```

Or have the top-level script do `exec >> "$LOGFILE" 2>&1` at the top so all stdout/stderr funnels there. Tell the user the log path so they can `tail -f`. PBS stdout (.o file) still serves as a final record - the .log is complementary for live visibility.
