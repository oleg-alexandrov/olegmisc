---
name: qsub scripts need chmod +x on pfx after rsync
description: After rsync Mac to pfx of a .sh script, verify the pfx copy is executable; PBS exits 254 "permission denied" in ~2s if not.
type: feedback
originSessionId: 4e721443-3adf-4e71-8d76-772f811b9dfa
---
After `rsync` of a `.sh` script from Mac to pfx, always verify the pfx copy has the execute bit set (or pre-chmod +x on pfx). PBS will silently fail with `Exit_status = 254` in ~2 seconds when the script lacks +x, and the only surface symptom is a tiny walltime in `qstat -x`.

Observed 2026-04-21: `bundle_adjust_htdem_gcp_pncb.sh` was 755 on Mac, but rsync landed it as 644 on pfx (likely a umask / mode-mapping effect on NAS). Job 24390332 died in 00:00:02 with exit 254. After `ssh pfx chmod +x ...` resubmit succeeded.

**Why:** CLAUDE.md already documents this rule ("PBS fails with 'Permission denied' (exit 254) if the script lacks execute permission"), but the trap is that rsync does NOT always preserve the bit - NAS settings can strip it. Checking Mac permissions is not enough.

**How to apply:** After every `rsync ... pfx:.../*.sh`, run `ssh pfx 'chmod +x <path>'` or `ls -la <path>` to confirm. Before qsub of a fresh-rsynced script, `ssh pfx 'ls -la $script'` and verify `rwx` on the owner.
