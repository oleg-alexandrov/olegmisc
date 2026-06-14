---
name: Wipe on pfx and git rm on Mac together
description: When wiping scripts from pfx, always also git rm the same paths from the Mac projects repo in one step - do not wait to be asked separately
type: feedback
originSessionId: 6edea641-7d58-4d85-a5e7-19cea61b1599
---
When cleaning up scripts in `~/projects/<subdir>/` on pfx, always follow the rm with a `git rm` on the Mac for the same paths. Do not ask separately; do it in the same ssh/local pair.

**Why:** Oleg reminded me 2026-04-15 during the sfs_mons_mouton cleanup. Asking each time after each pfx wipe is friction. The git state and the pfx state should stay in sync in one action.

**How to apply:**
1. rm the script on pfx (relative paths under the project dir, per CLAUDE.md).
2. On the Mac, `git ls-files --error-unmatch` to see which were tracked, then `git rm` them.
3. Leave the commit/push for the user to request - CLAUDE.md rules on not committing/pushing without explicit instruction still hold.
