---
name: Batch ssh commands to pfx
description: Each ssh call to pfx has 5-10s overhead from MOTD/tunnel; batch ops into one ssh
type: feedback
originSessionId: 6edea641-7d58-4d85-a5e7-19cea61b1599
---
When investigating files on pfx (NAS), do not fire off one ssh per directory or file. The connection/MOTD overhead dominates wall time and frustrates the user.

**Why:** User flagged slowness 2026-04-14 when I did 5+ sequential `ssh pfx "ls ..."` calls to explore `sfs_clip_v1/` and `sfs_clip_v2/`. Each took ~10s including banner.

**How to apply:** Bundle all `ls`, `cat`, `find`, `grep` operations for a single investigation into ONE `ssh pfx 'bash -c "..."'` invocation. Use `printf "\n---LABEL---\n"` as a separator inside the remote command (NOT `===` - zsh on pfx parses that as path resolution and errors out). Write the remote command out as a heredoc or a multi-line script string, do all listings/cats/greps in one go, then parse the combined output locally.
