---
name: nasa-mac-todo
description: "Open items on the NASA work Mac (nasa_mac, neon) after its 2026-11-18 setup; do them when the tunnel is back up."
metadata:
  node_type: memory
  type: project
  originSessionId: 322aa5b5-87b2-4a2b-bf1d-c9c9b818f7e3
  modified: 2026-11-18T20:36:32.846Z
---

nasa_mac setup (2026-11-18) is done except these, blocked when the reverse tunnel
(l1:5009) dropped:

- Delete the Miniforge installer: `/Users/oalexan1/Downloads/miniforge/Miniforge3-MacOSX-arm64.sh`,
  then `rmdir /Users/oalexan1/Downloads/miniforge`.
- Confirm Oleg finished `claude` login and `~/miniconda3/envs/gh/bin/gh auth login`
  (check with `gh auth status`).
- First real test of `bash ~/bin/tunnel.sh` on neon (the new neon case, direct to l1,
  needs the VPN). It replaces the hand-opened tunnel.

Tunnel down = `ssh nasa_mac` times out on port 5009. Ask Oleg to rerun tunnel.sh on
neon. Machine detail: [[machines-tools]].
