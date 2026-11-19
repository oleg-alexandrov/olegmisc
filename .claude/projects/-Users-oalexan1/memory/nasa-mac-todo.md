---
name: nasa-mac-todo
description: "Open items on the NASA work Mac (nasa_mac, neon) after its 2026-11-18 setup; do them when the tunnel is back up."
metadata:
  node_type: memory
  type: project
  originSessionId: 322aa5b5-87b2-4a2b-bf1d-c9c9b818f7e3
  modified: 2026-11-19T04:24:10.318Z
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

- gh is still NOT logged in on neon (checked 2026-11-18 evening).
- neon keyboard/desktop (final, 2026-11-18): NO Option/Cmd swap (it broke iTerm
  Alt+arrows, removed). Screenshots go to the clipboard, area screenshot is
  Option+Shift+4 (hotkey 30), Chrome Ctrl+C/V via NSUserKeyEquivalents, iTerm
  Option+arrows = prev/next tab, native Spaces on Ctrl+arrows and Ctrl+1..4.
  AeroSpace fully removed (it cannot get Accessibility under Jamf). Detail in
  [[machines-tools]]. If NASA IT ever grants Accessibility (also wanted for
  Karabiner), revisit.
- `.claude/settings.json` on neon has iTerm 3.7's `it2 handle-agent-hook` hooks
  (Mac-only path). Do NOT push as is: l1/mac_arm would run a missing binary on
  every hook event. Keep it local, or make the hook path-guarded first.
