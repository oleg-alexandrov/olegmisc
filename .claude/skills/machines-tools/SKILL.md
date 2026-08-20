---
name: machines-tools
description: Machine map and local tooling - the l1/Mac mini/pfe/Athena boxes with their build commands, ssh aliases and quirks, the common stereo_gui aliases (sg/sw/swa/sgm), and how to send email to Oleg via msmtp. Load when choosing a machine to run work on, using a viewing alias, or emailing Oleg.
---

## Machines

- **lunokhod1** (`l1`) - primary dev/build/git box (g++ 12.4 in `asp_deps`, 16
  cores). Build: `make -C ~/projects/StereoPipeline/build -j16`. Remotes:
  `origin`=fork, `god`=org.
- **Mac mini** (`ssh mac_arm`) - notes/docs machine + secondary build. **Always
  `make install`** (never bare `make`; installed libs go stale). Storage is
  tight - wipe stale `/tmp` cruft (never active/this-session work; if unsure ask).
  **No `timeout`/`gtimeout` on this Mac** - never wrap commands in `timeout` (it
  exits 127 "command not found", which silently looks like the wrapped command
  failed - cost a whole night of false "pfe down"). To bound an `ssh` probe use
  `ssh -o ConnectTimeout=N`. Detail: `~/projects/pleiades_notes.sh`.
  **This bites AGAIN with REMOTE commands: `ssh mac_arm 'timeout N <cmd>'` runs
  `timeout` ON THE MAC (none there) - it errors "command not found" and `<cmd>`
  never runs, which looks like `<cmd>` failed (falsely concluded a mac `git pull`
  couldn't reach github, 2026-08-10). Put any `timeout` on the l1 SIDE, wrapping
  the whole `ssh` (`timeout N ssh mac_arm '<cmd>'`), never inside the remote
  command. Same for any Mac-run script.**

- **Reach pfe with `ssh pfx`, NOT `ssh pfe`.** `pfx` is the ssh-config alias that
  hops through the sfe secure front end onto a pfe node (lands on e.g. pfe21) and
  works non-interactively (no SecurID prompt). `ssh pfe` goes through a different
  ProxyCommand that demands a 2FA passcode and fails non-interactively. So for ALL
  pfe access (probes, scp, running commands) use `ssh pfx` / `scp ... pfx:`. lfe is
  `ssh pfx` then `ssh lfe`.

- **Reachability check first (auto mode):** when a task depends on `l1` or
  `pfe`, probe them BEFORE committing to a plan (`ssh pfx` with `-o ConnectTimeout=8`).
  A dead host found mid-pipeline stalls an autonomous run. Cheap to test up front.

- **Athena / Turin** (another supercomputer, separate from Pleiades) - reach via
  **`ssh athfe01`** (..04); the hostname `athena` does NOT resolve. Model
  **`tur_ath`** (Turin, 256c), OWN scheduler, submit from athfe ONLY with
  **`/opt/pbs/bin/qsub`** (not `/PBS/bin/qsub`). `node_stats.sh` does NOT show the
  per-model Free table there - gauge load with `qstat`. **FULLY VISIBLE
  (confirmed 2026-08-07): `/nobackup`, `~/projects`, AND the ASP dev build
  (`~/projects/BinaryBuilder/StereoPipeline/bin`) are ALL visible from Athena
  compute nodes, so Athena runs ASP jobs EXACTLY like any pfe node - no data
  staging needed. It just has MORE cores per node (256 vs 28/40) and is MORE
  EXPENSIVE (higher SBU). Use it like any other node when you need throughput.**
  Single-node Athena: NO `--nodes-list` (ssh distribution to the HSN hostname is
  flaky and killed a job) - use `--processes`/`--threads-multiprocess` for local
  parallelism instead. Flaky historically; default to `bro_ele` for small work,
  reach for tur_ath when a big core count helps. Full detail + submit sample:
  `~/projects/pleiades_notes.sh` (athfe entry, "HOW TO FIND ATHENA").

Per-machine build commands, conda init, paths, the athfe tunnel hop, `/tmp`
triage: `~/projects/machines.sh` (and `install_asp_notes.sh`).

## Common Aliases

Full list in `~/.bash_aliases`. Viewing aliases/functions (`sg`, `sw`, `swa`, `sgm`) -
see the defs in `~/projects/aliases_notes.sh`. Quick:
- `sg` = `stereo_gui --window-size 1500 1000 --font-size 12` (view images/DEMs)
- `swa` = `sg -w --hide-all` (single-window overlay, start hidden)
- `sgm <min> <max> <files>` = stereo_gui colorbar view clamped to that range (geodiffs/DEMs)

## Sending Email to Oleg

How to email Oleg (msmtp; recipient oleg.alexandrov@gmail.com) is described in
`~/projects/send_email_notes.sh`.
