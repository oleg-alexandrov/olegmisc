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

Recipient is always `oleg.alexandrov@gmail.com`. Transport is `msmtp` (same
mailer the nightly uses). Compose an RFC822 file (Subject header first, blank
line, then body - put full https URLs inline, Oleg copy-pastes them), then pipe
it in. Do NOT inline a multi-line body in a remote ssh string (nested quoting
eats newlines).
```bash
cat > /tmp/claude_mail.txt <<'MSG'
Subject: <one-line subject>
To: oleg.alexandrov@gmail.com

<body>
MSG
msmtp oleg.alexandrov@gmail.com < /tmp/claude_mail.txt   # Mac local msmtp (primary)
```
Mac msmtp = `/opt/homebrew/bin/msmtp`, config `~/.msmtprc` (perms 600). Fallback
if the Mac config breaks - l1's msmtp over ssh, piping the same file as stdin:
`ssh l1 '~/miniconda3/envs/gh/bin/msmtp oleg.alexandrov@gmail.com' < /tmp/claude_mail.txt`.
Exit 0 = accepted. Full detail: `~/projects/send_email_notes.sh`.

## ssh login banners (l1, pfe, athena) - the banner is on STDERR

These U.S. Government hosts print a long CUI banner on every login. The banner is
sent by the ssh CLIENT to its **local stderr** (an sshd pre-auth `Banner`; a remote
`~/.hushlogin` does NOT suppress it - that only hushes /etc/motd). The real command
output is on **stdout**. So the clean, principled fix is to drop the ssh client's
stderr - do NOT rely on fragile content-based grep filters:

- **Default:** `ssh pfe "cmd 2>&1" 2>/dev/null` - the INNER `2>&1` runs on the remote
  and folds the remote command's own stderr (tracebacks, warnings) INTO stdout, so
  you still SEE real remote errors; the OUTER `2>/dev/null` drops only the ssh-client
  banner. Clean stdout, real errors kept, no banner.
- **Stdout-only:** `ssh pfe "cmd" 2>/dev/null` - clean, but drops the remote command's
  real stderr too (fine when you only need stdout and detect failure by missing output).
- Verified 2026-08-27: split test proved 0 banner lines on stdout, all on stderr, for
  both forms. This beats the old `grep -vE 'information system|...'` hack, which is
  content-fragile and occasionally still leaks.

Batch all remote ops into ONE ssh call - MOTD/banner overhead is ~10s per call.
(Reaching pfe: use `ssh pfx`, NOT `ssh pfe`. Confirmed 2026-09-04: `ssh pfe` hits
the sfe bastion which rejects publickey and demands a SecurID passcode -
`Permission denied (publickey,keyboard-interactive)` - so it CANNOT run headless.
`pfx` keys straight through to a pfe node. mac_arm keys in headless via the
passwordless `~/.ssh/id_rsa` and needs no passcode.)

## NEVER add `-o BatchMode=yes` to these ssh probes (CRITICAL)

`mac_arm` and `pfx` authenticate with the passwordless `~/.ssh/id_rsa` key (there is
NO ssh-agent in the tool environment - `SSH_AUTH_SOCK` is empty). `-o BatchMode=yes`
suppresses that interactive/key exchange and the connection stalls after printing
only the login banner - which reads as "host unreachable / hangs" and sends you down
a false "can't reach pfe/mac" rabbit hole (burned 2026-09-04, wasted several probes
before dropping BatchMode). So: plain `ssh mac_arm '<cmd>'` / `ssh pfx '<cmd>'`, no
BatchMode. Bound it with an OUTER `timeout N ssh ...` on the l1 side (never inside the
remote cmd - see the Mac `timeout` trap above), and give the FIRST proxied connection
room: use `timeout 45`-`60`, not 6-8s - the initial hop through the proxy/sfe is slow
and a short timeout kills it before auth completes (another false-negative source).
To diagnose an ssh that "hangs", run `ssh -v <host> true` and read the auth lines
(`Offering public key`, `Authenticated ... using "publickey"`, or `Permission denied
(...keyboard-interactive)`) rather than guessing.
