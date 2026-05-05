#!/bin/bash
# tunnel.sh - SSH tunnel setup for Mac <-> pfx (NAS) <-> lunokhod1

# Establishes a VPN-free path from Mac to lunokhod1 via a NAS pfe node. Run this
# script with no arguments on l1 first, then on Mac. Both tunnels must be active
# for the full chain to work: Mac -> pfx -> l1.

# The chain:
#   Mac --> pfx (pfe21) --> lunokhod1

#   l1 creates an outbound SSH to pfx with -R (reverse tunnel), so pfx
#   can reach l1 through the already-established connection. No inbound
#   firewall rules needed on l1.

#   Mac creates an outbound SSH to pfx with -L (forward tunnels), so
#   Mac can reach both pfx and l1 through pfx.

# Why this works without VPN:
#   l1 can SSH outbound to NAS (allowed by NDC firewall).
#   Mac can SSH to NAS (via the internet, through sfe gateway).
#   The reverse tunnel from l1 to pfx allows pfx to connect back to l1
#   over the existing outbound SSH connection. No new inbound connection
#   to l1 is created, so the NDC firewall is not involved.

# IMPORTANT: pfe vs pfx
#   "pfe" is a load-balanced relay that lands you on pfe21, pfe22, etc.
#   Reverse tunnels live on ONE specific node. If you SSH to "pfe" and
#   land on a different node, localhost:PORT won't work. We use "pfx"
#   as our alias for pfe21. All machines must use the SAME node.

# Auth setup (one-time):
#   The reverse tunnel terminates at l1's sshd, so the connecting side
#   (pfx or Mac) needs a private key that l1's authorized_keys accepts.
#   l1's own key is used for this:
#     From l1: scp ~/.ssh/id_rsa pfx:~/.ssh/id_rsa_l1
#     From Mac: scp pfx:~/.ssh/id_rsa_l1 ~/.ssh/id_rsa_l1
#   Then ~/.ssh/config on pfx and Mac must have:
#     IdentityFile ~/.ssh/id_rsa_l1
#   in their Host l1 entry.

# SSH keep alive setup:
#   ServerAliveInterval 60 + ServerAliveCountMax 300 (in ~/.ssh/config
#   Host * block) keeps the tunnel alive for up to 5 hours of network
#   interruption. The tunnel can live for days/weeks as long as neither
#   machine reboots and no admin kills the sshd process.

# Port assignments:
#   5955 - l1's sshd exposed on pfx (reverse tunnel from l1)
#   6078 - pfx's sshd exposed on l1 (forward tunnel from l1)
#   3102 - pfx's sshd exposed on Mac (forward tunnel from Mac)
#   3079 - Mac's sshd exposed on pfx (reverse tunnel from Mac)
#
# SSH config note for l1:
#   mac_arm lives on pfx (port 3079), not on l1. So l1's ~/.ssh/config
#   needs "ProxyJump pfx" and Port 3079 in the mac_arm entry to chain
#   through pfx. Without ProxyJump, ssh mac_arm from l1 won't work.
#
# Usage: bash ~/bin/tunnel.sh
#   Or with port overrides: L1_SSHD_ON_PFX=5960 bash ~/bin/tunnel.sh
#
# Athfe fallback (Mac only): when pfe is down (e.g. monthly patching), run
#   USE_ATHFE=1 bash ~/bin/tunnel.sh
# to skip the tunnel and point Host pfx at athfe01.nas.nasa.gov:22 directly.
# pfe and athfe share the NAS filesystem, so all rsync/ssh paths in scripts
# still resolve to the same files. Re-run plain `bash ~/bin/tunnel.sh` to
# restore the normal tunnel route once pfe is back.

# Port assignments (single source of truth)
# Override any of these with environment variables before running.
L1_SSHD_ON_PFX=${L1_SSHD_ON_PFX:-5955}
L1_TO_PFX=${L1_TO_PFX:-6078}
MAC_SSHD_ON_PFX=${MAC_SSHD_ON_PFX:-3079}
MAC_TO_PFX=${MAC_TO_PFX:-3102}

# pfx = our specific pfe node. All tunnels go here. Don't use generic "pfe".
PFX_HOST=pfe21.nas.nasa.gov

# Keep tunnels alive and detect dead connections quickly
SSH_ALIVE="-o ServerAliveInterval=30 -o ServerAliveCountMax=3"

HOST=$(hostname)

kill_port() {
  local port=$1
  local pids
  pids=$(lsof -ti :$port 2>/dev/null)
  if [ -n "$pids" ]; then
    echo "Killing stale processes on port $port (PIDs: $pids)"
    echo "$pids" | xargs kill 2>/dev/null
    sleep 1
  fi
}

update_remote_port() {
  # Usage: update_remote_port <ssh_target> <host_alias> <port>
  # Uses set_port.py on the remote machine to update its ~/.ssh/config
  local target=$1
  local alias=$2
  local port=$3
  echo "Updating $alias=$port on $target"
  ssh "$target" "python3 ~/bin/set_port.py $alias $port" 2>/dev/null ||
    ssh "$target" "python ~/bin/set_port.py $alias $port" 2>/dev/null ||
    echo "Warning: could not update port on $target"
}

case "$HOST" in
  *Mac-mini*|*Olegs-Mac*)
    if [ "${USE_ATHFE:-0}" = "1" ]; then
      echo "Mac mini: athfe fallback mode (pfe assumed down)"
      echo "Pointing Host pfx at athfe01.nas.nasa.gov:22 directly. No tunnel."
      ~/bin/set_pfx_target.py athfe
      echo ""
      echo "Done. ssh pfx and rsync ... pfx:... now go to athfe01"
      echo "(same NAS filesystem as pfe). Mac<->l1 chain is NOT restored;"
      echo "wait for pfe to come back for that. To restore normal route:"
      echo "  bash ~/bin/tunnel.sh"
      exit 0
    fi

    echo "Mac mini: connecting to pfx"

    # Set up forward tunnel to pfx (so we can talk to pfx directly)
    # and reverse tunnel so pfx (and thus others) can reach us
    kill_port $MAC_TO_PFX

    echo "Connecting to $PFX_HOST and creating tunnels:"
    echo "  Local:  localhost:$MAC_TO_PFX -> pfx:22 (forward)"
    echo "  Local:  localhost:$L1_SSHD_ON_PFX -> pfx:$L1_SSHD_ON_PFX (forward, chains to l1)"
    echo "  Remote: pfx:$MAC_SSHD_ON_PFX -> Mac:22 (reverse)"

    ssh $PFX_HOST -N -f $SSH_ALIVE \
      -L ${MAC_TO_PFX}:localhost:22 \
      -L ${L1_SSHD_ON_PFX}:localhost:${L1_SSHD_ON_PFX} \
      -R ${MAC_SSHD_ON_PFX}:localhost:22

    if [ $? -ne 0 ]; then
      echo "ERROR: Failed to connect to pfx. Check your SSH keys and network."
      exit 1
    fi

    echo "Tunnels up. Updating ssh config on this machine."

    # Update local ssh config: pfx points to pfx via our forward tunnel.
    # Use set_pfx_target.py (not set_port.py) so Hostname is also restored
    # to localhost in case it was previously switched to athfe fallback.
    ~/bin/set_pfx_target.py tunnel $MAC_TO_PFX

    # Update pfx's config so it knows how to reach this Mac
    ssh pfx "python3 ~/bin/set_port.py mac_arm $MAC_SSHD_ON_PFX" 2>/dev/null

    # Set up l1 so Mac can reach it through pfx's reverse tunnel
    ~/bin/set_port.py l1 $L1_SSHD_ON_PFX

    echo ""
    echo "Done. Available connections:"
    echo "  ssh pfx       -> pfx ($PFX_HOST)"
    echo "  ssh l1        -> lunokhod1 (via pfx, needs l1 tunnels running)"
    echo ""
    echo "From pfx: ssh mac_arm -> this Mac (reverse tunnel)"
    echo ""
    echo "Note: 'l1' only works after running tunnel.sh on lunokhod1."
    ;;

  *lunokhod1*)
    echo "Lunokhod1: setting up reverse tunnel through pfx"

    echo "Connecting to $PFX_HOST and creating tunnels:"
    echo "  Reverse: pfx:$L1_SSHD_ON_PFX -> l1:22"
    echo "  Forward: localhost:$L1_TO_PFX -> pfx:22"

    kill_port $L1_TO_PFX

    ssh $PFX_HOST -N -f $SSH_ALIVE \
      -R ${L1_SSHD_ON_PFX}:localhost:22 \
      -L ${L1_TO_PFX}:localhost:22

    if [ $? -ne 0 ]; then
      echo "ERROR: Failed to connect to pfx. Check SSH keys and network."
      exit 1
    fi

    echo "Tunnels active on pfx ($PFX_HOST)."

    # Update local ssh config first so 'ssh pfx' uses the forward tunnel
    ~/bin/set_port.py pfx $L1_TO_PFX

    # Update pfx's ssh config via the forward tunnel (avoids second NAS auth)
    update_remote_port "pfx" l1 $L1_SSHD_ON_PFX

    echo ""
    echo "Done. From l1:"
    echo "  ssh pfx       -> pfx ($PFX_HOST) via forward tunnel"
    echo ""
    echo "From pfx ($PFX_HOST):"
    echo "  ssh l1"
    echo ""
    echo "From Mac (after running tunnel.sh on Mac):"
    echo "  ssh l1"
    ;;

  *)
    echo "Unknown host: $HOST"
    echo "Expected hostname containing: Mac-mini or lunokhod1"
    echo "If this is a new machine, add a case for it in tunnel.sh."
    exit 1
    ;;
esac
