#!/bin/bash
# tunnel.sh - SSH tunnel setup for Mac <-> pfx <-> laptop <-> lunokhod1
#
# Detects which machine it runs on and does the right thing.
# Run on laptop first (must be on VPN), then on Mac.
#
# The chain: Mac --> pfx --> laptop --> lunokhod1
#   - pfx is our alias for a SPECIFIC pfe node (pfe22)
#   - laptop is the bridge to lunokhod1 (only laptop can reach it, via VPN)
#   - lunokhod1 cannot initiate tunnels (no reverse forwarding from behind firewall)
#
# IMPORTANT: pfe vs pfx
#   "pfe" is a load-balanced relay that lands you on pfe21, pfe22, pfe23, etc.
#   Reverse tunnels are created on ONE specific node. If you SSH to "pfe" and
#   land on a different node, localhost:PORT won't work (tunnel is elsewhere).
#   We use "pfx" as our alias for a specific node (pfe22) to avoid this mixup.
#   All machines must connect to the SAME pfx node for tunnels to work.
#
# Usage: bash ~/bin/tunnel.sh
#   Or with port overrides: LAPTOP_SSHD_ON_PFX=7000 bash ~/bin/tunnel.sh

# === Port assignments (single source of truth) ===
# Override any of these with environment variables before running.
LAPTOP_SSHD_ON_PFX=${LAPTOP_SSHD_ON_PFX:-6079}
L1_SSHD_ON_PFX=${L1_SSHD_ON_PFX:-5500}
MAC_SSHD_ON_PFX=${MAC_SSHD_ON_PFX:-3079}
MAC_TO_PFX=${MAC_TO_PFX:-3101}

# pfx = our specific pfe node. All tunnels go here. Don't use generic "pfe".
PFX_HOST=pfe22.nas.nasa.gov

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
    echo "=== Mac mini: connecting to pfx ==="

    # Set up forward tunnel to pfx (so we can talk to pfx directly)
    # and reverse tunnel so pfx (and thus others) can reach us
    kill_port $MAC_TO_PFX

    echo "Connecting to $PFX_HOST and creating tunnels:"
    echo "  Local:  localhost:$MAC_TO_PFX -> pfx:22 (forward)"
    echo "  Remote: pfx:$MAC_SSHD_ON_PFX -> Mac:22 (reverse)"

    ssh $PFX_HOST -N -f $SSH_ALIVE \
      -L ${MAC_TO_PFX}:localhost:22 \
      -R ${MAC_SSHD_ON_PFX}:localhost:22

    if [ $? -ne 0 ]; then
      echo "ERROR: Failed to connect to pfx. Check your SSH keys and network."
      exit 1
    fi

    echo "Tunnels up. Updating ssh config on this machine."

    # Update local ssh config: pfx points to pfx via our forward tunnel
    ~/bin/set_port.py pfx $MAC_TO_PFX

    # Update pfx's config so it knows how to reach this Mac
    ssh pfx "python3 ~/bin/set_port.py mac_arm $MAC_SSHD_ON_PFX" 2>/dev/null

    # Now set up ProxyJump entries for laptop and l1
    # These go through pfx, which reaches laptop/l1 via the reverse
    # tunnels that laptop set up on pfx.
    # Use set_port.py to update the ports for chained connections.
    ~/bin/set_port.py laptop $LAPTOP_SSHD_ON_PFX
    ~/bin/set_port.py l1 $L1_SSHD_ON_PFX

    echo ""
    echo "Done. Available connections:"
    echo "  ssh pfx       -> pfx ($PFX_HOST)"
    echo "  ssh laptop    -> laptop (via pfx, needs laptop tunnels running)"
    echo "  ssh l1        -> lunokhod1 (via pfx+laptop, needs laptop tunnels + VPN)"
    echo ""
    echo "From pfx: ssh mac_arm -> this Mac (reverse tunnel)"
    echo ""
    echo "Note: 'laptop' and 'l1' only work after running tunnel.sh on the laptop."
    ;;

  *laptop*|*oalexan1-*)
    echo "=== Laptop: setting up reverse tunnels through pfx ==="

    # The laptop is the key bridge. It creates reverse tunnels on pfx so that:
    #   pfx:$LAPTOP_SSHD_ON_PFX -> laptop:22
    #   pfx:$L1_SSHD_ON_PFX    -> lunokhod1:22 (via laptop's VPN)
    kill_port $LAPTOP_SSHD_ON_PFX
    kill_port $L1_SSHD_ON_PFX

    echo "Connecting to $PFX_HOST and creating reverse tunnels:"
    echo "  pfx:$LAPTOP_SSHD_ON_PFX -> laptop:22"
    echo "  pfx:$L1_SSHD_ON_PFX -> lunokhod1:22"

    ssh $PFX_HOST -N -f $SSH_ALIVE \
      -R ${LAPTOP_SSHD_ON_PFX}:localhost:22 \
      -R ${L1_SSHD_ON_PFX}:lunokhod1.ndc.nasa.gov:22

    if [ $? -ne 0 ]; then
      echo "ERROR: Failed to connect to pfx. Check VPN and SSH keys."
      exit 1
    fi

    echo "Reverse tunnels active on pfx ($PFX_HOST)."

    # Update pfx's ssh config so it knows about laptop and l1
    update_remote_port "$PFX_HOST" laptop $LAPTOP_SSHD_ON_PFX
    update_remote_port "$PFX_HOST" l1 $L1_SSHD_ON_PFX

    # Push this script to l1 so all machines stay in sync
    scp ~/bin/tunnel.sh lunokhod1.ndc.nasa.gov:bin/tunnel.sh 2>/dev/null &&
      echo "Pushed tunnel.sh to lunokhod1" ||
      echo "Warning: could not push to lunokhod1 (VPN down?)"

    echo ""
    echo "Done. From pfx ($PFX_HOST):"
    echo "  ssh laptop  (port $LAPTOP_SSHD_ON_PFX)"
    echo "  ssh l1      (port $L1_SSHD_ON_PFX)"
    ;;

  *lunokhod1*)
    echo "=== Lunokhod1: no tunnels to create ==="
    echo "Lunokhod1 cannot initiate tunnels to pfx (no reverse forwarding)."
    echo "The laptop handles the relay. Make sure tunnel.sh ran on the laptop."
    echo ""
    echo "To verify connectivity, check that the laptop's VPN is up"
    echo "and that tunnel.sh was run on the laptop (connecting to $PFX_HOST)."
    ;;

  *)
    echo "Unknown host: $HOST"
    echo "Expected hostname containing: Mac-mini, laptop, or lunokhod1"
    echo "If this is a new machine, add a case for it in tunnel.sh."
    exit 1
    ;;
esac
