#!/bin/bash
# tunnel.sh - SSH tunnel setup for Mac <-> pfe <-> laptop <-> lunokhod1
#
# Detects which machine it runs on and does the right thing.
# Run on laptop first (must be on VPN), then on Mac.
#
# The chain: Mac --> pfe --> laptop --> lunokhod1
#   - pfe is the relay (all machines can connect to it)
#   - laptop is the bridge to lunokhod1 (only laptop can reach it, via VPN)
#   - lunokhod1 cannot initiate tunnels (no reverse forwarding from behind firewall)
#
# Usage: bash ~/bin/tunnel.sh
#   Or with port overrides: LAPTOP_SSHD_ON_PFE=7000 bash ~/bin/tunnel.sh

# === Port assignments (single source of truth) ===
# Override any of these with environment variables before running.
LAPTOP_SSHD_ON_PFE=${LAPTOP_SSHD_ON_PFE:-6079}
L1_SSHD_ON_PFE=${L1_SSHD_ON_PFE:-5500}
MAC_SSHD_ON_PFE=${MAC_SSHD_ON_PFE:-3079}
MAC_TO_PFX=${MAC_TO_PFX:-3101}

PFE_HOST=pfe21.nas.nasa.gov

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
    echo "=== Mac mini: connecting to pfe ==="

    # Set up forward tunnel to pfe (so we can talk to pfe directly)
    # and reverse tunnel so pfe (and thus others) can reach us
    kill_port $MAC_TO_PFX

    ssh $PFE_HOST -N -f $SSH_ALIVE \
      -L ${MAC_TO_PFX}:localhost:22 \
      -R ${MAC_SSHD_ON_PFE}:localhost:22

    if [ $? -ne 0 ]; then
      echo "ERROR: Failed to connect to pfe. Check your SSH keys and network."
      exit 1
    fi

    echo "Tunnels up. Updating ssh config on this machine."

    # Update local ssh config: pfx points to pfe via our forward tunnel
    ~/bin/set_port.py pfx $MAC_TO_PFX

    # Update pfe's config so it knows how to reach this Mac
    ssh pfx "python3 ~/bin/set_port.py mac_arm $MAC_SSHD_ON_PFE" 2>/dev/null

    # Now set up ProxyJump entries for laptop and l1
    # These go through pfx, which reaches laptop/l1 via the reverse
    # tunnels that laptop set up on pfe.
    # Use set_port.py to update the ports for chained connections.
    ~/bin/set_port.py laptop $LAPTOP_SSHD_ON_PFE
    ~/bin/set_port.py l1 $L1_SSHD_ON_PFE

    echo ""
    echo "Done. Available connections:"
    echo "  ssh pfx       -> pfe (direct)"
    echo "  ssh laptop    -> laptop (via pfe, needs laptop tunnels running)"
    echo "  ssh l1        -> lunokhod1 (via pfe+laptop, needs laptop tunnels + VPN)"
    echo ""
    echo "From pfe: ssh mac_arm -> this Mac (reverse tunnel)"
    echo ""
    echo "Note: 'laptop' and 'l1' only work after running tunnel.sh on the laptop."
    ;;

  *laptop*|*oalexan1-*)
    echo "=== Laptop: setting up reverse tunnels through pfe ==="

    # The laptop is the key bridge. It creates reverse tunnels on pfe so that:
    #   pfe:$LAPTOP_SSHD_ON_PFE -> laptop:22
    #   pfe:$L1_SSHD_ON_PFE    -> lunokhod1:22 (via laptop's VPN)
    kill_port $LAPTOP_SSHD_ON_PFE
    kill_port $L1_SSHD_ON_PFE

    ssh $PFE_HOST -N -f $SSH_ALIVE \
      -R ${LAPTOP_SSHD_ON_PFE}:localhost:22 \
      -R ${L1_SSHD_ON_PFE}:lunokhod1.ndc.nasa.gov:22

    if [ $? -ne 0 ]; then
      echo "ERROR: Failed to connect to pfe. Check VPN and SSH keys."
      exit 1
    fi

    echo "Reverse tunnels active on pfe."

    # Update pfe's ssh config so it knows about laptop and l1
    update_remote_port "$PFE_HOST" laptop $LAPTOP_SSHD_ON_PFE
    update_remote_port "$PFE_HOST" l1 $L1_SSHD_ON_PFE

    # Push this script to l1 and pfe so all machines stay in sync
    scp ~/bin/tunnel.sh lunokhod1.ndc.nasa.gov:bin/tunnel.sh 2>/dev/null &&
      echo "Pushed tunnel.sh to lunokhod1" ||
      echo "Warning: could not push to lunokhod1 (VPN down?)"

    echo ""
    echo "Done. From any machine with tunnels to pfe:"
    echo "  ssh laptop  (port $LAPTOP_SSHD_ON_PFE on pfe)"
    echo "  ssh l1      (port $L1_SSHD_ON_PFE on pfe)"
    ;;

  *lunokhod1*)
    echo "=== Lunokhod1: no tunnels to create ==="
    echo "Lunokhod1 cannot initiate tunnels to pfe (no reverse forwarding)."
    echo "The laptop handles the relay. Make sure tunnel.sh ran on the laptop."
    echo ""
    echo "To verify connectivity, check that the laptop's VPN is up"
    echo "and that tunnel.sh was run on the laptop."
    ;;

  *)
    echo "Unknown host: $HOST"
    echo "Expected hostname containing: Mac-mini, laptop, or lunokhod1"
    echo "If this is a new machine, add a case for it in tunnel.sh."
    exit 1
    ;;
esac
