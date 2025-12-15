#!/bin/bash

# Create a reverse SSH tunnel to the given machine, trying ports from 5001 to
# 5999. Push the selected port to the remote machine using bin/set_port.sh so
# that it can be used for the reverse SSH connection.

if [ "$#" -lt 1 ]; then echo Usage: $0 machine; exit; fi
machine=$1; shift

# Socket file for multiplexing
sock="/tmp/ssh_socket_${machine}_$$"

portFile=/tmp/port_${machine}.txt
[ ! -f $portFile ] && touch $portFile

port=$(cat $portFile)
if [ -z "$port" ] || [ "$port" -ge 9999 ]; then port=5000; fi
((port++))

# Clean up socket on exit
trap "rm -f $sock" EXIT

while true; do
    echo "Trying port $port"

    # 1. Start maser connection with remote port forwarding
    # -M: Master mode
    # -S: Socket path
    # -o ExitOnForwardFailure=yes: Fails immediately if port is busy (saves time).
    # Then the loop will try the next port.
    ssh -o ExitOnForwardFailure=yes -M -S $sock -f -N -R ${port}:localhost:22 $machine
    
    if [ $? -eq 0 ]; then
        # 2. Run the check using the existing socket (Instant)
        ssh -S $sock $machine "bin/set_port.sh $port"
        
        if [ $? -eq 0 ]; then
            echo "Success"
            echo $port > $portFile
            break
        else
            # If set_port.sh failed, close the tunnel we just opened
            ssh -S $sock -O exit $machine
        fi
    fi

    ((port++))
    if [ "$port" -ge 5999 ]; then port=5000; fi
done

# Update the local time as otherwise compilation may fail due to time skew
echo "Updating local time"
sudo ntpdate time.google.com

