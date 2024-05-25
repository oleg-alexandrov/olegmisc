#!/bin/bash

# Set up reverse port forwarding and record that part on the remote machine.

if [ "$#" -lt 1 ]; then echo Usage: $0 machine; exit; fi
machine=$1; shift

# Read the last port used from a file
portFile=/tmp/port_${machine}.txt
port=$(cat $portFile)

# if it does not exist, or it is >= 9999, then set it to 5000
if [ "$port" == "" ] || [ "$port" -ge 9999 ]; then
    port=5000 
fi

# Increment it
((port++));

ans=1
while [ 1 ]; do
    echo Trying port $port
    ssh $machine -N -X -f -R ${port}:localhost:22
    # Updating the remote machine's port requires a second ssh command
    ssh $machine -X "bin/set_port.sh $port"
    ans=$?
    if [ "$ans" -eq "0" ]; then
      echo Success
      # Save it for next time
      echo $port > $portFile
      break
    fi
    
    ((port=port+1))
    if [ "$port" -ge 9999 ]; then port=5000; fi
done

echo Updating the local machine time
sudo ntpdate pool.ntp.org
