#!/bin/bash

if [ "$#" -lt 2 ]; then echo Usage: $0 portName remoteMachine; exit; fi

# Set up reverse port forwarding and record the port on the remote
# machine.
port=$1
machine=$2

ans=1
while [ 1 ]; do
    ssh $machine -N -X -f -R ${port}:localhost:22
    ans=$?
    if [ "$ans" -eq "0" ]; then
        break
    fi
    ((port=port+1))
    echo Trying port $port
done
ssh $machine "echo ${port} > .ssh/port.txt"
ans=$?

