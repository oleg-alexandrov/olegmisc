#!/bin/bash

if [ "$#" -lt 3 ]; then echo Usage: $0 port port_in machine; exit; fi

# Set up reverse port forwarding and record the port on the remote
# machine. Then, the script set_port.sh will take that recorded port
# and modify .ssh/config on the remote machine.

port=$1; shift
port_in=$1; shift
machine=$1; shift

ans=1
while [ 1 ]; do
    ssh $machine -N -X -f -R ${port}:localhost:22 -L ${port_in}:localhost:22
    ans=$?
    if [ "$ans" -eq "0" ]; then
        break
    fi
    ((port=port+1))
    echo Trying port $port
done
ssh $machine "echo ${port} > .ssh/port.txt"
ans=$?

