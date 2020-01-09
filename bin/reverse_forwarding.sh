#!/bin/bash

# Establish a reverse-port-forwarding connection from a desired remote
# machine using a given port to the local machine on port 22. If the
# connection already exists, do not create a new one.

# Example usage (run this on the local machine):
# reverse_forwarding.sh remote_user@remote_machine 4005
#
# Then, from the remote machine one can do
# ssh localhost -p 4005 -l local_user -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no
# to ssh into the local machine.

if [ "$#" -lt 2 ]; then echo Usage: $0 machine port; exit; fi

machine=$1
port=$2

dest=$port:localhost:22
cmd="ssh $machine -R $dest -n -N -f "

exists=$(ps ux | grep ssh | grep $dest | grep -i -v grep)

if [ "$exists" != "" ]; then
    echo Connection exists:
    echo $exists
else
    echo Establishing connection
    echo $cmd
    $cmd 2>&1 
    ans=$?
    if [ "$ans" -eq "0" ]; then
        echo Connection established
    else
        echo Connection failed
    fi
fi
