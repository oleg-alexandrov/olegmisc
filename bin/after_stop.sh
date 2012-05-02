#!/bin/bash

# Wait until process cmdRunning stops. Then start the command cmdRunAfter.

if [ "$#" -lt 2 ]; then 
    echo Usage: $0 cmdRunning cmdRunAfter
    exit
fi

cmdRunning=$1
shift
echo cmdRunning=$cmdRunning
cmdRunAfter="$*"
echo cmdRunAfter=$cmdRunAfter

while [ 1 ]; do

    val=$(ps ux | grep -E "\b$cmdRunning\b" | grep -v grep | grep -v $0)
    if [ "$val" = "" ]; then
        echo Command $cmdRunning stopped
        break
    else
        echo Still running $val
        sleep 5 # Wait
    fi
    
done

$cmdRunAfter