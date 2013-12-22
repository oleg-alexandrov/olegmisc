#!/bin/bash

port=$1 # e.g., 3250:localhost:22
T=lunokhod1

# We want to keep reverse forwarding from given machine to $T alive.
# To verify that it is alive, test by ssh-ing to $T and doing ssh
# back. If that fails, kill the current reverse forwarding process
# and restart it.
progName=$(echo $0 | perl -pi -e "s#^.*\/##g")
echo progName is $progName
machine=$(uname -n)
output=$(ssh $T "ssh $machine ls / 2>/dev/null" 2>/dev/null) 
if [ "$output" = "" ]; then
    echo "Connection to $T is bad or non-existent, will restart it"
    pid=$(ps ux |grep localhost | grep $port | grep -v ps | \
        grep -v grep | grep -v $progName | awk '{print $2}')
    ps ux |grep localhost | grep $port | grep -v ps |  grep -v grep |grep -v $progName
    echo "Pid of bad conneection is $pid"
    if [ "$pid" != "" ]; then
        kill -9 $pid
    fi
    ssh -N -f $T -R $port > /dev/null 2>&1
else
    echo "Connection to $T is good, output is $output"
fi
