#!/bin/bash

# Edit .ssh/config on the remote machine (from the remote machine) and
# set the given port. Count on a tag in the text to find where to
# edit.

# Must have one input argument
if [ "$#" -ne 1 ]; then
    echo "Usage: set_port.sh <port>"
    exit 1
fi
port=$1; shift

old_port=$(grep -B 1 "portTag" ~/.ssh/config | tail -n 2 | head -n 1)
if [ "$old_port"  == "" ]; then 
  echo Failed to find the port tag in .ssh/config on machine $(uname -n)
  exit 1
fi
    
new_port="  Port $port";
if [ "$old_port" != "$new_port" ]; then
    perl -pi -e "s#$old_port#$new_port#g" ~/.ssh/config
    #echo Changed port on machine $(uname -n) to $port
else
    #echo Port already set to $port
fi
