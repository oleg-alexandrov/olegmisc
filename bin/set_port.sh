#!/bin/bash

# Edit .ssh/config on the remote machine (from the remote machine) and
# set the given port. Count on a tag in the text to find where to
# edit.

old_port=$(grep -B 1 "portTag" ~/.ssh/config | tail -n 2 | head -n 1)
if [ "$old_port"  != "" ]; then 
    p=$(cat ~/.ssh/port.txt)
    if [ "$p" != "" ]; then
        new_port="  Port $p";
        if [ "$old_port" != "$new_port" ]; then
            perl -pi -e "s#$old_port#'  Port ' . $p#eg" ~/.ssh/config
            echo Changed port on machine $(uname -n) for machine \'laptop\' to $p
        else
            echo Port already set to $p
        fi
    fi
else
    echo Could not modify .ssh/config on machine $(uname -n)
fi

