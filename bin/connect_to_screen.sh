#!/bin/bash

#if [ "$#" -lt 1 ]; then echo Usage: $0 argName; exit; fi

# Connect to given screen if it exists or create a new one if not
# function connect_to_screen {
prefix=$1
count=$2
session=${prefix}${count}
ans=$(screen -ls | grep -E "$session\b" | awk '{print $1}')
echo ans is $ans
#exit

if [ "$ans" == "" ]; then
    screen -S $session
else
    screen -d -x $ans
fi
# }
