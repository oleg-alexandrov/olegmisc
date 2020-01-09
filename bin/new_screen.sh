#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 prefix; exit; fi

# Attach to a detached screen session with given prefix,
# or, if no detached screen exists, create a new one

prefix=$1
echo prefix is $prefix

i=1;
while [ 1 ]; do
    g=$(screen -ls | perl -pi -e "s#\t# #g" | grep "${prefix}$i ");
    echo $g;
    if [ "$g" = "" ]; then
        screen -S ${prefix}$i;
        break;
    fi;
    ((i=i+1));
done