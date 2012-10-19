#!/bin/bash

# tkdiff of the first several lines
if [ "$#" -lt 3 ]; then 
    echo Usage: $0 numLines file1 file2
    exit
fi

n=$1
file1=$2
file2=$3

head -n $n $file1 > /tmp/tmp1.txt
head -n $n $file2 > /tmp/tmp2.txt

diff /tmp/tmp1.txt /tmp/tmp2.txt

rm -fv /tmp/tmp1.txt /tmp/tmp2.txt

