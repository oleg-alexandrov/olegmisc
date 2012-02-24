#!/bin/bash

# tkdiff of the first several lines

n=$1
file1=$2
file2=$3

head -n $n $file1 > /tmp/tmp1.txt
head -n $n $file2 > /tmp/tmp2.txt

tkdiff /tmp/tmp1.txt /tmp/tmp2.txt

rm -fv /tmp/tmp1.txt /tmp/tmp2.txt

