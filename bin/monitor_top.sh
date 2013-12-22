#!/bin/bash

if [ "$#" -lt 2 ]; then echo Usage: $0 prog logfile; exit; fi

prog=$1
logfile=$2

s=5
((num=24*3600/s))

rm -f $logfile
for ((i = 0; i < num; i++)); do
    v=$(top -u $(whoami) -b -n 1 |grep $prog)
    echo $v
    echo $v >> $logfile
    sleep $s
done
