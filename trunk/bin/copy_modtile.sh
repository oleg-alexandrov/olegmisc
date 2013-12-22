#!/bin/bash


if [ "$#" -lt 2 ]; then echo Usage: $0 from to; exit; fi

from=$1
to=$2

a=$(ls -l --time-style '+%Y-%m-%d %T' $from | awk '{print $7}')
b=$(ls -l --time-style '+%Y-%m-%d %T' $from | awk '{print $8}')
touchtm="$a $b"
echo $touchtm
touch -d "$touchtm" $to

