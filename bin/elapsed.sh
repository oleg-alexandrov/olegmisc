#!/bin/bash

if [ "$#" -lt 2 ]; then echo Usage: $0 type file; exit; fi

type=$1
file=$2
#echo $file
grep 2013 $file | grep $type | grep -v LOW| head -n 1
grep 2013 $file | grep $type | grep -v LOW| tail -n 1
