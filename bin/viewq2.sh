#!/bin/bash


if [ "$#" -lt 1 ]; then echo Usage: $0 argName; exit; fi

b=$1
e=${b/.tif/_scaled.tif}
echo $b $e
time_run.sh float2int2.pl $b $e
image2qtree.pl $e
