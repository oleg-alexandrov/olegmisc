#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 img1.tif img2.tif; exit; fi

img1=$1
img2=$2

time_run.sh ipfind --num-threads 8 --normalize $img1 $img2
time_run.sh ipmatch $img1 $img2  -r homography --debug
