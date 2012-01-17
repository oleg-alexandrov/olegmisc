#!/bin/bash

plainDiff=0
if [ "$#" -eq 3 ]; then 
    shift
    plainDiff=1
fi

echo $1 > /tmp/img1.txt
echo $2 > /tmp/img2.txt
gdalinfo -stats $1 >> /tmp/img1.txt
gdalinfo -stats $2 >> /tmp/img2.txt

if [ "$plainDiff" -eq 1 ]; then
    diff /tmp/img1.txt /tmp/img2.txt
else
    tkdiff /tmp/img1.txt /tmp/img2.txt
fi

rm -f /tmp/img1.txt /tmp/img2.txt
