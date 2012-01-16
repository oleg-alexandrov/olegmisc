#!/bin/bash

echo $1 > /tmp/img1.txt
echo $2 > /tmp/img2.txt
gdalinfo -stats $1 >> /tmp/img1.txt
gdalinfo -stats $2 >> /tmp/img2.txt

tkdiff /tmp/img1.txt /tmp/img2.txt

rm -f /tmp/img1.txt /tmp/img2.txt
