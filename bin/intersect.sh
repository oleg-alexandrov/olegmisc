#!/bin/bash

if [ "$#" -lt 2 ]; then echo Usage: $0 file1 file2; exit; fi

img1=$1
img2=$2
win=$($HOME/bin/intersection.pl $img1 $img2)
img1_crop=$(echo $img1 | perl -pi -e "s#^.*\/##g"); img1_crop=${img1_crop/.tif/_crop.tif}
img2_crop=$(echo $img2 | perl -pi -e "s#^.*\/##g"); img2_crop=${img2_crop/.tif/_crop.tif}

time_run.sh gdal_translate -projwin $win $img1 $img1_crop
time_run.sh gdal_translate -projwin $win $img2 $img2_crop

