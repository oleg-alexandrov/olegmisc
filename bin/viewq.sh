#!/bin/bash


if [ "$#" -lt 1 ]; then echo Usage: $0 argName; exit; fi

# gdal_translate -outsize 10% 10% $1 tmp.tif
# float2int2.pl tmp.tif tmp_sc.tif
# gdal_translate -outsize 10% 10% $1 tmp.tif
#eog tmp_sc.tif

if [ "$#" -ge 3 ]; then
    pct=$3
else
    pct=10
fi

if [ "$#" -ge 2 ]; then
    name=$1
    img=$2
else
    name=""
    img=$1
fi

e=${img/.tif/_"$pct"pct.tif}
g=${img/.tif/_int_"$pct"pct.tif}
echo $img $e $g
time_run.sh gdal_translate -outsize $pct% $pct% $img $e
time_run.sh float2int2.pl $e $g

if [ "$name" != "" ]; then
    time_run.sh image2qtree.pl $name $g
else
    time_run.sh image2qtree.pl $g
fi
