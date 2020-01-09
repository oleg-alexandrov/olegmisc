#!/bin/bash


if [ "$#" -lt 1 ]; then echo Usage: $0 argName; exit; fi

# gdal_translate -outsize 10% 10% $1 tmp.tif
# float2int2.pl tmp.tif tmp_sc.tif
# gdal_translate -outsize 10% 10% $1 tmp.tif
#eog tmp_sc.tif

for b in $*; do
    echo $b
    e=${b/.tif/_int.tif}
    float2int2.pl $b $e
    echo $b $e
done

out=$(echo "$*" | perl -pi -e "s#.tif#_int.tif#g")
echo $out
eog $out


