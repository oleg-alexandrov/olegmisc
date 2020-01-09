#!/bin/bash


if [ "$#" -lt 1 ]; then echo Usage: $0 dispFile; exit; fi

dispFile=$1
files=""
filesn=""
for b in 1 2 3; do
    bFile=${dispFile/.tif/_b$b.tif}
    bFilen=${dispFile/.tif/_bn$b.tif}
    gdal_translate -b $b $dispFile $bFile
    float2int2.pl $bFile $bFilen
    files="$files $bFile"
    filesn="$filesn $bFilen"
done

echo $files
echo $filesn
eog $filesn
