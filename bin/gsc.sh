#!/bin/bash

if [ "$#" -lt 2 ]; then echo Usage: $0 file.tif pct; exit; fi

in_file=$1
pct=$2
out_file=$3

if [ "$out_file" = "" ]; then
    out_file=$in_file
    for ext in tif ntf TIF NTF cub CUB vrt VRT; do
        out_file=${out_file/.$ext/_"$pct"pct.$ext}
    done
    for ext in tif ntf TIF NTF cub CUB vrt VRT; do
        out_file=${out_file/.$ext/.tif}
    done
fi

time_run.sh ~/bin/gdal_translate.pl -outsize $pct% $pct% $in_file $out_file
