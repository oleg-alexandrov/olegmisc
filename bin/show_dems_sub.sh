#!/bin/bash


if [ "$#" -lt 1 ]; then echo Usage: $0 sum dems; exit; fi

sub=$1
shift
for f in $*; do
    args=""
    if [ -e $f ]; then 
        g=${f/.tif/sub$sub.tif}
        time_run.sh gdal_translate -outsize $sub% $sub% $f $g
    else
        g=$f
    fi
    args="$args $g"
done

cmd=$0
cmd=${cmd/_sub.sh/.pl}
cmd="$cmd $args"
$cmd
