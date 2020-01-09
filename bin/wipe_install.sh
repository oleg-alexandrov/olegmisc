#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 dirToClean; exit; fi

dir=$1
if [ ! -d "$dir" ]; then echo directory does not exist: $dir; exit; fi

for f in  ~/projects/visionworkbench/src/vw/tools/* ~/projects/StereoPipeline/src/asp/Tools/*; do
    g=$(basename $f)
    g=$(echo $g | perl -pi -e "s#\..*?\$##g")
    g=$dir/bin/$g
    if [ ! -f "$g" ]; then continue; fi
    #echo rm -fv $g
    rm -fv $g
done

rm -rfv $dir/include/vw
rm -rfv $dir/include/asp

rm -fv $dir/lib/libasp*
rm -fv $dir/lib/libvw*



