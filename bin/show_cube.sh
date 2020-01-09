#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 file.cub mpp; exit 1; fi

cub=$1
mpp=$2
if [ "$mpp" = "" ]; then mpp=5; fi

. ~/bin/isis_setup.sh
map=tmp-$(basename $cub)
map=${map/.cub/.map.cub}
echo $HOME/projects/isis/bin/cam2map from=$cub to=$map pixres=mpp resolution=$mpp
$HOME/projects/isis/bin/cam2map from=$cub to=$map pixres=mpp resolution=$mpp
~/bin/image2qtree.pl $cub $map
rm -fv $map

