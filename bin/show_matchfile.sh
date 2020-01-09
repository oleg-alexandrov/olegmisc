#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 match-file.match; exit; fi

f=$1
log=$(ls -alh -rtd $(dirname $f)/*log* |grep -E "bundle_adjust|stereo_pprc" | tail -n 1 | ~/bin/print_col.pl 0)
l=$(echo $f |  perl -p -e "s#^.*?\/##g" | perl -p -e "s#^.*?-(.*?)__.*?\$#\$1#g" | perl -p -e "s#disp-##g")
r=$(echo $f |  perl -p -e "s#^.*?\/##g" | perl -p -e "s#^.*?-.*?__(.*?)(|-clean).match\$#\$1#g")

#echo 1 $l $r
#echo $log

l=$(grep -E "bundle_adjust|stereo_pprc" $log | perl -p -e "s#\s#\n#g" | grep $l | grep -i -E ".cub|.ntf|.tif")
r=$(grep -E "bundle_adjust|stereo_pprc" $log | perl -p -e "s#\s#\n#g" | grep $r | grep -i -E ".cub|.ntf.|.tif")

#echo 2 $l $r

echo stereo_gui $l $r $f
stereo_gui $l $r $f 2>&1 |grep -i -v fontconfig






