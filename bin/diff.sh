#!/bin/bash

dem1=$1
dem2=$2
tag=$3
max=$4

extended=0
signed=0
if [ "$tag" = "e" ]; then extended=1; fi
if [ "$tag" = "s" ]; then signed=1; fi

if [ "$max" = "" ]; then max=0; fi
if [ "$tag" = "" ]; then tag=v1; fi

# Make a prefix, and don't make it too long
pref=$(echo tmp-$tag-$dem1-$dem2 | perl -p -e "s#\/#-#g" 2>/dev/null | perl -p -e "s#\$_#substr(\$_, 0, 200)#eg")

#echo Writing to $pref
rm -fv $pref* > /dev/null 2>&1

if [ ! -f "$dem1" ]; then echo Missing file $dem1; exit; fi
if [ ! -f "$dem2" ]; then echo Missing file $dem2; exit; fi

if [ "$signed" -eq 0 ]; then
    geodiff $dem1 $dem2 -o $pref --absolute > /dev/null 2>&1
else
    geodiff $dem1 $dem2 -o $pref > /dev/null 2>&1
fi
ans=$?
if [ "$ans" != "0" ]; then echo "Diff of $dem1 and $dem2 failed"; fi

gdalinfo -stats $pref-diff.tif | grep -i minimum | grep -i maximum
if [ "$extended" -eq 1 ]; then 
    gdalinfo -stats $pref-diff.tif | grep -i _mean
fi

if [ "$max" != "0" ]; then
    colormap --min 0 --max $max $pref-diff.tif --colormap-style binary-red-blue > /dev/null 2>&1
    image2qtree.pl $pref-max-$max-$tag tmp-diff_CMAP.tif > /dev/null 2>&1
fi

rm -fv $pref* > /dev/null 2>&1
