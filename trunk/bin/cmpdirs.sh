#!/bin/sh

dir1=$1
dir2=$2

cd $dir1
files1=$(find .)
cd ../$dir2
files2=$(find .)
cd ..
files="$files1 $files2"

for f in $files; do 
        #echo $dir1/$f $dir2/$f
        if [ -d $dir1/$f ] && [ -d $dir2/$f ]; then continue; fi;
        cmp $dir1/$f $dir2/$f

done 
