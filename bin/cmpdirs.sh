#!/bin/sh

# Compare two directories. Exit with status 0 if the directories
# are identical, and with status 1 otherwise.

dir1=$1
dir2=$2

if [ ! -d "$dir1" ] || [ ! -d "$dir2" ]; then
    echo One of $dir1 or $dir2 is missing
    exit 1
fi

cd $dir1
files1=$(find .)
cd ../$dir2
files2=$(find .)
cd ..
files="$files1 $files2"

ret=0
for f in $files; do 
        #echo $dir1/$f $dir2/$f
        if [ -d $dir1/$f ] && [ -d $dir2/$f ]; then continue; fi;
        out=$(cmp $dir1/$f $dir2/$f)
        if [ "$out" != "" ]; then
            echo $out
            ret=1
        fi
done 

exit $ret