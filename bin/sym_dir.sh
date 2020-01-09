#!/bin/bash

if [ "$#" -lt 2 ]; then echo Usage: $0 from to; exit; fi

from=$1
to=$2

if [ -e $to ]; then echo Dir exists $to; exit 1; fi

if [ ! -d $from ]; then echo Dir does not exist $from; fi

mkdir -p $to
cd $to
for f in $(ls ../$from/* |grep -i -v log |grep -i -v Intersect | grep -i -v DEM | grep -i -v PC); do
    echo ln -s $f .
    ln -s $f .
done
rm -fv *PC*;
cd ..

echo Symlinked most of $from to $to
