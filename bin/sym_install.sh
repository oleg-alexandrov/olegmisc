#!/bin/bash

# Fake an install by creating symlinks; this takes much less time than
# running 'make install' all the time.

base=$HOME/projects/visionworkbench$BASE
rm -rfv $base/build

mkdir -p $base/build/include
ln -s $base/src/vw $base/build/include

mkdir -p $base/build/lib
for f in $(find $base -regextype posix-egrep -regex '.*\.(la|a|so).*$'); do
    g=$(basename $f)
    echo ln -s $f $base/build/lib/$g
    ln -s $f $base/build/lib/$g
done

base=$HOME/projects/StereoPipeline$BASE
rm -rfv $base/build
mkdir -p $base/build/include
ln -s $base/src/asp $base/build/include

mkdir -p $base/build/lib
for f in $(find $base -regextype posix-egrep -regex '.*\.(la|a|so).*$'); do
    g=$(basename $f)
    echo ln -s $f $base/build/lib/$g
    ln -s $f $base/build/lib/$g
done


    

