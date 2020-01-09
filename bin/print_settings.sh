#!/bin/bash

machine=$(uname -n | perl -pi -e "s#\..*?\$##g")
file=../release_$machine.conf
if [ ! -f $file ]; then
    echo Warning: File $file does not exist
    file=release_$machine.conf
    if [ ! -f $file ]; then
        echo Error: File $file does not exist
        exit 1
    fi
fi

currDir=$(pwd)

dir=$(grep -i export $file | grep ASP= | perl -pi -e "s#^.*?ASP=##g" | perl -pi -e "s#\\\$HOME#$HOME#g")

echo 
if [ -d "$dir" ]; then
    echo $dir exists
else
    echo $dir does not exist
    dir2=$(dirname $dir)
    cd $dir2
    echo now in $dir2
    tar xjfv $dir.tar.bz2
fi
echo " "
echo " "

cd $currDir
grep -i export $file
