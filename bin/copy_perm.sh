#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 argName; exit; fi

file=$1
dir=$(dirname $file)

mkdir -p tmp
mv $dir tmp
cp -rfv tmp/$dir .
