#!/bin/bash

if [ "$#" -lt 4 ]; then echo Usage: $0 dir path output.txt cmd; exit; fi

# Go to directory $dir, and execute there $cmd output going to given file.

source ~/.bashenv
dir=$1
shift

if [ "$dir" != "" ]; then
    cd $dir
    if [ "$?" -ne 0 ]; then exit 1; fi
fi

path=$1
shift
if [ ! -d "$path" ]; then
    echo "Path does not exist: $path"
    exit 1
fi
export PATH=$path:$PATH
which stereo

output=$1
shift

cmd="$*"

pwd
echo "$cmd > $output 2>&1"

$cmd > $output 2>&1

