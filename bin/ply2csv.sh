#!/bin/bash

#if [ "$#" -lt 1 ]; then echo Usage: $0 argName; exit; fi

in=$1
out=${in/.ply/.csv}
echo Writing $out
cat $in | grep -A 10000000 end_header | grep -v end_header | grep -v "0 0 0" > $out
