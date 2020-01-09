#!/bin/sh

source ~/.bashenv

echo "$*"
out_file=$1

shift

cmd="$*"
echo "$cmd > $out_file 2>&1"
$cmd > $out_file 2>&1

