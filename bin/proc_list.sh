#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 log.txt; exit; fi

log=$1

grep "processed in" $log | perl -pi -e "s#^.*?processed##g" | print_col.pl 2 | sortbycol.pl 0
