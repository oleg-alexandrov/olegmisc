#!/bin/bash

# convert file.cpp:292 into: vim file.cpp +292

line=$(echo $* | perl -pi -e "s#^(.*?):(\d+).*?\$#\$1 \+\$2#g")
cmd="vim $line"
echo $cmd
$cmd

