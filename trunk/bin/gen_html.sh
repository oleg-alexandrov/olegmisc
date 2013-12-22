#!/bin/bash

if [ "$#" -lt 3 ]; then 
    echo Usage: $0 index.html files.png
    exit
fi

index=$1
shift
files=$*

dir=$(dirname $index)
mkdir -p $dir
rm -fv $index 
for f in $files; do
    echo $f
    echo "<img src=$f><br>" >> $index; 
done

index=$(pwd)/$index
index=${index/\/byss\/docroot\//}
link=https://byss.arc.nasa.gov/$(whoami)/$index
echo "$link"
# tac list.html > tmp.html
# echo "<p><a href=$link>$link</a>" >> tmp.html
# tac tmp.html > list.html
# echo "Appended to list.html"

