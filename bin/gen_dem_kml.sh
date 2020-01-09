#!/bin/bash


if [ "$#" -lt 1 ]; then echo Usage: $0 name files; exit; fi

name=$1
shift
files=$*
B=$(whoami)@byss
base=https://byss.arc.nasa.gov/$(whoami)
root=/byss/docroot/$(whoami)
address=$base/$name.kml
list=list.html
tmpList=tmpFile.html

links=$(echo $files | perl -pi -e "s#\s#\n#g" | perl -pi -e "s#.tif##g" | perl -pi -e "s#[/\.]#_#g" | perl -pi -e "s#^(.*?)\s*\$#$base/\$1.kml #g")

kml=$name.kml
~/bin/create_combined_kml.pl $kml $(echo $links)

echo "Number of links: " $(grep -i networklink $kml | wc)

rsync -avz $B:$root/$list .
tac $list > $tmpList
echo "<p> <a href=\"$address\">$address</a><br>" >> $tmpList
tac $tmpList > $list
rsync -avz $kml $list $B:$root
