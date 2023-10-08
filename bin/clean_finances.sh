#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 file.csv; exit; fi

file=$1
/bin/cp -fv $file /tmp # On windows it does not let one edit

file=/tmp/$(basename $file)

echo Cleaning $file

perl -pi -e 's/[^[:ascii:]]//g' $file
perl -pi -e 's/\-//g' $file
perl -pi -e 's/\$//g' $file

sudo /bin/cp -fv $file ~/Desktop

