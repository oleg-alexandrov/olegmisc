#!/bin/bash

if [ "$#" -lt 2 ]; then 
    echo Usage: $0 currDir goldDir
    exit
fi

currDir=$1
goldDir=$2

chmod a+w -R $goldDir 
rm -rfv $goldDir
cp -rfv $currDir $goldDir
chmod a-w -R $goldDir 


