#!/bin/bash

if [ "$#" -lt 3 ]; then echo Usage: $0 inExt outExt inFiles; exit; fi

inExt=$1; shift
outExt=$1; shift

for fileIn in $*; do
    fileOut=${fileIn/$inExt/$outExt}
    cp -iv $fileIn $fileOut
done

