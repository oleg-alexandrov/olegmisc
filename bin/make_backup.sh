#!/bin/bash

#for d in $HOME/PhotometryTK/src/photk $HOME/PhotometryTK/src/tools \
#    $HOME/projects/albedo $HOME/projects/BinaryBuilder; do
#for d in $HOME/projects/HiRISE/HiPrecision; do    
for d in ""; do
    if [ ! -e "$d" ]; then continue; fi
    cd $d;
    num=$(ls -d -t backup/*[0-9] | head -n 1 | perl -pi -e "s#^.*\/##g")
    if [ "$num" = "" ]; then num=0; fi;
    ((num++))

    bkdir=backup/$num;
    echo Will backup $(pwd) to $bkdir
    mkdir -p $bkdir
    for f in *cxx *cpp *.m *.*c *.h photo*.txt *.pl *.sh *.py *.java *.class *akefile; do
        if [ -e $f ]; then cp $f $bkdir; fi;
    done
done
