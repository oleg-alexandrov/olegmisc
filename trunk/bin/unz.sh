#!/bin/sh

case "$1" in
    *.tar.z|*.tar.gz|*.tgz)  tar xzfv  $1;;
    *.tar)                   tar xfv   $1;;
    *.Z|*.z|*.gz)            gunzip    $1;;
    *.tar.bz2)               bzip2 -dc $1 | tar xfv - ;;
    *.bz2)                   bzip2 -d  $1;;
    *.zip)                   unzip     $1;;
    *)                       echo "file ending not supported";;
esac
 
