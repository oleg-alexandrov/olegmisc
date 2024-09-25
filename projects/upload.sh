#!/bin/bash

cd $HOME/projects/stereopipeline/daily_build
pwd

build=$(ls -trd *x86_64-Linux-GLIBC-2.5.tar.bz2 | tail -n 1)
nbuild=StereoPipeline-2.2.2_post-x86_64.tar.bz2
cp -fv $build $nbuild

echo Uploading $nbuild
ftp -i -n psc.apl.washington.edu << END

user anonymous anonymous
binary on 
cd incoming/forDavid
ls 
put $nbuild
END

echo "Done!"