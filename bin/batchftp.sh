#!/bin/bash

# Fetch files via ftp from the remote backup server and compare 
# with the local reference copy.

LOCAL_REF="$HOME/OlegBackup";
LOCAL_DEST="$HOME/DriveHQ";
REMOTE_SOURCE="/backup";
PREFIX="Old";
USER_PASS=`cat user_pass.txt`;

mkdir $LOCAL_DEST/$PREFIX;

cd $LOCAL_REF;
for dir in $PREFIX/*/; do
 
    cd $LOCAL_REF;
    cd $dir; pwd;
    files=`ls *`;

    cd $LOCAL_DEST;
    mkdir $dir; cd $dir; pwd;
    for file in $files; do 

# This piece of code must not be indented
ftp -p -n -v ftp.drivehq.com << ENDTAG
user $USER_PASS
pwd
cd $REMOTE_SOURCE/$dir
pwd
binary on
ls $file
get $file
bye
ENDTAG

    #exit 0;
    echo "sleep ..."; sleep 700; # drivehq requires at least 5 minutes of sleep

    done;

done;

