#!/bin/bash

echo Running $0 on $(date)

source ~/.bashenv 

ORIGIN=$B

machine=$(uname -n |grep -i pfe)
if [ "$machine" != "" ]; then
    DEST=/nobackupnfs1/$(whoami)
else
    DEST=$HOME
fi

# Copy only files matching the given patterns
rsync -avz                                     \
    --exclude "Downloads/*"                    \
    --exclude ".svn/*"                         \
    --exclude "projects/base_system*"          \
    --exclude "projects/isis*"                 \
    --include-from=$DEST/bin/rsync_include.txt \
    --exclude "*"                              \
    $ORIGIN: $DEST 

# Copy  entire directories
#for dir in Documents gitserver .git .emacs .emacs.d .xemacs; do 
for dir in Documents gitserver; do 
    rsync -avz --exclude "*.key" $ORIGIN:$dir $DEST
done


