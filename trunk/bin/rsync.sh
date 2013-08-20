#!/bin/bash

echo Running $0 on $(date)

source ~/.bashenv

ORIGIN=$B

DEST=$HOME
if [ "$(uname -n |grep -i pfe)" != "" ]; then
    DEST=/nobackupnfs1/$(whoami)
fi
if [ "$(uname -n |grep -i zula)" != "" ]; then
    DEST=/media/raid/oleg/
fi

# Copy only files matching the given patterns
rsync -avz                                     \
    --exclude "Downloads/*"                    \
    --exclude "GNUstep/*"                      \
    --exclude ".svn/*"                         \
    --exclude "projects/base_system*"          \
    --exclude "projects/packages*"             \
    --exclude "projects/isis*"                 \
    --include-from=$DEST/bin/rsync_include.txt \
    --exclude "*"                              \
    $ORIGIN: $DEST 2>/dev/null

# # Copy  entire directories
# #for dir in Documents gitserver .git .emacs .emacs.d .xemacs; do
# for dir in Documents gitserver; do
#     rsync -avz --exclude "*.key" $ORIGIN:$dir $DEST
# done
