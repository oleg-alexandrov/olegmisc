#!/bin/bash

echo Running $0 on $(date)

source ~/.bashenv
source ~/.bash_profile # Get the new home dir from here
ORIGIN=$M

DEST=$HOME

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
