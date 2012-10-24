#!/bin/bash

echo Running $0 on $(date)

source ~/.bashenv 

HOST=$B
#HOST=$M

# Copy only files matching the given patterns
rsync -avz                                     \
    --exclude "Downloads/*"                    \
    --exclude ".svn/*"                         \
    --exclude "projects/base_system*"          \
    --exclude "projects/isis*"                 \
    --include-from=$HOME/bin/rsync_include.txt \
    --exclude "*"                              \
    $HOST: $HOME 

# Copy  entire directories
#for dir in Documents gitserver .git .emacs .emacs.d .xemacs; do 
for dir in Documents gitserver; do 
    rsync -avz --exclude "*.key" $HOST:$dir $HOME
done

