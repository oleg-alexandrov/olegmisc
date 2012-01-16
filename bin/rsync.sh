#!/bin/bash

# Copy only files matching the given patterns
rsync -avz                                     \
    --exclude "Downloads/*"                    \
    --exclude "projects/base_system*"          \
    --include-from=$HOME/bin/rsync_include.txt \
    --exclude "*"                              \
    $B: $HOME

# Copy  entire directories
rsync -avz $B:Documents $HOME
rsync -avz $B:gitserver $HOME
