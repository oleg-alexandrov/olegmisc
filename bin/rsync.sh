#!/bin/bash

rsync -avz                                     \
    --exclude "Downloads/*"                    \
    --exclude "projects/base_system*"          \
    --include-from=$HOME/bin/rsync_include.txt \
    --exclude "*"                              \
    $B: $HOME

