#!/bin/bash

echo Running $0 on $(date)

source ~/.bashenv 

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

for dir in visionworkbench StereoPipeline; do 
    rsync -avz --exclude "*.o" --exclude ".deps" \
        --exclude ".libs*" --exclude "tests/*"   \
        $B:projects/$dir $HOME/projects
done
