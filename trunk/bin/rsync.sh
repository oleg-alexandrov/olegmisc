#!/bin/bash

echo Running $0 on $(date)

source ~/.bashenv 

# Copy only files matching the given patterns
rsync -avz                                     \
    --exclude "Downloads/*"                    \
    --exclude ".svn/*"                         \
    --exclude "projects/base_system*"          \
    --include-from=$HOME/bin/rsync_include.txt \
    --exclude "*"                              \
    $B: $HOME 

# Copy  entire directories
#for dir in Documents gitserver .git .emacs .emacs.d .xemacs; do 
for dir in Documents gitserver .git; do 
    rsync -avz --exclude "*.key" $B:$dir $HOME
done
# for dir in visionworkbench StereoPipeline PhotometryTK; do 
#     rsync -avz --exclude "*.o" --exclude "*.a" --exclude "*.a" --exclude ".deps" \
#         --exclude ".libs*"                       \
#         $B:projects/$dir $HOME/projects
# done
 
