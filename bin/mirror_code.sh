#!/bin/bash

cd $HOME/projects

rsync -avz --include-from=$HOME/bin/rsync_include.txt --exclude "*" visionworkbench/ visionworkbench_debug
rsync -avz --include-from=$HOME/bin/rsync_include.txt --exclude "*" StereoPipeline/ StereoPipeline_debug
