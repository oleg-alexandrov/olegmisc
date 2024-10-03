#!/bin/bash

# Format the elapsed time from the StereoPipelineTest logs

cd ~/projects/StereoPipelineTest

for f in */output.txt; do echo $f $(grep -i elapsed $f | tail -n 1); done > output.txt
perl -pi -e "s#\(\[hour.*?\n#\n#g" output.txt
perl -pi -e "s#/output.txt##g" output.txt
perl -pi -e "s#\s*./run.sh:\s*# #g" output.txt
perl -pi -e "s#\.\d*\s*\n#\n#g" output.txt
perl -pi -e "s#elapsed=##g" output.txt

# Handle the case where the elapsed time is in the format 1:23:45.
# Make it stand out as a big number. Should not happen.
perl -pi -e "s#(\d+):(\d+):(\d+)#1000\$1\.\$2\$3#g" output.txt

# Handle the case where the elapsed time is in the format 1:23.
# Make it numerical.
perl -pi -e "s#(\d+):(\d+)#\$1\.\$2#g" output.txt

# Sort by 2nd column and print
cat output.txt | sort -n -k 2
echo Saved to: output.txt
