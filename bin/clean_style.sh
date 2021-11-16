#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 argName; exit; fi

for file in $*; do 
    perl -pi -e "s#\s*\n#\n#g" $file
    perl -pi -e "s#(if|while|for)(\()#\$1 \$2#g" $file
    perl -pi -e "s#(try)(\{)#\$1 \$2#g" $file
    perl -pi -e "s#(\))(\{)#\$1 \$2#g" $file
    perl -pi -e "s#([^\s\-=])(=)([^\s\-=])#\$1 \$2 \$3#g" $file # don't put < it is dangerous.
    perl -pi -e "s#([^\s])\s?(\/\/)#\$1  \$2#g" $file
    perl -pi -e "s#(//)([^\s])#\$1  \$2#g" $file
    perl -pi -e "s#\}\s*else\s*\{#} else {#g" $file
    perl -pi -e "s#(if\s*\([^\s].*)\s+(\)\s*\{)#\$1\$2#g" $file # consistent spaces around if
    perl -pi -e "s#(if\s*\(\s.*)\s*(\)\s*\{)#\$1\$2#g" $file # consistent spaces around if
    perl -pi -e "s#(if\s*\()\s(.*)\s*(\)\s*\{)#\$1\$2\$3#g" $file # consistent spaces around if
    perl -pi -e "s#(ofstream\s+\w+)\s+(\()#\$1\$2#g" $file
    perl -pi -e "s#[ \t]*,[ \t]*([^\n])#, \$1#g" $file # fix commas
    perl -pi -e "s#http\w*:\s*//\s*#https:\/\/#g" $file # fix https
    remote_copy.pl $file $M
done

