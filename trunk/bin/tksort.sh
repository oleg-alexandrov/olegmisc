#!/bin/bash

cat $1  | sort -n > /tmp/file1.txt
cat $2  | sort -n > /tmp/file2.txt

tkdiff /tmp/file1.txt /tmp/file2.txt
rm -fv /tmp/file1.txt /tmp/file2.txt
