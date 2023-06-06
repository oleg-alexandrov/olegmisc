#!/usr/bin/python

import sys, os, re

num = 0
sum_val = 0

with open(sys.argv[1]) as f:
    for line in f:
        sum_val += float(line)
        num += 1

print("average: " + str(sum_val / num))
