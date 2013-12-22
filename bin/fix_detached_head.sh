#!/bin/bash

# attach detached head
git branch temp; git checkout temp
git branch -f master temp
git checkout master
git rebase origin/master
git branch -d temp
