#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 machines [pull]; exit; fi

# Push or pull settings from machine.

machines=$*
cd $HOME

files=".bash_aliases .bash_login .bash_profile .bashrc .bashenv bin .zshrc"
for machine in $machines; do
    if [ "$machine" = "pull" ]; then continue; fi

    for file in $files; do 
        if  [ "$(echo $* | grep pull)" = "" ]; then 
            echo pushing $file to $machine
            rsync -avz $file $machine: 
        else
            echo pulling $file from $machine
            rsync -avz $machine:$file . 
        fi
    done
done    