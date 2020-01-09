#!/bin/bash


# Alias management

# Refresh the aliases
if [ -f ~/.unaliases    ]; then source ~/.unaliases;    fi;
if [ -f ~/.bash_aliases ]; then source ~/.bash_aliases; fi;

#echo "Input is" \'"$0"\' \'"$*"\'

if [ "$*" ]; then
    # Set the alias, and then display and save it
    alias "$*" > /dev/null # set the alias
    ans=$( alias "$*" | perl -pi -e 's#(^|\n)(\w+=)#$1 . "a " . $2#eg' );
    if [ "$ans" != "" ]; then echo "$ans"; fi; # echo it
    alias > ~/.bash_aliases;
    perl -pi -e "s#^([^\s]+=)#alias \$1#" ~/.bash_aliases;
else
    alias;  # Just list the aliases
fi

# # Pass this alias to byss, the master machine
# machine=$(uname -n | grep byss);
# if [ "$machine" == "" ]; then
#     echo ssh byss -X "$0" "$*" 2>/dev/null
#     ssh byss -X "$0" "$*" 2>/dev/null
# fi