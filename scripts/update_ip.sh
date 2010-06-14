#!/bin/sh

cd $HOME/bin
rm -f index.html*
wget http://checkip.dyndns.com/
svn commit -m index

echo done


