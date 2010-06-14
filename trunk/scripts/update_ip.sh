#!/bin/sh

cd /home/aoleg/bin
rm -f index.html*
wget http://checkip.dyndns.com/
svn commit -m index

echo done


