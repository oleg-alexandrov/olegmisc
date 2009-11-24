#!/bin/sh

cd /home/aoleg/public_html/wp/afd
rm -f index.html*
wget http://checkip.dyndns.com/
svn commit -m index

echo done


