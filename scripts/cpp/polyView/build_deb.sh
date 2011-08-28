#!/bin/sh

cp control debian/DEBIAN 
dpkg-deb --build debian
mv debian.deb polyView_0.5.deb

# Install (need to be root)
# dpkg -i ./polyView_0.5.deb

# Uninstall
# dpkg -r polyView



