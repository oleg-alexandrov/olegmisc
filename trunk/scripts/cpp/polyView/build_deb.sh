#!/bin/sh

dpkg-deb --build debian
mv debian.deb polyView_0.5.deb

# Install (need to be root)
# dpkg -i ./polyView_0.5.deb

# Uninstall
# dpkg -r polyView

# See what libraries an executable depends on:
# ldd exeName

# See which libraries are installed in Ubuntu:
# dpkg --get-selections

# Find the Ubuntu version:
# cat /etc/issue


