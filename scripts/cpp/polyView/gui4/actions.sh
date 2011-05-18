#!/bin/bash

fun=$1
perl -pi -e "s#([ ]*\/\/\s*actions.*?\n)#  void $fun\(\);\n\$1#g" polyView.h

perl -pi -e "s#([ ]*\/\/\s*actions.*?\n)#  void $fun       \(\);\n\$1#g" appWindow.h

perl -pi -e "s#(\/\/\s*actions.*?\n)#void polyView::$fun\(\)\{\n  \n\n\}\n\n\$1#g" polyView.cpp
perl -pi -e "s#(\/\/\s*actions.*?\n)#void appWindow::$fun       \(\)\{ m_poly\-\>$1       \(\); \}\n\$1#g" appWindow.cpp
test
