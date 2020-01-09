#!/bin/bash

export D=/tmp/BinaryBuilderQCnjhE/install
export E=$D

export D=/usr/lib64/qt4
export E=/usr

export CXXFLAGS="-I$E/include -I$E/include/QtCore -I$E/include/QtOpenGL -I$E/include/QtDBus -I$E/include/Qt -I$E/include/Qt3Support -I$E/include/QtCore -I$E/include/QtDBus -I$E/include/QtDesigner -I$E/include/QtGui -I$E/include/QtHelp -I$E/include/QtMultimedia -I$E/include/QtNetwork -I$E/include/QtOpenGL -I$E/include/QtSql -I$E/include/QtSvg -I$E/include/QtTest -I$E/include/QtUiTools -I$E/include/QtXml -I$E/include/QtXmlPatterns -I$E/include/gsl -Wall -Wextra -Wno-unused-parameter "
export QT_CXXFLAGS="-I$D/include -I$D/include/QtCore -I$D/include/QtOpenGL -I$D/include/QtDBus -I$D/include/Qt -I$D/include/Qt3Support -I$D/include/QtCore -I$D/include/QtDBus -I$D/include/QtDesigner -I$D/include/QtGui -I$D/include/QtHelp -I$D/include/QtMultimedia -I$D/include/QtNetwork -I$D/include/QtOpenGL -I$D/include/QtSql -I$D/include/QtSvg -I$D/include/QtTest -I$D/include/QtUiTools -I$D/include/QtXml -I$D/include/QtXmlPatterns -I$D/include/gsl -Wall -Wextra -Wno-unused-parameter "

export PATH=$D/bin:$PATH
export LD_LIBRARY_PATH=$D/lib

./configure --with-qt_qmake=$D --with-qt=$D --enable-qt-qmake --enable-module-gui --with-qt=$D --with-x11
