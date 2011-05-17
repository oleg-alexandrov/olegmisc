#    CONFIG += qt warn_on release
#    TARGET  = polyView
#
#    QT += qt3support


#The following line was inserted by qt3to4
#QT +=  
TEMPLATE        = app
TARGET          = polyView

QT              +=  qt3support


SOURCES = mainProg.cpp polyView.cpp appWindow.cpp utils.cpp  ../geom/dPoly.cpp ../geom/cutPoly.cpp ../geom/geomUtils.cpp ../geom/polyUtils.cpp ../geom/edgeUtils.cpp ../geom/dTree.cpp
HEADERS = polyView.h appWindow.h utils.h ../geom/dPoly.h ../geom/cutPoly.h  ../geom/geomUtils.h ../geom/polyUtils.h ../geom/edgeUtils.h ../geom/dTree.h ../geom/baseUtils.h

#HEADERS         = acuTuneMainWin.h 
#SOURCES         = main.cpp acuTuneMainWin.cpp

#FORMS           = acuTuneMainWinWidget.ui 

build_all:!build_pass {
    CONFIG -= build_all
    CONFIG += release
}

# install
target.path = $$[QT_INSTALL_DEMOS]/acutune
sources.files = $$SOURCES $$HEADERS $$FORMS *.pro
sources.path = $$[QT_INSTALL_DEMOS]/acutune
INSTALLS += target sources
