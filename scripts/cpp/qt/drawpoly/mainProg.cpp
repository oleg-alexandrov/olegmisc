#include <qstatusbar.h>
#include <qmessagebox.h>
#include <qmenubar.h>
#include <qapplication.h>
#include <qimage.h>
#include <iostream>
#include <stdlib.h>
#include "appWindow.h"
#include "drawpoly.h"
#include "utils.h"

using namespace std;
using namespace utils;

int main(int argc, char** argv){

  char * progName = argv[0];
  if (argc < 2){
    printUsage(progName);
    exit(1);
  }

  int yFactor = -1; // To compensate for Qt's origin in the upper-left corner
  int windowWidX, windowWidY;
  vector<dPoly> polyVec;          
  vector<bool>    plotPointsOnlyVec;
  parseCmdOptionsLoadData(//inputs
                          argc, argv, progName, yFactor,
                          // outputs
                          windowWidX, windowWidY,
                          polyVec, plotPointsOnlyVec
                          );

  QApplication app(argc, argv);
  char * name = "drawPoly";
  
  appWindow m(NULL,  name, polyVec, plotPointsOnlyVec,
              yFactor, windowWidX, windowWidY);
  m.resize(windowWidX, windowWidY);
  m.setCaption(name);
  if ( QApplication::desktop()->width() > m.width() + 10
       && QApplication::desktop()->height() > m.height() +30 )
    m.show();
  else
    m.showMaximized();
  
  QObject::connect( qApp, SIGNAL(lastWindowClosed()), qApp, SLOT(quit()) );
  
  return app.exec();
}
