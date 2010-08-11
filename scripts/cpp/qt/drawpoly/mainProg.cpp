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

  char * exeName = argv[0];

  int windowWidX, windowWidY;
  vector<string>  polyFilesVec;
  vector<bool>    plotPointsOnlyVec;
  
  parseCmdOptions(// inputs
                  argc, argv, exeName,
                  // outputs
                  windowWidX, windowWidY,
                  polyFilesVec, plotPointsOnlyVec
                  );

  QApplication app(argc, argv);
  char * progName = "drawPoly";
  
  appWindow m(NULL, progName, polyFilesVec, plotPointsOnlyVec,
              windowWidX, windowWidY);
  m.setCaption(progName);
  m.show();
  
  QObject::connect( qApp, SIGNAL(lastWindowClosed()), qApp, SLOT(quit()) );
  
  return app.exec();
}
