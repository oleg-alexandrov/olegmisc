#include <qstatusbar.h>
#include <qmessagebox.h>
#include <qmenubar.h>
#include <qapplication.h>
#include <qimage.h>
#include <iostream>
#include <stdlib.h>
#include "appWindow.h"
#include "polyView.h"
#include "utils.h"

using namespace std;
using namespace utils;

int main(int argc, char** argv){

  char * exeName = argv[0];

  int windowWidX, windowWidY;
  bool            useCmdLineColors;
  vector<string>  cmdLineColors, polyFilesVec;
  vector<bool>    plotPointsOnlyVec;
  bool            plotAsLines;
  bool            noClosedPolys; // Don't join the last vertex to the first one
  
  parseCmdOptions(// inputs
                  argc, argv, exeName,
                  // outputs
                  windowWidX, windowWidY,
                  useCmdLineColors, cmdLineColors,
                  polyFilesVec, plotPointsOnlyVec,
                  plotAsLines, noClosedPolys
                  );

  QApplication app(argc, argv);
  string progName = "polyView";
  
  appWindow m(NULL, progName, useCmdLineColors, cmdLineColors,
              polyFilesVec, plotPointsOnlyVec, plotAsLines, noClosedPolys,
              windowWidX, windowWidY);
  m.setCaption(progName);
  m.show();
  
  QObject::connect( qApp, SIGNAL(lastWindowClosed()), qApp, SLOT(quit()) );
  
  return app.exec();
}
