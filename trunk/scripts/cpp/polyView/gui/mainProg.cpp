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
  cmdLineOptions options;
  
  parseCmdOptions(// inputs
                  argc, argv, exeName,
                  // outputs
                  windowWidX, windowWidY, options
                  );
  
  QApplication app(argc, argv);
  string progName = "polyView";
  
  appWindow m(NULL, progName, options, windowWidX, windowWidY);
  m.setCaption(progName.c_str());
  m.show();
  
  QObject::connect( qApp, SIGNAL(lastWindowClosed()), qApp, SLOT(quit()) );
  
  return app.exec();
}
