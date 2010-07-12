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

  if (argc < 2){
    cerr << "Usage: " << argv[0] <<  " poly.xg" << endl;
    exit(1);
  }

  int widX, widY;
  extractWindowDims(argc - 1, argv + 1, widX, widY); // argv[0] is prog name

  bool plotVertsOnly = false; // plot the edges or just the vertices

  int yFactor = -1; // To compensate for Qt's origin in the upper-left corner

  vector<xg_poly> polyVec;          polyVec.resize(argc);
  vector<bool>    plotVertsOnlyVec; plotVertsOnlyVec.clear();
  
  int numClips = 0;
  for (int argIter = 1; argIter < argc; argIter++){

    char * filename = argv[argIter];
    if (strlen(filename) == 0) continue;

    if ( strstr(filename, "-p") ){
      plotVertsOnly = !plotVertsOnly;
      continue;
    }
    
    //cout << "Reading " << filename << endl;
    if ( ! polyVec[numClips].read_poly(filename) ) exit(1);
  
    double * xv = (double*)polyVec[numClips].get_xv();
    double * yv = (double*)polyVec[numClips].get_yv();
    int numV    = polyVec[numClips].get_totalNumVerts();
    for (int s = 0; s < numV; s++){
      xv[s] = xv[s];
      yv[s] = yFactor*yv[s];
    }

    // Flip the annotations as well
    std::vector<anno> annotations = polyVec[numClips].get_annotations();
    for (int s = 0; s < (int)annotations.size(); s++){
      annotations[s].y *= yFactor;
    }
    polyVec[numClips].set_annotations(annotations);

    plotVertsOnlyVec.push_back(plotVertsOnly);
    
    numClips++;
    
  }

  if (numClips == 0){
    cerr << "No polygons to plot" << endl;
    exit(1);
  }
  polyVec.resize(numClips);

  QApplication app(argc, argv);
  char * name = "polyViewer";
  
  appWindow m(NULL,  name, polyVec, plotVertsOnlyVec,
              yFactor, widX, widY);
  m.resize(widX, widY);
  m.setCaption(name);
  if ( QApplication::desktop()->width() > m.width() + 10
       && QApplication::desktop()->height() > m.height() +30 )
    m.show();
  else
    m.showMaximized();
  
  QObject::connect( qApp, SIGNAL(lastWindowClosed()), qApp, SLOT(quit()) );
  
  return app.exec();
}
