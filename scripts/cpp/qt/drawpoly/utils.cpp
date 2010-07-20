#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <limits>
#include <cstring>
#include "utils.h"

using namespace std;

void utils::extractWindowDims(// inputs
                              int numArgs, char ** args,
                              // outputs
                              int & windowWidX, int & windowWidY
                              ){

  // Parse the command line arguments '-geo[metry] 500x600'

  windowWidX = 900; windowWidY = 700; // defaults
   
  for (int s = 1; s < numArgs; s++){

    if ( !strstr(args[s-1], "-geo") ) continue;

    string lineStr = args[s];
    char * line    = (char*) lineStr.c_str();
    
    // Blank the geometry settings once located
    // to not confuse other parsers.
    args[s-1][0] = '\0';
    args[s  ][0] = '\0';
    
    char * pch, * delimiter = "x";
    
    pch = strtok (line, delimiter);
    if (pch == NULL) continue;
    int windowWidX_tmp = atoi(pch);

    pch = strtok (NULL, delimiter);
    if (pch == NULL) continue;
    int windowWidY_tmp = atoi(pch);
    
    if (windowWidX_tmp > 0 && windowWidY_tmp > 0){
      windowWidX = windowWidX_tmp;
      windowWidY = windowWidY_tmp;
    }
    
  }
  
}

void utils::parseCmdOptionsLoadData(//inputs
                                    int argc, char** argv, char * progName,
                                    int yFactor,
                                    // outputs
                                    int & windowWidX, int & windowWidY,
                                    std::vector<xg_poly>  & polyVec, 
                                    std::vector<bool>     & plotPointsOnlyVec
                                    ){

  // To do: Loading better happen in the drawpoly class where the
  // saving happens too. Here it is enough just to extract the
  // filenames and parse the other optons.
  
  polyVec.resize(argc);
  plotPointsOnlyVec.clear();

  // Skip argv[0] as that's the program name
  extractWindowDims(argc - 1, argv + 1, windowWidX, windowWidY);

  bool plotPointsOnly = false; // plot the edges or just the vertices
  
  int numClips = 0;
  for (int argIter = 1; argIter < argc; argIter++){

    char * filename = argv[argIter];

    if ( strstr(filename, "-h") || strstr(filename, "--h") ||
         strstr(filename, "-?") ){
      printUsage(progName);
      exit(0);
    }

    if (strlen(filename) == 0) continue;

    if ( strstr(filename, "-p") ){
      plotPointsOnly = !plotPointsOnly;
      continue;
    }
    plotPointsOnlyVec.push_back(plotPointsOnly);
    
    //cout << "Reading " << filename << endl;
    if ( ! polyVec[numClips].read_poly(filename, plotPointsOnly) ) exit(1);

    // Flip the polygons to compensate for Qt's origin
    // being in the upper-right corner
    double * xv = (double*)polyVec[numClips].get_xv();
    double * yv = (double*)polyVec[numClips].get_yv();
    int numV    = polyVec[numClips].get_totalNumVerts();
    for (int s = 0; s < numV; s++){
      xv[s] = xv[s];
      yv[s] = yFactor*yv[s];
    }

    // Flip the annotations as well
    std::vector<anno> annotations;
    polyVec[numClips].get_annotations(annotations);
    for (int s = 0; s < (int)annotations.size(); s++){
      annotations[s].y *= yFactor;
    }
    polyVec[numClips].set_annotations(annotations);
      
    numClips++;
    
  }
  
  if (numClips == 0){
    cerr << "No polygons to plot" << endl;
    exit(1);
  }
  polyVec.resize(numClips);

  return;
}

  
