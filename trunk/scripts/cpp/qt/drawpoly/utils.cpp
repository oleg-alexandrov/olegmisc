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

void utils::parseCmdOptions(//inputs
                            int argc, char** argv, char * exeName,
                            // outputs
                            int & windowWidX,      int & windowWidY,
                            std::vector<std::string>   & polyFilesVec, 
                            std::vector<bool>          & plotPointsOnlyVec
                            ){

  polyFilesVec.clear();
  plotPointsOnlyVec.clear();

  // Skip argv[0] as that's the program name
  extractWindowDims(argc - 1, argv + 1, windowWidX, windowWidY);

  bool plotPointsOnly = false; // plot the edges or just the vertices
  
  for (int argIter = 1; argIter < argc; argIter++){

    char * filename = argv[argIter];

    if (filename == NULL || strlen(filename) == 0) continue;

    if ( strstr(filename, "-h") || strstr(filename, "--h") ||
         strstr(filename, "-?") ){
      printUsage(exeName);
      exit(0);
    }

    if ( strstr(filename, "-p") ){
      plotPointsOnly = !plotPointsOnly;
      continue;
    }

    // Other command line options are ignored
    if (filename[0] == '-') continue;
    
    plotPointsOnlyVec.push_back(plotPointsOnly);
    polyFilesVec.push_back(string(filename));
  }
  
  if (polyFilesVec.size() == 0){
    cerr << "No polygons to plot" << endl;
    exit(1);
  }

  return;
}

  
