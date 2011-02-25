#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <limits>
#include <cstring>
#include "utils.h"

using namespace std;

void utils::printUsage(char * progName){
  
  std::cout << "Usage: " << progName
            << " [ -geo[metry] 1000x800 ] [ -c[olor] yellow ] file_1.xg ... "
            << "[ -p[oints] ] [ -nc | -noClosedPolys ] [ -l | -linesOnly ] file_N.xg "
            << std::endl;
  
}

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
                            bool                       & useCmdLineColors, 
                            std::vector<std::string>   & cmdLineColors, 
                            std::vector<std::string>   & polyFilesVec, 
                            std::vector<bool>          & plotPointsOnlyVec,
                            bool                       & plotAsLines,
                            bool                       & noClosedPolys
                            ){

  useCmdLineColors = false;
  plotAsLines      = false;
  noClosedPolys    = false;
  cmdLineColors.clear();
  polyFilesVec.clear();
  plotPointsOnlyVec.clear();

  // Skip argv[0] as that's the program name
  extractWindowDims(argc - 1, argv + 1, windowWidX, windowWidY);

  string color        = "yellow"; // default command line color
  bool plotPointsOnly = false; // plot the edges or just the vertices
  
  for (int argIter = 1; argIter < argc; argIter++){

    char * currArg = argv[argIter];

    if (currArg == NULL || strlen(currArg) == 0) continue;

    if (currArg[0] == '-'){
      // Transform -P into -p, etc.
      transform(currArg, currArg + strlen(currArg), currArg, ::tolower);
    }

    if (strncmp (currArg, "-?",  2) == 0 ||
        strncmp (currArg, "-h",  2) == 0 ||
        strncmp (currArg, "--h", 3) == 0){
      printUsage(exeName);
      exit(0);
    }

    if ( strncmp (currArg, "-p",  strlen("-p")) == 0 ){
      plotPointsOnly = !plotPointsOnly;
      continue;
    }

    if ( strncmp (currArg, "-l",          strlen("-l")) == 0 ||
         strncmp (currArg, "-linesonly",  strlen("-linesonly")) == 0 ){
      plotAsLines = true;
      continue;
    }

    if ( strncmp (currArg, "-nc",             strlen("-nc"))            == 0 || 
         strncmp (currArg, "-noclosedpolys",  strlen("-noclosedpolys")) == 0
         ){
      noClosedPolys = true;
      continue;
    }

    if ( strncmp (currArg, "-c",  2) == 0 && argIter < argc - 1){
      useCmdLineColors = true;
      color = argv[argIter + 1];
      argIter++;
      continue;
    }

    // Other command line options are ignored
    if (currArg[0] == '-') continue;
    
    cmdLineColors.push_back(color);
    polyFilesVec.push_back(currArg);
    plotPointsOnlyVec.push_back(plotPointsOnly);
  }
  
  return;
}

  
void utils::findClosestPointAndDist(// inputs
                                    double x0, double y0,
                                    std::vector<dPoly> & polyVec,
                                    // outputs
                                    double & min_x, double & min_y,
                                    double & min_dist
                                    ){

  min_x = x0; min_y = y0; min_dist = DBL_MAX;
  
  for (int s = 0; s < (int)polyVec.size(); s++){

    double min_x0 = x0, min_y0 = y0, min_dist0 = DBL_MAX;
    polyVec[s].findClosestPointAndDist(x0, y0,                   // inputs
                                       min_x0, min_y0, min_dist0 // outputs
                                       );

    if (min_dist0 <= min_dist){
      min_dist = min_dist0;
      min_x    = min_x0;
      min_y    = min_y0;
    }
    
  }

  return;
}

void utils::findClosestPolyAndDist(// inputs
                                   double x0, double y0,
                                   std::vector<dPoly> & polyVec,
                                   // outputs
                                   int & minVecIndex, int & minPolyIndex,
                                   double & min_dist
                                   ){

  min_dist     = DBL_MAX;
  minVecIndex  = -1;
  minPolyIndex = -1;
  
  for (int vecIter = 0; vecIter < (int)polyVec.size(); vecIter++){

    double dist   = DBL_MAX;
    int polyIndex = -1;
    polyVec[vecIter].findClosestPolyIndex(x0, y0,           // in
                                          polyIndex, dist   // out
                                          );

    if (dist <= min_dist){
      minVecIndex  = vecIter;
      minPolyIndex = polyIndex;
      min_dist     = dist;
    }
    
  }

  return;
}

std::string utils::inFileToOutFile(const std::string & inFile){

  string outFile = "";

  bool lastDot = true;
  for (int s = (int)inFile.length() - 1; s >= 0; s--){

    string currChar = inFile.substr(s, 1);
    if ( currChar == "/"){
      break; // strip path
    }else if (currChar == "." && lastDot){
      outFile = string("_out") + currChar + outFile;
      lastDot = false;
    }else{
      outFile = currChar + outFile;
    }
    
  }

  if (outFile.length() == 0){
    cerr << "Invalid filename" << endl;
  }
  
  return outFile;
  
}


