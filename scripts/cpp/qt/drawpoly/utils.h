#ifndef UTILS_H
#define UTILS_H
#include <cmath>
#include <iostream>
#include <string>
#include <vector>
#include "../../polyUtils/dPoly.h"

namespace utils{

  void findClosestPointAndDist(// inputs
                               double x0, double y0,
                               std::vector<dPoly> & polyVec,
                               // outputs
                               double & min_x, double & min_y,
                               double & min_dist
                               );
  
  void findClosestPolyAndDist(// inputs
                              double x0, double y0,
                              std::vector<dPoly> & polyVec,
                              // outputs
                              int & minVecIndex, int & minPolyIndex,
                              double & min_dist
                              );

  void extractWindowDims(// inputs
                         int numArgs, char ** args,
                         // outputs
                         int & windowWidX, int & windowWidY
                         );

  void parseCmdOptions(//inputs
                       int argc, char** argv, char * exeName,
                       // outputs
                       int & windowWidX,      int & windowWidY,
                       bool                       & useCmdLineColors, 
                       std::vector<std::string>   & cmdLineColors, 
                       std::vector<std::string>   & polyFilesVec, 
                       std::vector<bool>          & plotPointsOnlyVec,
                       bool                       & plotAsLines,
                       bool                       & noClosedPolys
                       );

  std::string inFileToOutFile(const std::string & inFile);

  void printUsage(char * progName);

  std::string getFilenameExtension(std::string filename);

}

#endif
