#ifndef UTILS_H
#define UTILS_H
#include <cmath>
#include <iostream>
#include <string>
#include <vector>
#include "../geom/dPoly.h"

namespace utils{

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
  std::string replaceAll(std::string result, 
                         const std::string & replaceWhat, 
                         const std::string & replaceWithWhat);

}

#endif
