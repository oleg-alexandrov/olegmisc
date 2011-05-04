#ifndef UTILS_H
#define UTILS_H
#include <cmath>
#include <iostream>
#include <string>
#include <vector>
#include "../geom/dPoly.h"

struct cmdLineOptions{
  bool                     noClosedPolys;
  bool                     useCmdLineColors; 
  int                      lineWidth;
  std::vector<std::string> cmdLineColors; 
  std::vector<std::string> polyFilesVec; 
  std::vector<bool>        plotPointsOnlyVec;
};

namespace utils{

  void extractWindowDims(// inputs
                         int numArgs, char ** args,
                         // outputs
                         int & windowWidX, int & windowWidY
                         );

  void parseCmdOptions(//inputs
                       int argc, char** argv, std::string exeName,
                       // outputs
                       int & windowWidX, int & windowWidY, cmdLineOptions & options
                       );

  std::string inFileToOutFile(const std::string & inFile);

  void printUsage(std::string progName);

  std::string getFilenameExtension(std::string filename);
  std::string replaceAll(std::string result, 
                         const std::string & replaceWhat, 
                         const std::string & replaceWithWhat);

}

#endif
