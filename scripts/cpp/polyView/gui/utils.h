#ifndef UTILS_H
#define UTILS_H
#include <cmath>
#include <iostream>
#include <string>
#include <vector>
#include "../geom/dPoly.h"

enum closedPolyInfo{
  // If an array of points as read from file has the first vertex equal to the last
  // one, we treat it is a closed polygon (last vertex joins the first vertex).
  // If the user wants to override this behavior, the first two fields below
  // become necessary.
  forceClosedPoly, forceNoClosedPoly, isClosedPolyFromFile
};
  

struct polyOptions{
  // Each polygon file has these options
  bool            plotAsPoints;
  bool            useCmdLineColor;
  int             lineWidth;
  closedPolyInfo  isClosedInfo;
  std::string     cmdLineColor;
  std::string     polyFileName;
};

struct cmdLineOptions{
  bool                     noClosedPolys;
  std::vector<bool>        plotPointsOnlyVec;
  bool                     useCmdLineColors; 
  int                      lineWidth;
  std::vector<std::string> cmdLineColors; 
  std::vector<std::string> polyFilesVec; 
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
