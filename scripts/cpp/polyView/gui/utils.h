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
  forceClosedPoly, forceNonClosedPoly, readClosedPolyInfoFromFile
};
  

struct polyOptions{
  // Each polygon file has these options
  bool            plotAsPoints;
  bool            isPolyFilled;
  closedPolyInfo  isPolyClosed;
  int             lineWidth;
  bool            useCmdLineColor;
  std::string     bgColor;
  std::string     fgColor;
  std::string     cmdLineColor;
  std::string     polyFileName;
  
  polyOptions(){
    plotAsPoints    = false;
    isPolyFilled    = false;
    isPolyClosed    = readClosedPolyInfoFromFile;
    lineWidth       = 1;
    useCmdLineColor = false;
    bgColor         = "black";
    fgColor         = "white";
    cmdLineColor    = "green";
    polyFileName    = "unnamed.xg";
  }
  
};

struct cmdLineOptions{
  std::vector<polyOptions> polyOptionsVec;
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
