#ifndef UTILS_H
#define UTILS_H
#include <cmath>
#include <iostream>
#include <string>
#include "../../polyUtils/dPoly.h"

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
                       std::vector<std::string>   & polyFilesVec, 
                       std::vector<bool>          & plotPointsOnlyVec
                       );

  inline void printUsage(char * progName){
    
    std::cout << "Usage: " << progName << " [ -geo 1000x800 ] file_1.xg ... "
              << "[ -p ] file_N.xg " << std::endl;

  }
  
}

#endif
