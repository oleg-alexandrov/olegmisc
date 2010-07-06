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
                              int & widX, int & widY
                              ){

  // Parse the command line arguments '-geo[metry] 500x600'

  widX = 900; widY = 700; // defaults
   
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
    int widX_tmp = atoi(pch);

    pch = strtok (NULL, delimiter);
    if (pch == NULL) continue;
    int widY_tmp = atoi(pch);
    
    if (widX_tmp > 0 && widY_tmp > 0){
      widX = widX_tmp;
      widY = widY_tmp;
    }
    
  }
  
}


  
