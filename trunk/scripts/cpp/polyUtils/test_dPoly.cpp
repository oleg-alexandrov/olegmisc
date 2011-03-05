#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cassert>
#include <limits>
#include "dPoly.h"

using namespace std;
using namespace utils;

int main(int argc, char** argv){

  dPoly poly;
  if (argc <= 1){
    cout << "Usage: " << argv[0] << " file.xg" << endl;
    exit(1);
  }
  
  char * filename = argv[1];
  cout << "Reading " << filename << endl;
  
  bool isPointCloud = false;

  if (! poly.readPoly(filename, isPointCloud) ) exit(1);

  //poly.sortFromLargestToSmallest();
  double * xv   = (double*)poly.get_xv(); // to do: fix this hack
  double * yv   = (double*)poly.get_yv();
  int      numV = poly.get_totalNumVerts();

  bool isClosedPolyLine = true;
  utils::snapPolyLineTo45DegAngles(isClosedPolyLine, numV, xv, yv);

  const char * outFile = "out.xg";
  cout << "Writing to " << outFile << endl;
  poly.writePoly(outFile);

  return 0;
  
}
