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
  bool isPointCloud = false;
  cout << "Reading " << filename << endl;
  if (! poly.read_poly(filename, isPointCloud) ) exit(1);

  char * outFile = "out.xg";
  cout << "Writing to " << outFile << endl;
  poly.write_poly(outFile);
  
  return 0;
  
}
