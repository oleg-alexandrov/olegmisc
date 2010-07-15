#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cassert>
#include <limits>
#include <fstream>
#include "xg_poly.h"
#include "cutPoly.h"

using namespace std;
using namespace utils;

int main(int argc, char** argv){

  xg_poly poly;
  if (argc <= 1){
    cout << "Usage: " << argv[0] << " file.xg" << endl;
    exit(1);
  }
  
  char * filename = argv[1];
  cout << "Reading " << filename << endl;
  if (! poly.read_poly(filename) ) exit(1);

  const double * xv       = poly.get_xv();
  const double * yv       = poly.get_yv();
  const int    * numVerts = poly.get_numVerts();
  int numPolys            = poly.get_numPolys();
  //int totalNumVerts     = poly.get_totalNumVerts();

  double xll = 100, xur = 426;
  double yll = 100, yur = 501.214;

  vector<double> cutX, cutY;
  vector<int> cutNumPolys;

  cutPoly(numPolys, numVerts, xv, yv, xll, yll, xur, yur, // inputs
          cutX, cutY, cutNumPolys                         // outputs
          );
  
  char * outFile = "clipped.xg";
  cout << "Writing to " << outFile << endl;
  double scale = 1.0;
  char * layer = "";
  vector<string> colorsOut, layersOut; 
  write_xg(outFile, cutX, cutY, cutNumPolys, cutNumPolys.size(), cutX.size(),
           colorsOut, "yellow", scale, layersOut, layer);

  return 0;
  
}
