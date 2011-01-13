#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cassert>
#include <limits>
#include <fstream>
#include "dPoly.h"
#include "cutPoly.h"

using namespace std;
using namespace utils;

int main(int argc, char** argv){

  if (argc <= 1){
    cout << "Usage: " << argv[0] << " file.xg" << endl;
    exit(1);
  }
  
  char * filename = argv[1];
  bool isPointCloud = false;
  cout << "Reading " << filename << endl;

  dPoly poly;
  if (! poly.readPoly(filename, isPointCloud) ) exit(1);

  const double * xv       = poly.get_xv();
  const double * yv       = poly.get_yv();
  const int    * numVerts = poly.get_numVerts();
  int numPolys            = poly.get_numPolys();
  //int totalNumVerts     = poly.get_totalNumVerts();

//   double xll = 100, xur = 426;
//   double yll = 100, yur = 501.214;

  double w[] = {373.255, -54.6423, 1086, 4837};
    
  vector<double> cutX, cutY;
  vector<int> cutNumPolys;

  cutPoly(numPolys, numVerts, xv, yv, w[0], w[1], w[2], w[3], // inputs
          cutX, cutY, cutNumPolys                             // outputs
          );
  
  char * outFile = "clipped.xg";
  cout << "Writing to " << outFile << endl;
  //double scale = 1.0;
  vector<string> colorsOut, layersOut; 
  std::vector<anno>  annotations;
  // writePoly(outFile, isPointCloud,
  // cutX, cutY, cutNumPolys, cutNumPolys.size(), cutX.size(),
  // colorsOut, "yellow", scale, layersOut, annotations);

  ofstream win("window.xg");
  cout << "Writing to window.xg" << endl;
  win << "color = cyan" << endl;
  win << w[0] << ' ' << w[1] << endl;
  win << w[2] << ' ' << w[1] << endl;
  win << w[2] << ' ' << w[3] << endl;
  win << w[0] << ' ' << w[3] << endl;
  win.close();
  return 0;
  
}
