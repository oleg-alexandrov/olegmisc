#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cassert>
#include <limits>
#include "kdTree.h"
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

  double * xv   = (double*)poly.get_xv(); // to do: fix this hack
  double * yv   = (double*)poly.get_yv();
  int      numV = poly.get_totalNumVerts();


  vector<Point> Pts;
  Pts.reserve(numV);
  Pts.clear();
  for (int s = 0; s < numV; s++){
    Pts.push_back(Point(xv[s], yv[s]));
  }
  
  kdTree T;
  T.formTree(Pts); // Pts will be reordered but otherwise unchanged inside of this function
  
    
  return 0;
  
}
