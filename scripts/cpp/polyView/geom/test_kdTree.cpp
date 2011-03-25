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
  // Pts will be reordered but otherwise unchanged inside of this function.
  // Do not modify this vector afterward.
  T.formTree(Pts); 

  vector<Point> outPts; // Must be different than Pts
  double xl = 0, yl = 1, xh = 8, yh = 7;
  T.getPointsInBox(xl, yl, xh, yh, outPts);

  vector<Point> outPts2;
  outPts2.clear();
  for (int s = 0; s < numV; s++){
    const Point & P = Pts[s]; // alias
    if (xl <= P.x && P.x <= xh && yl <= P.y && P.y <= yh) outPts2.push_back(P);
  }


  cout << "Points in outPts" << endl;
  for (int s = 0; s < (int)outPts.size(); s++)
    cout << outPts[s].x << ' ' << outPts[s].y << endl;

  cout << endl << "Points in outPts2" << endl;
  for (int s = 0; s < (int)outPts2.size(); s++)
    cout << outPts2[s].x << ' ' << outPts2[s].y << endl;
  
  return 0;
}
