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

  if (argc < 3){
    cout << "Usage: " << argv[0] << " poly.xg box.xg" << endl;
    exit(1);
  }
  
  char * polyFile = argv[1];
  char * boxFile  = argv[2];
  cout << "Reading " << polyFile << " and " << boxFile << endl;
  
  bool isPointCloud = false;

  dPoly poly, box;
  if (! poly.readPoly(polyFile, isPointCloud) ) exit(1);
  if (! box.readPoly(boxFile,   isPointCloud) ) exit(1);

  double * xv   = (double*)poly.get_xv(); // to do: fix this hack
  double * yv   = (double*)poly.get_yv();
  int      numV = poly.get_totalNumVerts();

  double xl, yl, xh, yh; 
  box.bdBox(xl, yl, xh, yh);

  vector<Point> Pts;
  Pts.reserve(numV);
  Pts.clear();
  for (int s = 0; s < numV; s++){
    Pts.push_back(Point(xv[s], yv[s]));
  }
  
  kdTree T;
  // Pts will be reordered but otherwise unchanged inside of this function.
  // Do not modify this vector afterward.
  T.formTreeOfPoints(Pts); 

  vector<Point> outPts; // Must be different than Pts
  T.getPointsInBox(xl, yl, xh, yh, outPts);

  vector<Point> outPts2;
  outPts2.clear();
  for (int s = 0; s < numV; s++){
    const Point & P = Pts[s]; // alias
    if (xl <= P.x && P.x <= xh && yl <= P.y && P.y <= yh) outPts2.push_back(P);
  }

  assert(outPts.size() == outPts2.size());
  sort(outPts.begin(),  outPts.end(),  lexLessThan);
  sort(outPts2.begin(), outPts2.end(), lexLessThan);
  
  for (int s = 0; s < (int)outPts.size(); s++){
    const Point & P = outPts[s];
    const Point & P2 = outPts2[s];
    cout << P.x << ' ' << P.y << ' ' << P2.x << ' ' << P2.y << endl;
    assert(P.x == P2.x && P.y == P2.y);
  }
  
  cout << "Points in outPts" << endl;
//   for (int s = 0; s < (int)outPts.size(); s++)
//     cout << outPts[s].x << ' ' << outPts[s].y << endl;

  string outFile = "ptsInBox.xg";
  cout << "Writing to " << outFile << endl;
  ofstream of(outFile.c_str());
  of << "color = white" << endl;
  for (int s = 0; s < (int)outPts.size(); s++)
    of << outPts[s].x << ' ' << outPts[s].y << endl;
  of.close();
  
//   cout << endl << "Points in outPts2" << endl;
//   for (int s = 0; s < (int)outPts2.size(); s++)
//     cout << outPts2[s].x << ' ' << outPts2[s].y << endl;
  
  string outFile2 = "ptsInBox2.xg";
  cout << "Writing to " << outFile2 << endl;
  ofstream of2(outFile2.c_str());
  of2 << "color = green" << endl;
  for (int s = 0; s < (int)outPts2.size(); s++)
    of2 << outPts2[s].x << ' ' << outPts2[s].y << endl;
  of2.close();

  return 0;
}
