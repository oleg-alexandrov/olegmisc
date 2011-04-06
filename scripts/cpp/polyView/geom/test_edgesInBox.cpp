#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cassert>
#include <limits>
#include "dPoly.h"
#include "polyUtils.h"
#include "geomUtils.h"

using namespace std;
using namespace utils;

int main(int argc, char** argv){

  if (argc <= 2){
    cout << "Usage: " << argv[0] << " poly.xg box.xg" << endl;
    exit(1);
  }
  
  char * polyFile   = argv[1];
  char * boxFile    = argv[2];
  bool isPointCloud = false;

  dPoly poly;
  if (! poly.readPoly(polyFile, isPointCloud) ) exit(1);

  dPoly box;
  if (! box.readPoly(boxFile, isPointCloud) ) exit(1);
  double xl, yl, xh, yh;
  box.bdBox(xl, yl, xh, yh);

  edgeTree T;
  T.putPolyEdgesInTree(poly);

  vector<seg> edgesInBox;
  T.findPolyEdgesInBox(xl, yl, xh, yh, // inputs
                       edgesInBox      // output
                       );
  
  const char * outFile = "edgesInBox.xg";
  cout << "Writing to " << outFile << endl;
  ofstream fs(outFile);
  fs << "color = red" << endl;
  for (int t = 0; t < (int)edgesInBox.size(); t++){
    const seg & S = edgesInBox[t];
    fs << S.begx  << ' ' << S.begy << endl;
    fs << S.endx  << ' ' << S.endy << endl;
    fs << "NEXT" << endl;
  }
  fs.close();

  return 0;
  
}
