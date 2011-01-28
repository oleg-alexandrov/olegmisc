#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include "dPoly.h"

// Compare two polygons point-by-point. We assume there can be points on
// edges of polygons. If one polygon has a point repeated twice, but
// the second one does not, we will flag that as well.

using namespace std;

struct dPoint{
  double x, y;
};

bool operator< (dPoint P, dPoint Q){
  return ( P.x < Q.x ) || (P.x == Q.x && P.y < Q.y);
}

int main (int argc, char ** argv){

  if (argc < 3){
    cerr << "Usage: " << argv[0] << " file1.xg file2.xg" << endl;
    exit(1);
  }

  dPoly P; P.readPoly(argv[1]);
  dPoly Q; Q.readPoly(argv[2]);
  
  return 0;

}
