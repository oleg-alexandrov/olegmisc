#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include "dPoly.h"
#include "geomUtils.h"
#include "polyUtils.h"

// Compare two polygons point-by-point. We assume there can be points
// on edges of polygons. If one polygon has a point repeated twice,
// but the second polygon has it repeated just once, this will be
// flagged as a difference.

using namespace std;
using namespace utils;

int main (int argc, char ** argv){

  if (argc < 3){
    cerr << "Usage: " << argv[0] << " file1.xg file2.xg" << endl;
    exit(1);
  }

  vector<dPoint>  vP, vQ;
  
  char *inFile1 = argv[1], *inFile2 = argv[2];
  dPoly P; if (! P.readPoly(inFile1) ) exit(1);
  dPoly Q; if (! Q.readPoly(inFile2) ) exit(1);

  findPolyDiff(P, Q,  // inputs
               vP, vQ // outputs
               );

  string outFile1 = "diff1.xg", outFile2 = "diff2.xg", color1 = "red", color2 = "green";
  cout << "Writing points in " << inFile1 << ' ' << "not in " << inFile2 << " to "
       << outFile1 << endl;
  ofstream of1(outFile1.c_str());
  of1 << "color = " << color1 << endl;
  int count1 = 0;
  for (int s = 0; s < (int)vP.size(); s++){
    dPoint p = vP[s];
    of1 << p.x << ' ' << p.y << endl;
    count1++;
  }
  of1.close();
  cout << "Wrote " << count1 << " points" << endl;
  
  cout << "Writing points in " << inFile2 << ' ' << "not in " << inFile1 << " to "
       << outFile2 << endl;
  int count2 = 0;
  ofstream of2(outFile2.c_str());
  of2 << "color = " << color2 << endl;
  for (int s = 0; s < (int)vQ.size(); s++){
    dPoint q = vQ[s];
    of2 << q.x << ' ' << q.y << endl;
    count2++;
  }
  of2.close();
  cout << "Wrote " << count2 << " points" << endl;

  return 0;

}
