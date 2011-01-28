#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <set>
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

void putPolyInMultiSet(const dPoly & P, multiset<dPoint> & mP){

  const double * x = P.get_xv();
  const double * y = P.get_yv();

  mP.clear();
  
  int numVerts = P.get_totalNumVerts();
  for (int v = 0; v < numVerts; v++){
    dPoint P;
    P.x = x[v];
    P.y = y[v];
    mP.insert(P);
  }

  return;
}

int main (int argc, char ** argv){

  if (argc < 3){
    cerr << "Usage: " << argv[0] << " file1.xg file2.xg" << endl;
    exit(1);
  }

  dPoly P; P.readPoly(argv[1]); multiset<dPoint> mP; putPolyInMultiSet(P, mP);
  dPoly Q; Q.readPoly(argv[2]); multiset<dPoint> mQ; putPolyInMultiSet(Q, mQ);

  // Put here points in both multi-sets
  vector<dPoint> shared;
  shared.clear();

  multiset<dPoint>::iterator it;
  for (it = mP.begin(); it != mP.end(); it++){
    if ( mQ.find(*it) != mQ.end() ) shared.push_back(*it);
  }

  for (int s = 0; s < (int)shared.size(); s++) mP.erase(shared[s]);
  for (int s = 0; s < (int)shared.size(); s++) mQ.erase(shared[s]);

  char *outFile = "diff.xg", *color1 = "red", *color2 = "green";

  cout << "Writing the differences to " << outFile << endl;
  cout << "Points in " << argv[1] << ' ' << " not in " << argv[2] << " are in "
       << color1  << endl;
  cout << "Points in " << argv[2] << ' ' << " not in " << argv[1] << " are in "
       << color2  << endl;
  
  ofstream of(outFile);
  of << "color = " << color1 << endl;
  for (it = mP.begin(); it != mP.end(); it++) of << it->x << ' ' << it->y << endl;
  of << "color = " << color2 << endl;
  for (it = mQ.begin(); it != mQ.end(); it++) of << it->x << ' ' << it->y << endl;
  of.close();

  return 0;

}
