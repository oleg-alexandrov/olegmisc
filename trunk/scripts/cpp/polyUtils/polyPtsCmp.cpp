#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <set>
#include "dPoly.h"

// Compare two polygons point-by-point. We assume there can be points
// on edges of polygons. If one polygon has a point repeated twice,
// but the second polygon has it repeated just once, this will be
// flagged as a difference.

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

  char *inFile1 = argv[1], *inFile2 = argv[2];
  dPoly P; P.readPoly(inFile1); multiset<dPoint> mP; putPolyInMultiSet(P, mP);
  dPoly Q; Q.readPoly(inFile2); multiset<dPoint> mQ; putPolyInMultiSet(Q, mQ);

  // If a point is in mP, and also in mQ, mark it as being in mP and wipe it from mQ
  vector<dPoint> shared;
  shared.clear();
  multiset<dPoint>::iterator ip, iq;
  for (ip = mP.begin(); ip != mP.end(); ip++){
    iq = mQ.find(*ip);
    if ( iq != mQ.end() ){
      shared.push_back(*ip);
      mQ.erase(iq); // Erase just the current instance of the given value
    }
  }
  
  // Wipe it from mP as well
  for (int s = 0; s < (int)shared.size(); s++){
    ip = mP.find(shared[s]);
    if ( ip != mP.end() ){
      mP.erase(ip); // Erase just the current instance of the given value
    }
  }
  
  char *outFile1 = "diff1.xg", *outFile2 = "diff2.xg", *color1 = "red", *color2 = "green";
  cout << "Writing points in " << inFile1 << ' ' << "not in " << inFile2 << " to "
       << outFile1 << endl;
  ofstream of1(outFile1);
  of1 << "color = " << color1 << endl;
  int count1 = 0;
  for (ip = mP.begin(); ip != mP.end(); ip++){
    of1 << ip->x << ' ' << ip->y << endl;
    count1++;
  }
  of1.close();
  cout << "Wrote " << count1 << " points" << endl;
  
  cout << "Writing points in " << inFile2 << ' ' << "not in " << inFile1 << " to "
       << outFile2 << endl;
  int count2 = 0;
  ofstream of2(outFile2);
  of2 << "color = " << color2 << endl;
  for (iq = mQ.begin(); iq != mQ.end(); iq++){
    of2 << iq->x << ' ' << iq->y << endl;
    count2++;
  }
  of2.close();
  cout << "Wrote " << count2 << " points" << endl;

  return 0;

}
