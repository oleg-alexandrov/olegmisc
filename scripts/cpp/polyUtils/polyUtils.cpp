#include <cmath>
#include <cstdlib>
#include <iostream>
#include <cmath>
#include <cfloat>
#include <cstring>
#include <cassert>
#include "geomUtils.h"
#include "dPoly.h"

using namespace std;
using namespace utils;

  
void utils::findClosestPolyAndDist(// inputs
                                   double x0, double y0,
                                   std::vector<dPoly> & polyVec,
                                   // outputs
                                   int & minVecIndex, int & minPolyIndex,
                                   double & min_dist
                                   ){

  min_dist     = DBL_MAX;
  minVecIndex  = -1;
  minPolyIndex = -1;
  
  for (int vecIter = 0; vecIter < (int)polyVec.size(); vecIter++){

    double dist   = DBL_MAX;
    int polyIndex = -1;
    polyVec[vecIter].findClosestPolyIndex(x0, y0,           // in
                                          polyIndex, dist   // out
                                          );

    if (dist <= min_dist){
      minVecIndex  = vecIter;
      minPolyIndex = polyIndex;
      min_dist     = dist;
    }
    
  }

  return;
}

void utils::findClosestPointAndDist(// inputs
                                    double x0, double y0,
                                    std::vector<dPoly> & polyVec,
                                    // outputs
                                    double & min_x, double & min_y,
                                    double & min_dist
                                    ){

  min_x = x0; min_y = y0; min_dist = DBL_MAX;
  
  for (int s = 0; s < (int)polyVec.size(); s++){

    double min_x0 = x0, min_y0 = y0, min_dist0 = DBL_MAX;
    polyVec[s].findClosestPointAndDist(x0, y0,                   // inputs
                                       min_x0, min_y0, min_dist0 // outputs
                                       );

    if (min_dist0 <= min_dist){
      min_dist = min_dist0;
      min_x    = min_x0;
      min_y    = min_y0;
    }
    
  }

  return;
}

void utils::findPolyDiff(const dPoly & P, const dPoly & Q, // inputs
                         std::vector<dPoint> & vP, std::vector<dPoint> & vQ // outputs
                         ){
    
  // Compare two polygons point-by-point. We assume that the polygons
  // may have collinear points.  If one polygon has a point repeated
  // twice, but the second polygon has it repeated just once, this
  // will be flagged as a difference as well.
  
  multiset<dPoint> mP; putPolyInMultiSet(P, mP);
  multiset<dPoint> mQ; putPolyInMultiSet(Q, mQ);

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

  vP.clear(); vQ.clear();
  for (ip = mP.begin(); ip != mP.end(); ip++){
    dPoint p;
    p.x = ip->x;
    p.y = ip->y;
    vP.push_back(p);
  }
  for (iq = mQ.begin(); iq != mQ.end(); iq++){
    dPoint q;
    q.x = iq->x;
    q.y = iq->y;
    vQ.push_back(q);
  }
  
  return;
}
