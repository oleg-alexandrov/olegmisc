#include <cmath>
#include <cstdlib>
#include <iostream>
#include <cmath>
#include <cfloat>
#include <cstring>
#include <cassert>
#include "geomUtils.h"
#include "polyUtils.h"
#include "dPoly.h"

using namespace std;
using namespace utils;

  
void utils::findClosestPolyEdge(// inputs
                                double x0, double y0,
                                std::vector<dPoly> & polyVec,
                                // outputs
                                int & minVecIndex, int & minPolyIndex,
                                double & minX, double & minY, double & minDist
                                ){

  minVecIndex  = -1;
  minPolyIndex = -1;
  minX         = DBL_MAX;
  minY         = DBL_MAX;
  minDist      = DBL_MAX;
  
  for (int vecIter = 0; vecIter < (int)polyVec.size(); vecIter++){

    double lx = DBL_MAX, ly = DBL_MAX, ldist = DBL_MAX;
    int polyIndex = -1;
    polyVec[vecIter].findClosestPolyEdge(x0, y0,                   // in
                                         polyIndex, lx, ly, ldist  // out
                                         );

    if (ldist <= minDist){
      minVecIndex  = vecIter;
      minPolyIndex = polyIndex;
      minX         = lx;
      minY         = ly;
      minDist      = ldist;
    }
    
  }

  return;
}

void utils::findClosestPointAndDist(// inputs
                                    double x0, double y0,
                                    const std::vector<dPoly> & polyVec,
                                    // outputs
                                    double & minX, double & minY,
                                    double & minDist
                                    ){

  minX = x0; minY = y0; minDist = DBL_MAX;
  
  for (int s = 0; s < (int)polyVec.size(); s++){

    double minX0 = x0, minY0 = y0, minDist0 = DBL_MAX;
    polyVec[s].findClosestPointAndDist(x0, y0,                // inputs
                                       minX0, minY0, minDist0 // outputs
                                       );

    if (minDist0 <= minDist){
      minDist = minDist0;
      minX    = minX0;
      minY    = minY0;
    }
    
  }

  return;
}

void utils::findAndSortDistsBwPolys(// inputs
                                    const std::vector<dPoly> & polyVec1,
                                    const std::vector<dPoly> & polyVec2,
                                    // outputs
                                    std::vector<segDist> & distVec
                                    ){

  // Given two vectors of polygons, for each vertex in each polygon in
  // the first vector find the distance to the closest vertex in the
  // second vector of polygons, and the segment on which that distance
  // is achieved.

  // The complexity of this is O(m*n) where m and n are the total
  // number of vertices in the first and second vector
  // respectively. This will be slow for large m and n, and can be
  // sped up using kd-trees or some hashing approach.

  // To do: Find the distance to closest edge, not closest vertex.
  // Rename all functions to "closestVertex" and "closestEdge" as
  // appropriate.
  
  distVec.clear();

  for (int s = 0; s < (int)polyVec1.size(); s++){

    const dPoly  & P = polyVec1[s]; // alias
    const double * x = P.get_xv();
    const double * y = P.get_yv();
    int numVerts     = P.get_totalNumVerts();

    for (int t  = 0; t < numVerts; t++){
      
      double minX, minY, minDist = DBL_MAX;
      findClosestPointAndDist(// inputs
                              x[t], y[t], polyVec2,  
                              // outputs
                              minX, minY,  minDist
                              );

      if (minDist != DBL_MAX)
        distVec.push_back(segDist(x[t], y[t], minX, minY, minDist));
      
    }
    
  }

  sort(distVec.begin(), distVec.end(), segDistGreaterThan);
  
  return;
}

void utils::putPolyInMultiSet(const dPoly & P, std::multiset<dPoint> & mP){

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

