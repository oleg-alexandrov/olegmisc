#include <cmath>
#include <cstdlib>
#include <iostream>
#include <cmath>
#include <cfloat>
#include <cstring>
#include <cassert>
#include "edgeUtils.h"
#include "dPoly.h"
#include "polyUtils.h"
#include "geomUtils.h"
#include "dTree.h"

using namespace std;
using namespace utils;

void utils::findClosestPolyVertex(// inputs
                                  double x0, double y0,
                                  const std::vector<dPoly> & polyVec,
                                  // outputs
                                  double & minX, double & minY,
                                  double & minDist
                                  ){

  // Find the closest point in a given vector of polygons to a given point.
  
  minX = x0; minY = y0; minDist = DBL_MAX;
  
  for (int s = 0; s < (int)polyVec.size(); s++){

    double minX0 = x0, minY0 = y0, minDist0 = DBL_MAX;
    polyVec[s].findClosestPolyVertex(x0, y0,                // inputs
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

void utils::findClosestPolyEdge(// inputs
                                double x0, double y0,
                                const std::vector<dPoly> & polyVec,
                                // outputs
                                int & minVecIndex, int & minPolyIndex,
                                double & minX, double & minY, double & minDist
                                ){

  // Find the closest edge in a given vector of polygons to a given point.
  
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

void utils::findDistanceBwPolys(// inputs
                                const dPoly & poly1,
                                const dPoly & poly2,
                                // outputs
                                std::vector<segDist> & distVec
                                ){

  // Given two sets of polygons, for each vertex in the first set of
  // polygons find the distance to the closest point (may be on edge)
  // in the second set of polygons, and the segment with the smallest
  // distance. Sort these segments in decreasing value of their
  // lengths.

  // The complexity of this algorithm is roughly
  // size(poly1)*log(size(poly2)).
  
  distVec.clear();

  const double * x1 = poly1.get_xv();
  const double * y1 = poly1.get_yv();
  int numVerts1     = poly1.get_totalNumVerts();
  int numVerts2     = poly2.get_totalNumVerts();

  if (numVerts1 == 0 || numVerts2 == 0) return; // no vertices

  // Put the edges of the second polygon in a tree for fast access
  edgeTree T;
  T.putPolyEdgesInTree(poly2);

  vector<seg> edgesInBox;
  
  for (int t = 0; t < numVerts1; t++){

    double x = x1[t], y = y1[t];
    double closestX, closestY, closestDist;
    seg closestEdge;
    T.findClosestEdgeToPoint(x, y,                                        // inputs 
                             closestEdge, closestDist, closestX, closestY // outputs
                             );
    distVec.push_back(segDist(x, y, closestX, closestY, closestDist));
    
  }

  sort(distVec.begin(), distVec.end(), segDistGreaterThan);

  return;
}

void utils::findDistanceBwPolysBruteForce(// inputs
                                          const dPoly & poly1,
                                          const dPoly & poly2,
                                          // outputs
                                          std::vector<segDist> & distVec
                                          ){

  // A naive (but simple) implementation of findDistanceBwPolys.
  
  distVec.clear();

  const double * x = poly1.get_xv();
  const double * y = poly1.get_yv();
  int numVerts1    = poly1.get_totalNumVerts();
  int numVerts2    = poly2.get_totalNumVerts();
  
  if (numVerts1 == 0 || numVerts2 == 0) return; // no vertices

  for (int t  = 0; t < numVerts1; t++){
    
    int minPolyIndex;
    double minX, minY, minDist = DBL_MAX;
    poly2.findClosestPolyEdge(x[t], y[t],                        // inputs
                              minPolyIndex, minX, minY,  minDist // outputs
                              );

    
    if (minDist != DBL_MAX)
      distVec.push_back(segDist(x[t], y[t], minX, minY, minDist));
  }

  sort(distVec.begin(), distVec.end(), segDistGreaterThan);
  
  return;
}

void utils::putPolyInMultiSet(const dPoly & P, std::multiset<dPoint> & mP){

  const double * x  = P.get_xv();
  const double * y  = P.get_yv();
  int totalNumVerts = P.get_totalNumVerts();

  mP.clear();
  for (int v = 0; v < totalNumVerts; v++){
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
  // may have collinear points. If one polygon has a point repeated
  // twice, but the second polygon has it repeated just once, this
  // will be flagged as a difference as well.

  // This utility will not be able to detect when two polygons are
  // different but contain exactly the same points.
  
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

