#include "cutPoly.h"
#include <cassert>
#include <algorithm>
using namespace std;
using namespace utils;

namespace localPolyUtils{

  struct valIndex{
    double val;
    int index;
  };

  inline bool lessThan(valIndex A, valIndex B){
    return A.val < B.val;
  }
  
}

using namespace localPolyUtils;

void utils::cutPoly(// inputs -- the polygons
                     int numPolys, const int * numVerts,
                     const double * xv, const double * yv,
                     // inputs -- the cutting window
                     double xll, double yll,
                     double xur, double yur,
                     // outputs -- the cut polygons
                     std::vector< double> & cutX,
                     std::vector< double> & cutY,
                     std::vector< int>    & cutNumPolys){
  
  vector< double> hPolyX, hPolyY;
  vector<int> hPolyNumP;

  int start = 0;
  for (int pIter = 0; pIter < numPolys; pIter++){
    
    if (pIter > 0) start += numVerts[pIter - 1];
    
    int numV = numVerts[pIter];
    cutToHalfSpace(numV, xv + start, yv + start, xur,
                   hPolyX, hPolyY, hPolyNumP);
    
  }

  // Temporary
  cutX        = hPolyX;
  cutY        = hPolyY;
  cutNumPolys = hPolyNumP;
  
}

void utils::cutToHalfSpace(// inputs 
                           int numV, 
                           const double * xv, const double * yv,
                           double H, // cutting line -- cut to the left
                           // outputs -- the cut polygons
                           std::vector< double> & cutX,
                           std::vector< double> & cutY,
                           std::vector< int>    & cutNumPolys){

  vector<valIndex> ptsOnCutline; ptsOnCutline.clear();
  valIndex C;
  
  cutX.clear(); cutY.clear(); cutNumPolys.clear();

  int numCutPts = 0;
  
  for (int v = 0; v < numV; v++){

    int vn = (v + 1)%numV;
    
    double x0 = xv[v],  y0 = yv[v];
    double xn = xv[vn], yn = yv[vn];

    double cutx, cuty;
    
    if (x0 < H){

      cutX.push_back(x0);
      cutY.push_back(y0);
      numCutPts++; 

      if (xn <= H) continue;
      
      cutEdge(x0, y0, xn, yn, H, cutx, cuty);
      cutX.push_back(cutx);
      cutY.push_back(cuty);

      C.val = cuty; C.index = numCutPts; ptsOnCutline.push_back(C);
      numCutPts++; 
      
    }else if (x0 > H){

      if (xn >= H) continue;

      cutEdge(x0, y0, xn, yn, H, cutx, cuty);
      cutX.push_back(cutx);
      cutY.push_back(cuty);
      
      C.val = cuty; C.index = numCutPts; ptsOnCutline.push_back(C);
      numCutPts++; 

    }else if (x0 == H){

      int vp = (v == 0) ? (numV - 1) : (v - 1);
      double xp = xv[vp]; // yp = yv[vp];
      
      if (xp >= H && xn >= H) continue;
      
      cutX.push_back(x0);
      cutY.push_back(y0);
      
      C.val = y0; C.index = numCutPts; ptsOnCutline.push_back(C);
      numCutPts++; 
      
    }
    
  }

  // Find the connected components in the cut polygons
  vector<double> X, Y;
  vector<int> P;
  X.clear(); Y.clear(); P.clear();
  
  sort( ptsOnCutline.begin(), ptsOnCutline.end(), lessThan );
  cout << "----" << endl;
  for (int s = 0; s < (int)ptsOnCutline.size(); s++){
    cout << "point is "
         << ptsOnCutline[s].index << ' ' << ptsOnCutline[s].val << endl; 
  }

  vector<int> wasVisited;
  wasVisited.assign(numCutPts, 0);

  int start = 54, ptIter = 0, numPtsOnCutline = ptsOnCutline.size();
  assert(numPtsOnCutline%2 == 0);
  
  while(1){

    // Stop when all points are visited
    if (!wasVisited[start]){
      ptIter = start;
    }else{

      bool success = false;
      for (int v = 0; v < numCutPts; v++){
        if (!wasVisited[v]){
          ptIter  = v;
          success = true;
          break;
        }
      }

      if (!success) break; 
      
    }

    int numPtsInComp = 0;
    
    // Visit a given connected component
    while(1){
      
      if (wasVisited[ptIter]){
        P.push_back(numPtsInComp);
        break; // Arrived back to the starting point
      }
      
      X.push_back(cutX[ptIter]);
      Y.push_back(cutY[ptIter]);
      wasVisited[ptIter] = 1;
      numPtsInComp++;
      cout << "pt " << ptIter << endl;
      
      if (cutX[ptIter] != H){
        // The point is not at the cutline
        ptIter = (ptIter + 1)%numCutPts;
        continue;
      }

      // The point is at the cutline. Find where exactly it is
      // in the sorted cutline points.
      int cutlineIter = 0;
      bool success = false;
      for (cutlineIter = 0; cutlineIter < numPtsOnCutline; cutlineIter++){
        if (ptsOnCutline[cutlineIter].index == ptIter){
          success = true;
          break;
        }
      }
      assert(success);

      if (cutlineIter%2 == 0){
        // The point ptIter is at the cutline on the way out.
        // Find the point where it re-enters the current half-plane.
        assert(cutlineIter < numPtsOnCutline - 1);
        ptIter = ptsOnCutline[cutlineIter + 1].index;
        continue;
        
      }else{
        // The pont ptIter is at the cutline on the way in.
        // The next point will be inside the current half-plane.
        ptIter = (ptIter + 1)%numCutPts;
        continue;
      }
      
    } // End iterating over all connected components

  } // End iterating over all points
  
  cutX = X;
  cutY = Y;
  cutNumPolys = P;
  
}

void utils::cutEdge(double x0, double y0, double x1, double y1, double H,
                    double & cutx, double & cuty){
  
  assert( (x0 <= H && x1 >= H) || (x0 >= H && x1 <= H) && (x0 != x1) );

  // Find t such that
  // (1-t)*(x0, y0) + t*(x1, y1) intersect the line x = H
  
  double t = (H - x0)/(x1 - x0);
  t = max(t, 0.0); t = min(t, 1.0); // extra precautions
  
  cutx = H;
  cuty = (1-t)*y0 + t*y1;

  return;
}
