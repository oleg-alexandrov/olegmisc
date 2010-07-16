#include "cutPoly.h"
#include <cassert>
#include <algorithm>
#include <fstream>
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
    

    for (int pIter = 0; pIter < (int)hPolyNumP.size(); pIter++){
      cutNumPolys.push_back( hPolyNumP[pIter] );
    }
    
    for (int vIter = 0; vIter < (int)hPolyX.size(); vIter++){
      cutX.push_back( hPolyX[vIter] );
      cutY.push_back( hPolyY[vIter] );
    }
    
  }
  return;
}

void utils::cutToHalfSpace(// inputs 
                           int numV, 
                           const double * xv, const double * yv,
                           double H, // cutting line -- cut to the left
                           // outputs -- the cut polygons
                           std::vector< double> & cutX,
                           std::vector< double> & cutY,
                           std::vector< int>    & cutNumPolys){

  // Intersect the polygon with the half-space
  // nx*x + ny*y <= dotH.
  // There are four values for the triplet (nx, ny, H):
  // (-1,  0, xll) -- left cut
  // ( 1,  0, xur) -- right cut
  // ( 0, -1, yll) -- bottom cut
  // ( 0,  1, yur) -- top cut
  
  double nx = 1.0, ny = 0.0;
  double dotH = (nx + ny)*H; // This formula works only for nx*ny == 0.
  
  vector<valIndex> ptsOnCutline; ptsOnCutline.clear();
  valIndex C;
  
  cutX.clear(); cutY.clear(); cutNumPolys.clear();

  int cutPtsIndex = 0;
  
  for (int v = 0; v < numV; v++){

    int vnext = (v + 1)%numV;
    
    double xcurr = xv[v],     ycurr = yv[v];
    double xnext = xv[vnext], ynext = yv[vnext];

    double dotCurr = nx*xcurr + ny*ycurr;
    double dotNext = nx*xnext + ny*ynext;
    double cutx, cuty;
    
    if (dotCurr < dotH){

      // The current point is inside the half-plane
      
      cutX.push_back(xcurr);
      cutY.push_back(ycurr);
      cutPtsIndex++; 
      
      if (dotNext <= dotH) continue;
      
      cutEdge(xcurr, ycurr, xnext, ynext, dotH, cutx, cuty);
      cutX.push_back(cutx);
      cutY.push_back(cuty);

      C.val = cuty; C.index = cutPtsIndex;
      ptsOnCutline.push_back(C);

      cutPtsIndex++; 
      
    }else if (dotCurr > dotH){

      // The current point is outside the half-plane
      
      if (dotNext >= dotH) continue;

      cutEdge(xcurr, ycurr, xnext, ynext, dotH, cutx, cuty);
      cutX.push_back(cutx);
      cutY.push_back(cuty);
      
      C.val = cuty; C.index = cutPtsIndex;
      ptsOnCutline.push_back(C);

      cutPtsIndex++; 

    }else if (dotCurr == dotH){

      // The current point is at the edge of the half-plane

      int    vprev   = (v == 0) ? (numV - 1) : (v - 1);
      double xprev   = xv[vprev], yprev = yv[vprev];
      double dotPrev = nx*xprev + ny*yprev;
      
      if (dotPrev >= dotH && dotNext >= dotH) continue;
      
      cutX.push_back(xcurr);
      cutY.push_back(ycurr);

      if (dotPrev >= dotH || dotNext >= dotH){
        C.val = ycurr; C.index = cutPtsIndex;
        ptsOnCutline.push_back(C);
      }

      cutPtsIndex++; 
      
    }
    
  }

  
  // Find the connected components in the cut polygons
  // To do: Move this to its own function.
  
  vector<double> X, Y;
  vector<int> P;
  X.clear(); Y.clear(); P.clear();
  
  sort( ptsOnCutline.begin(), ptsOnCutline.end(), lessThan );

#if 0
  char * file = "cut.xg";
  cout << "Writing to " << file << endl;
  ofstream ch(file);
  for (int s = 0; s < (int)cutX.size(); s++){
    ch << cutX[s] << ' ' << cutY[s] << endl;
    ch << "anno " << cutX[s] << ' ' << cutY[s]  << ' ' << s << endl;
  }
  ch.close();
  
  for (int s = 0; s < (int)ptsOnCutline.size(); s++){
    cout << "point on cutline is "
         << ptsOnCutline[s].index << ' ' << ptsOnCutline[s].val << endl; 
  }
#endif

  vector<int> wasVisited;
  int numCutPts = cutX.size();
  wasVisited.assign(numCutPts, 0);

  int ptIter = 0, numPtsOnCutline = ptsOnCutline.size();

  // There must be an even number of points on a cutline.  For each
  // point at which we cross the cut line to the other side there must
  // be a corresponding point at which we come back.
  assert(numPtsOnCutline%2 == 0);
  
  while(1){

    // Stop when all points are visited
    bool success = false;
    for (int v = 0; v < numCutPts; v++){
      if (!wasVisited[v]){
        ptIter  = v;
        success = true;
        break;
      }
    }

    if (!success) break; 

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
      
      if (nx*cutX[ptIter] + ny*cutY[ptIter] != dotH){
        // The point is not at the cutline
        ptIter = (ptIter + 1)%numCutPts;
        continue;
      }

      // The point is at the cutline. Find where exactly it is in the
      // sorted cutline points. If it is not among those sorted
      // cutline points, it means that the polygon only touches the
      // cutline at that point rather than crossing over to the other
      // side.
      // To do: Use here some faster lookup, such as a map.
      int cutlineIter = 0;
      bool success = false;
      for (cutlineIter = 0; cutlineIter < numPtsOnCutline; cutlineIter++){
        if (ptsOnCutline[cutlineIter].index == ptIter){
          success = true;
          break;
        }
      }
      if (!success){
        ptIter = (ptIter + 1)%numCutPts;
        continue;
      }

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
  
  cutX        = X;
  cutY        = Y;
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
