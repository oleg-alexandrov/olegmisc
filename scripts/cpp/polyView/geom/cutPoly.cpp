#include "cutPoly.h"
#include <cassert>
#include <algorithm>
#include <fstream>
#include <cfloat>
using namespace std;
using namespace utils;

#define DEBUG_CUT_POLY 0 // Must be 0 in production code

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

  // Cut a given polygon with a box.

  // Intersect the polygon with each of the the half-planes
  // nx*x + ny*y <= (nx + ny)*H.
  // There are four values for the triplet (nx, ny, H):
  double cutParams[] = {
   -1,  0, xll, //  -- left cut
    1,  0, xur, //  -- right cut
    0, -1, yll, //  -- bottom cut
    0,  1, yur  //  -- top cut
  };

  int totalNumVerts = 0;
  for (int s = 0; s < numPolys; s++) totalNumVerts += numVerts[s];

  vector<double> Xin(xv, xv + totalNumVerts);        // A copy of xv as vector
  vector<double> Yin(yv, yv + totalNumVerts);        // A copy of yv as vector
  vector<int>    Pin(numVerts, numVerts + numPolys); // A copy of numVerts 
  
  vector<double> cutHalfX, cutHalfY, Xout, Yout;
  vector<int>    cutHalfP, Pout;

  for (int c = 0; c < 4; c++){

    Pout.clear(); Xout.clear(); Yout.clear();
    
    double nx   = cutParams[3*c + 0];
    double ny   = cutParams[3*c + 1];
    double H    = cutParams[3*c + 2];
    double dotH = (nx + ny)*H; // This formula works only for nx*ny == 0.

    int start = 0;
    for (int pIter = 0; pIter < (int)Pin.size(); pIter++){
      
      if (pIter > 0) start += Pin[pIter - 1];
      
      int numV = Pin[pIter];
      if (numV == 0) continue;
      
      cutToHalfSpace(nx, ny, dotH,
                     numV, vecPtr(Xin) + start, vecPtr(Yin) + start,
                     cutHalfX, cutHalfY, cutHalfP);
      
      for (int pIterCut = 0; pIterCut < (int)cutHalfP.size(); pIterCut++){
        Pout.push_back( cutHalfP[pIterCut] );
      }
      
      for (int vIter = 0; vIter < (int)cutHalfX.size(); vIter++){
        Xout.push_back( cutHalfX[vIter] );
        Yout.push_back( cutHalfY[vIter] );
      }
      
    }
    
    Pin = Pout; Xin = Xout; Yin = Yout;
    
  } // End iterating over cutting lines

  cutNumPolys = Pout; cutX = Xout; cutY = Yout;
  
  return;
}

void utils::cutToHalfSpace(// inputs 
                           double nx, double ny, double dotH,
                           int numV, 
                           const double * xv, const double * yv,
                           // outputs -- the cut polygons
                           std::vector<double> & cutX,
                           std::vector<double> & cutY,
                           std::vector<int>    & cutNumPolys){


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
    double cutx = 0.0, cuty = 0.0;
    
    if (dotCurr < dotH){

      // The current point is inside the half-plane
      
      cutX.push_back(xcurr);
      cutY.push_back(ycurr);
      cutPtsIndex++; 
      
      if (dotNext <= dotH) continue;
      
      cutEdge(xcurr, ycurr, xnext, ynext, nx, ny, dotH, cutx, cuty);
      cutX.push_back(cutx);
      cutY.push_back(cuty);

      C.val       = nx*cuty - ny*cutx;
      C.index     = cutPtsIndex;
      C.isOutward = true;
      ptsOnCutline.push_back(C);

      cutPtsIndex++; 
      
    }else if (dotCurr > dotH){

      // The current point is outside the half-plane
      
      if (dotNext >= dotH) continue;

      cutEdge(xcurr, ycurr, xnext, ynext, nx, ny, dotH, cutx, cuty);
      cutX.push_back(cutx);
      cutY.push_back(cuty);
      
      C.val       = nx*cuty - ny*cutx;
      C.index     = cutPtsIndex;
      C.isOutward = false;
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

        C.val       = nx*ycurr - ny*xcurr;
        C.index     = cutPtsIndex;
        C.isOutward = (dotPrev < dotH);
        ptsOnCutline.push_back(C);
        
      }

      cutPtsIndex++; 
      
    }
    
  }

  int numPtsOnCutline = ptsOnCutline.size();
  if (numPtsOnCutline == 0){
    cutNumPolys.push_back( cutX.size() );
    return;
  }

  processPointsOnCutline(ptsOnCutline);
  
  // Find the connected components in the cut polygons
  // To do: Move this to its own function.
  
  vector<double> X, Y;
  vector<int> P;
  X.clear(); Y.clear(); P.clear();

#if DEBUG_CUT_POLY
  static int c = -1;
  c++;
  char file[100];
  sprintf(file, "beforeCleanup%d.xg", c);
  cout << "\nWriting to " << file << endl;
  ofstream before(file);
  for (int s = 0; s < (int)cutX.size(); s++){
    before << cutX[s] << ' ' << cutY[s] << endl;
    before << "anno " << cutX[s] << ' ' << cutY[s]  << ' ' << s << endl;
  }
  before.close();

  for (int s = 0; s < (int)ptsOnCutline.size(); s++){
    cout.precision(20);
    cout << "point on cutline is (index outward val) "
         << ptsOnCutline[s].index     << ' '
         << ptsOnCutline[s].isOutward << ' '
         << ptsOnCutline[s].val       << endl; 
  }
#endif

  vector<int> wasVisited;
  int numCutPts = cutX.size();
  wasVisited.assign(numCutPts, 0);

  int ptIter = 0;
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
        break; // Arrived back to the starting point of the given component
      }
      
      X.push_back(cutX[ptIter]);
      Y.push_back(cutY[ptIter]);
      wasVisited[ptIter] = 1;
#if DEBUG_CUT_POLY
      cout << "ptIter = " << ptIter << endl;
#endif
      numPtsInComp++;

      // Decide which point we will visit next
      
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
      bool success = false;
      int cutlineIter = 0;
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

      if (ptsOnCutline[cutlineIter].isOutward){

        // We are getting out of the polygon. Find the point at
        // which we come back.
        ptIter = ptsOnCutline[cutlineIter].nextIndexInward;
        continue;
        
      }else{
        
        // The point ptIter is at the cutline on the way in.
        // The next point will be inside the current half-plane.
        ptIter = (ptIter + 1)%numCutPts;
        continue;
        
      }
      
    } // End iterating over all connected components

  } // End iterating over all points
  
  cutX        = X;
  cutY        = Y;
  cutNumPolys = P;

#if DEBUG_CUT_POLY
  sprintf(file, "afterCleanup%d.xg", c);
  cout << "Writing to " << file << endl;
  ofstream after(file);
  for (int s = 0; s < (int)cutX.size(); s++){
    after << cutX[s] << ' ' << cutY[s] << endl;
    after << "anno " << cutX[s] << ' ' << cutY[s]  << ' ' << s << endl;
  }
  after.close();
#endif
  
}

void utils::cutEdge(double x0, double y0, double x1, double y1,
                    double nx, double ny, double H,
                    double & cutx, double & cuty){

  // Find the point at which the line nx*x + ny*y = H intersects the
  // edge (x0, y0) --> (x1, y1).

  double dot0 = nx*x0 + ny*y0;
  double dot1 = nx*x1 + ny*y1;
  
  assert( (dot0 <= H && dot1 >= H) || (dot0 >= H && dot1 <= H ) );
  assert( dot0 != dot1 );

  // Find t such that (1-t)*(x0, y0) + t*(x1, y1) intersect the
  // cutting line
  
  double t = (H - dot0)/(dot1 - dot0);
  t = max(t, 0.0); t = min(t, 1.0); // extra precautions
  cutx = (1-t)*x0 + t*x1;
  cuty = (1-t)*y0 + t*y1;

  // Cut symmetrically in x0 and x1 to avoid problems later
  t = (H - dot1)/(dot0 - dot1);
  t = max(t, 0.0); t = min(t, 1.0); // extra precautions
  double cutx2 = (1-t)*x1 + t*x0;
  double cuty2 = (1-t)*y1 + t*y0;

  cutx = 0.5*cutx + 0.5*cutx2;
  cuty = 0.5*cuty + 0.5*cuty2;
  
  // The above formulas have floating point errors. Use the fact that
  // the cutting line is either vertical or horizontal to recover
  // precisely one of the two coordinates above.
  if (nx == 0){
    cuty = H/ny;
  }else if (ny == 0){
    cutx = H/nx;
  }
  
  return;
}

void utils::processPointsOnCutline(std::vector<valIndex> & ptsOnCutline){

  
  // Sort the cutline points along the cutline (the sort direction
  // does not matter).
  sort( ptsOnCutline.begin(), ptsOnCutline.end(), lessThan );

  // Find the position of each outward and each inward point on the cutline
  vector<int> outwardPositions, inwardPositions;
  outwardPositions.clear(); inwardPositions.clear();
  int numPtsOnCutline = ptsOnCutline.size();
  for (int s = 0; s < numPtsOnCutline; s++){
    const valIndex & C = ptsOnCutline[s];
    if (C.isOutward) outwardPositions.push_back(s);
    else             inwardPositions.push_back (s);
    //cout << "val index isOutward "
    //     << C.val << ' ' << C.index << ' ' << C.isOutward << endl; 
  }

  // There must be an even number of points on a cutline. That holds
  // true for any closed curve, with or without self-intersections.
  // Each time we cross the cutline to the other side at some point
  // there must be a corresponding point at which we come back. Match
  // the i-th outward point to the corresponding i-th inward point.
  int numOut = outwardPositions.size();
  int numIn  = inwardPositions.size();
  assert(numIn == numOut);
  for (int i = 0; i < numOut; i++){

    valIndex & C = ptsOnCutline[outwardPositions[i]]; //alias
    
    if (C.isOutward){
      C.nextIndexInward = ptsOnCutline[inwardPositions[i]].index;
#if DEBUG_CUT_POLY
      cout << "Going from " << C.index << " to " << C.nextIndexInward << endl;
#endif
    }else{
      C.nextIndexInward = -1; // To not leave it uninitialized
    }
    
  }
  
  return;
}

