#include <cmath>
#include <cstdlib>
#include <iostream>
#include <cmath>
#include <cfloat>
#include "geomUtils.h"
using namespace std;
using namespace utils;
void utils::snapPolyLineTo45DegAngles(bool isClosedPolyLine,
                                      int numVerts, double * xv, double * yv){

  // Given a polygonal line, transform it such that all edges
  // in it make an angle multiple of 45 degrees with the x axis.

  if (numVerts <= 0) return;
  
  // The vectors corresponding to angles multiple of 45 degree
  int numAngles = 8;
  double xs[numAngles], ys[numAngles];
  for (int a = 0; a < numAngles; a++){
    double theta = a*45*M_PI/180;
    xs[a] = cos(theta);
    ys[a] = sin(theta);
  }

  double factor = 2;
  xv[0] = factor*round(xv[0]/factor);
  yv[0] = factor*round(yv[0]/factor);

  for (int v = 0; v < numVerts - 1; v++){

    bool snap2ndClosest = false; // snap to closest, not second closest
    snapOneEdgeTo45(numAngles, xs, ys, snap2ndClosest,  // inputs
                    xv[v], yv[v], xv[v + 1], yv[v + 1]  // in-out
                    );
    
  }

  if (!isClosedPolyLine || numVerts < 3) return;

  // The poly line is closed. The vertex after the n-th vertex is the 0-th one.

  for (int attempt = 0; attempt < 2; attempt++){
    
    double x0 = xv[0],            y0 = yv[0];
    double x1 = xv[numVerts - 2], y1 = yv[numVerts - 2];
    double x2 = xv[numVerts - 1], y2 = yv[numVerts - 1];
    
    bool snap2ndClosest = (attempt != 0); 
    snapOneEdgeTo45(numAngles, xs, ys, snap2ndClosest,          // inputs
                    x0, y0, xv[numVerts - 1], yv[numVerts - 1]  // in-out
                    );

    double x3 = xv[numVerts - 1], y3 = yv[numVerts - 1];
    
    // Find the intersection of the lines
    // (x0, y0) --> (x3, y3) and
    // (x1, y1) --> (x2, y2).
    
    double det = ( (x3-x0)*(y2-y1) - (y3-y0)*(x2-x1) );
    double top = ( (x1-x0)*(y2-y1) - (y1-y0)*(x2-x1) );
    bool success = (det != 0 || top == 0);
    if (det != 0){
      double t = top/det;
      xv[numVerts - 1] = round( t*(x3-x0) + x0 );
      yv[numVerts - 1] = round( t*(y3-y0) + y0 );
    }else{
      xv[numVerts - 1] = x2;
      yv[numVerts - 1] = y2;
    }

    if (success) break;
    
  }
  
  // Validate
  for (int v = 0; v < numVerts; v++){
    double dx = xv[(v+1)%numVerts] - xv[v];
    double dy = yv[(v+1)%numVerts] - yv[v];
    if ( !( dx == 0 || dy == 0 || abs(dx) == abs(dy) ) ){
      cerr << "Error: Expecting vectors with angles of 45 degrees."  << endl;
      cerr << "Instead, got the vector (" << dx << ", " << dy << ")" << endl;
      //assert(false);
    }
  }
  
  return;
}

void utils::snapOneEdgeTo45(int numAngles, double* xs, double* ys,
                            bool snap2ndClosest, 
                            double & x0, double & y0,
                            double & x1, double & y1){

  // cout << "in:  " << x0 << ' ' << y0 << ' ' << x1 << ' ' << y1 << endl;
  
  double dx = x1 - x0, dy = y1 - y0;
  double len = distance(0, 0, dx, dy);
  if (len == 0.0) return;

  dx /= len;
  dy /= len;

  // Find the closest angle multiple of 45 degrees from (dx, dy)
  int minAngle   = 0;
  double minDist = DBL_MAX;
  for (int a = 0; a < numAngles; a++){
    double dist = distance(dx, dy, xs[a], ys[a]);
    if (dist <= minDist){
      minDist  = dist;
      minAngle = a;
    }
  }

  // We prefer to snap to the second closest angle if for some reason
  // we know that snapping to the closest angle does not work.
  if (snap2ndClosest){
    int minAngle2   = 0;
    double minDist2 = DBL_MAX;

    for (int a = 0; a < numAngles; a++){
      double dist = distance(dx, dy, xs[a], ys[a]);
      if (dist <= minDist2 && a != minAngle){
        minDist2  = dist;
        minAngle2 = a;
      }
    }
    minAngle = minAngle2;
  }
  
  // Snap. This is a bit hard to explain.
  double factor = 2 * ( abs(xs[minAngle]) + abs(ys[minAngle]) );
  len = factor*round(len/factor);
  x1 = x0 + round( len*xs[minAngle] );
  y1 = y0 + round( len*ys[minAngle] );

  // cout << "out: " << x0 << ' ' << y0 << ' ' << x1 << ' ' << y1 << endl;
  
  return;
}
